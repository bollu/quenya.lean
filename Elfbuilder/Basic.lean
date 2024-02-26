import Lean
import Elfbuilder.Raw
open Lean IO FS

inductive ExprKind
| register
| addr
| syscall

inductive Expr : (kind : ExprKind) → Type
| rdi : Expr .register
| rax : Expr .register
| extern (name : String) : Expr .addr
| data (data : ByteArray) : Expr .addr
| syscallExit : Expr .syscall

-- X86 instruction set.
inductive Instruction
| Label (name : String) -- create a label like _start at this location.
| MovAddr (r : Expr .register) (addr : Expr .addr)
| Call (fn : Expr .addr)
| Xor (r : Expr .register) (s : Expr .register)
| Syscall (opcode : Expr .syscall)

-- Sections

structure Section where
  offset : Nat := 0
  size : Nat := 0

def Section.toElf64_Shdr (s : Section) : Elf64_Shdr := {
   sh_offset := s.offset,
   sh_size := s.size
 }

structure TextSection extends Section where
  instructions : ByteArray := ByteArray.empty

def TextSection.tell (s : TextSection) : Nat := s.instructions.size
def TextSection.append (s : TextSection) (instr : ByteArray) : TextSection :=
  { s with instructions := s.instructions ++ instr }

structure DataSection extends Section where
  array : ByteArray := ByteArray.empty

-- +1 for each null terminator.
def DataSection.tell (s : DataSection) : Nat := s.array.size
def DataSection.append (s : DataSection) (data : ByteArray) : DataSection :=
  { s with array := s.array ++ data }

structure RelaTextSection extends Section where
  array : Array Elf64_Rela := #[]

def RelaTextSection.tell (s : RelaTextSection) : Nat := s.array.size
def RelaTextSection.append (s : RelaTextSection) (rela : Elf64_Rela) : RelaTextSection :=
  { s with array := s.array.push rela }

structure SymtabSection extends Section where
  array : Array Elf64_Sym := #[]

-- +1 for each null terminator.
def SymtabSection.tell (s : SymtabSection) : Nat := s.array.size
def SymtabSection.append (s : SymtabSection) (sym : Elf64_Sym) : SymtabSection :=
  { s with array := s.array.push sym }

structure StrtabSection extends Section where
  array : ByteArray := ByteArray.empty

-- +1 for each null terminator.
def StrtabSection.tell (s : StrtabSection) : Nat := s.array.size
def StrtabSection.append (s : StrtabSection) (str : String) : StrtabSection :=
  { s with array := s.array ++ str.toUTF8.push 0x00.toUInt8 }

inductive RealValueKind
| uint64
| uint32

@[reducible, simp]
def RealValueKind.data : RealValueKind → Type
| uint64 => UInt64
| uint32 => UInt32

structure RealValue where
  kind : RealValueKind
  value : RealValueKind.data kind

def RealValue.uint64 (v : UInt64) : RealValue := { kind := RealValueKind.uint64, value := v }
def RealValue.uint32 (v : UInt32) : RealValue := { kind := RealValueKind.uint32, value := v }

def RealValue.toUInt64 (v : RealValue) : UInt64 :=
  match v with
  | { kind := RealValueKind.uint64, value := v } => v
  | { kind := RealValueKind.uint32, value := v } => v.toUInt64

def RealValue.toUInt32 (v : RealValue) : UInt32 :=
  match v with
  | { kind := RealValueKind.uint64, value := v } => v.toUInt32
  | { kind := RealValueKind.uint32, value := v } => v

def RealValue.toInt (v : RealValue) : Nat :=
  match v with
  | { kind := RealValueKind.uint64, value := v } => v.toNat
  | { kind := RealValueKind.uint32, value := v } => v.toNat

def RealValue.toKind (v : RealValue) (kind : RealValueKind) : kind.data :=
  match kind with
  | RealValueKind.uint64 => v.toUInt64
  | RealValueKind.uint32 => v.toUInt32

instance {kind : RealValueKind} : ElfWriteable kind.data where
  write v :=
    match kind with
    | .uint64 => ElfWriteable.write v
    | .uint32 => ElfWriteable.write v


def nullSectionHeaderIndex : Nat := 1 -- CAREFUL, synchronized with `emitSectionHeaders`
def textSectionHeaderIndex : Nat := 1 -- CAREFUL, synchronized with `emitSectionHeaders`
def dataSectionHeaderIndex : Nat := 2 -- CAREFUL, synchronized with `emitSectionHeaders`
def strtabSectionHeaderIndex : Nat := 3 -- CAREFUL, synchronized with `emitSectionHeaders`
def symtabSectionHeaderIndex : Nat := 4 -- CAREFUL, synchronized with `emitSectionHeaders`
def relaSectionHeaderIndex : Nat := 4 -- CAREFUL, synchronized with `emitSectionHeaders`

def Elf64_Ehdr.default : Elf64_Ehdr  :=
  {
    e_ident := #[0x7f, 0x45, 0x4c, 0x46, ELFCLASS64, ELFDATA2LSB, EV_CURRENT.toUInt8, 0, 0, 0, 0, 0, 0, 0]
    e_type := ET_REL
    e_machine := EM_X86_64
    e_version := EV_CURRENT
    e_ehsize := 64 -- sizeof (Elf64_Ehdr)
    e_shentsize := 64 -- sizeof (Elf64_Shdr)
    e_shnum := 6 --  [null, text, data, strtab, symtab, rela.text]
    e_shstrndx := strtabSectionHeaderIndex -- rela index
    e_phoff := 0
    e_shoff := 0
    e_entry := 0
    e_flags := 0
    e_phentsize := 0
    e_phnum := 0
  }


/--
  State of the ELF Builder.
  The text section has the executable code used at runtime
  The data section has the executable data used at runtime (`static`, `#embed`, `global`...)
  Recall that:
    the relocation table points to symbols
    ...into the symbol table, which points to names
    ...from the string table [data needed at link/load time].
-/
structure ElfBuilderState  where
  header : Elf64_Ehdr := Elf64_Ehdr.default
  -- sections
  textSection : TextSection := {}
  dataSection : DataSection := {}
  relaTextSection : RelaTextSection := {}
  symtabSection : SymtabSection := {}
  strtabSection : StrtabSection := {}

/-- A two pass algorithm to write out what has been built by ELFBuilder -/
abbrev ElfBuilderM := StateT ElfBuilderState ElfWriterM

namespace ElfBuilderM

/-- Emit a string into the string table -/
def emitStrtabString (str : String) : ElfBuilderM Nat := do
  let out := (← get).strtabSection.tell
  modify fun s => { s with strtabSection := s.strtabSection.append str }
  return out

def emitDataString (str : String) : ElfBuilderM Nat := do
  let out := (← get).dataSection.tell
  modify fun s => { s with dataSection := s.dataSection.append <| str.toUTF8.push 0x00.toUInt8 }
  return out

def emitDataByteArray (data : ByteArray) : ElfBuilderM Nat := do
  let out := (← get).dataSection.tell
  modify fun s => { s with dataSection := s.dataSection.append data }
  return out

def emitRela (rela : Elf64_Rela) : ElfBuilderM Nat := do
  let out := (← get).relaTextSection.tell
  modify fun s => { s with relaTextSection := s.relaTextSection.append rela }
  return out

def emitSymtab (sym : Elf64_Sym) : ElfBuilderM Nat := do
  let out := (← get).symtabSection.tell
  modify fun s => { s with symtabSection := s.symtabSection.append sym }
  return out

def emitTextByteArray (data : ByteArray) : ElfBuilderM Nat := do
  let out := (← get).textSection.tell
  modify fun s => { s with textSection := s.textSection.append data }
  return out


def emitExpr : (kind : ExprKind) → Expr kind → ElfBuilderM Unit
| .register, Expr.rdi => return ()
| .register, Expr.rax => return ()
| .addr, Expr.extern name => do
  let symIx ← emitSymtab { st_name := (← emitStrtabString name), st_info := 0, st_other := 0, st_shndx := SHN_UNDEF, st_value := 0, st_size := 0 }
  let relaIx ← emitRela <| { r_offset := (← get).textSection.tell, r_addend := 0 : Elf64_Rela }.setSymbolAndType symIx R_X86_64_PC32
| .addr, Expr.data data => do
  let dataIx ← emitDataByteArray data
  let symIx ← emitSymtab { st_name := 0, st_info := 0, st_other := 0, st_shndx := dataSectionHeaderIndex, st_value := dataIx, st_size := 0 }
  let relaIx ← emitRela <| { r_offset := (← get).textSection.tell, r_info := 0, r_addend := 0 : Elf64_Rela }.setSymbolAndType dataIx R_X86_64_64
| .syscall, Expr.syscallExit => do
    let _ ← emitTextByteArray <| ByteArray.mk #[0x0f, 0x05]


def emitInstruction : Instruction → ElfBuilderM Unit
| Instruction.Label name => do
  let offset := (← get).textSection.tell
  let ix ← emitStrtabString name
  let symIx ← emitSymtab <|
    { st_name := ix, st_other := 0, st_shndx := textSectionHeaderIndex , st_value := offset, st_size := 0 : Elf64_Sym}.setBindingAndType STB_GLOBAL STT_FUNC
| Instruction.MovAddr r addr => do
  let _ ← emitExpr ExprKind.addr addr
  let _ ← emitExpr ExprKind.register r
  let _ ← emitTextByteArray <| ByteArray.mk #[0x48, 0xc7, 0xc0, 0x00, 0x00, 0x00, 0x00]
| Instruction.Call addr => do
  let _ ← emitExpr ExprKind.addr addr
  let _ ← emitTextByteArray <| ByteArray.mk #[0xe8, 0x00, 0x00, 0x00, 0x00]
| Instruction.Xor r s => do
  let _ ← emitExpr ExprKind.register r
  let _ ← emitExpr ExprKind.register s
  let _ ← emitTextByteArray <| ByteArray.mk #[0x48, 0x31, 0xc0]
| Instruction.Syscall opcode => do
  let _ ← emitExpr ExprKind.syscall opcode

-- 'E' in hex is: 0x45
-- 'L' in hex is: 0x4c
-- 'F' in hex is: 0x46
def emitHeaderAndSectionHeadersPlaceholder  : ElfBuilderM Unit := do
  let header := (← get).header
  ElfWriterM.write header
  let sh_offset ← ElfWriterM.tell
  modify fun s => { s with header.e_shoff := sh_offset.toUInt64 }
  ElfWriterM.seek (header.e_ehsize + header.e_shentsize * header.e_shnum).toNat

/-- Write Sections[1]: Text -/
def emitTextSection : ElfBuilderM Unit := do
  let section_offset ← ElfWriterM.tell
  ElfWriterM.write (← get).textSection.instructions
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with textSection := { offset := section_offset, size := section_size } }

/-- Write Sections[2]: Data -/
def emitDataSection : ElfBuilderM Unit := do
  let section_offset ← ElfWriterM.tell
  ElfWriterM.write (← get).dataSection.array
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with dataSection := { offset := section_offset, size := section_size } }

/-- Write Sections[3] : Rela -/
def emitRelaTextSection : ElfBuilderM Unit := do
  let section_offset ← ElfWriterM.tell
  for rela in (← get).relaTextSection.array do
    ElfWriterM.write rela
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with relaTextSection := { offset := section_offset, size := section_size } }

/-- Write Sections[4] : symtab -/
def emitSymtabSection : ElfBuilderM Unit := do
  let section_offset ← ElfWriterM.tell
  for sym in (← get).symtabSection.array do
    ElfWriterM.write sym
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with
    relaTextSection := {
      offset := section_offset,
      size := section_size
    }
  }

/-- Write Sections[5]: Strtab -/
def emitStrtabSection : ElfBuilderM Unit := do
  let section_offset ← ElfWriterM.tell
  ElfWriterM.write (← get).strtabSection.array
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with dataSection := { offset := section_offset, size := section_size } }

def emitSectionHeaders : ElfBuilderM Unit := do
  ElfWriterM.seek (← get).header.e_shoff.toNat
  -- null[0]
  let nullHeader : Elf64_Shdr := {}
  ElfWriterM.write nullHeader
  -- text[1]
  let textHeader : Elf64_Shdr := {
    (← get).textSection.toElf64_Shdr with
    sh_type := SHT_PROGBITS,
    sh_flags := SHF_ALLOC ||| SHF_EXECINSTR,
    sh_addralign := 16
  }
  ElfWriterM.write textHeader
  -- data[2]
  let dataHeader : Elf64_Shdr := {
    (← get).dataSection.toElf64_Shdr with
    sh_type := SHT_PROGBITS,
    sh_flags := SHF_ALLOC,
    sh_addralign := 1
  }
  ElfWriterM.write dataHeader
  -- strtab[3]
  let strtabHeader : Elf64_Shdr := {
    (← get).strtabSection.toElf64_Shdr with
    sh_type := SHT_STRTAB,
    sh_addralign := 1
  }
  ElfWriterM.write strtabHeader
  -- symtab[4]
  let symtabHeader : Elf64_Shdr := {
    (← get).symtabSection.toElf64_Shdr with
    sh_type := SHT_SYMTAB,
    sh_link := strtabSectionHeaderIndex,
    sh_info := (← get).symtabSection.array.foldl (init := 0) fun n sym => if sym.st_shndx == SHN_UNDEF then n + 1 else n,
    sh_addralign := 8,
    sh_entsize := 24 -- sizeof(Elf64_Sym)
  }
  ElfWriterM.write symtabHeader
  -- rela[5]
  let relaHeader : Elf64_Shdr := {
    (← get).relaTextSection.toElf64_Shdr with
    sh_type := SHT_RELA,
    sh_flags := SHF_INFO_LINK,
    sh_link := symtabSectionHeaderIndex,
    sh_info := textSectionHeaderIndex,
    sh_addralign := 8,
    sh_entsize := 24 -- sizeof(Elf64_Rela)
  }
  ElfWriterM.write relaHeader


namespace Internal
def linkerScript : ElfBuilderM Unit := do
  emitHeaderAndSectionHeadersPlaceholder
  emitTextSection
  emitDataSection -- strings and data used during runtime.
  emitRelaTextSection
  emitSymtabSection
  emitStrtabSection -- strings used during link time.
  emitSectionHeaders
end Internal

def runAndGetState (builder : ElfBuilderM Unit) : ElfWriterM ElfBuilderState := do
  let (_, state) ← builder.run {}
  return state

def runAndGetWriterM (builder : ElfBuilderM Unit) : ElfWriterM Unit := do
  StateT.run' Internal.linkerScript (← builder.runAndGetState)

def runAndGetByteArray (builder : ElfBuilderM Unit) : IO ByteArray := do
  (runAndGetWriterM builder).writeToBuffer

def runAndWriteToFileHandle (builder : ElfBuilderM Unit) (handle : IO.FS.Handle) : IO Unit := do
  (runAndGetWriterM builder).writeToFileHandle handle

def runAndWriteToFilePath (builder : ElfBuilderM Unit) (path : System.FilePath) : IO Unit := do
  (runAndGetWriterM builder).writeToFilePath path
end ElfBuilderM
