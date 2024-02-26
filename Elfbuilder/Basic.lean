import Lean
import Elfbuilder.Raw
open Lean IO FS

inductive ExprKind
| register
| addr
| imm32

inductive Expr : (kind : ExprKind) → Type
| eax : Expr .register
| rax : Expr .register
| rcx : Expr .register
| rdx : Expr .register
| rbx : Expr .register
| rsp : Expr .register
| rbp : Expr .register
| rsi : Expr .register
| rdi : Expr .register
| extern (name : String) : Expr .addr
| data (data : ByteArray) : Expr .addr
| uint32 (val : UInt32) : Expr .imm32

-- X86 instruction set.
inductive Instruction
| Label (name : String) -- create a label like _start at this location.
| Xor (r : Expr .register) (s : Expr .register)
| MovImm32R (imm : Expr .imm32) (r : Expr .register)
| Syscall

-- Sections

structure Section where
  offset : Nat := 0
  size : Nat := 0
  nameOffset : Nat := 0 -- offset in strtab for the name of the section

def Section.toElf64_Shdr (s : Section) : Elf64_Shdr := {
   sh_name := s.nameOffset,
   sh_offset := s.offset,
   sh_size := s.size,
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
  -- ELF strings are 1-indexed, so we start with a junk string at index 1. "JUNK" is 4 bytes long, so we can use it as a placeholder for the first 4 bytes of the string table.
  array : ByteArray :=  ("".toUTF8.push 0x00.toUInt8)

-- +1 for each null terminator.
def StrtabSection.tell (s : StrtabSection) : Nat := s.array.size
def StrtabSection.append (s : StrtabSection) (str : String) : StrtabSection :=
  { s with array := s.array ++ (str.toUTF8.push 0x00.toUInt8) }

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
    e_ident := #[0x7f, 0x45, 0x4c, 0x46, -- 4
      ELFCLASS64, ELFDATA2LSB, EV_CURRENT.toUInt8, -- 7
      0, 0, 0, -- 10
      0, 0, 0, 0, 0, 0] -- 16
    e_type := ET_REL
    e_machine := EM_X86_64
    e_version := EV_CURRENT
    e_ehsize := Sizeof.sizeof ``Elf64_Ehdr -- sizeof (Elf64_Ehdr)
    e_shentsize := Sizeof.sizeof ``Elf64_Shdr -- sizeof (Elf64_Shdr)
    e_shnum := 6 --  [null, text, data, strtab, symtab, rela.text]
    e_shstrndx := strtabSectionHeaderIndex -- strtab section index
    e_phoff := 0
    e_shoff := Sizeof.sizeof ``Elf64_Shdr
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

def emitTextUInt64 (i : UInt64) : ElfBuilderM Nat := do
  let arr := #[]
  let arr := arr.push <| ((i >>> 0) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 8) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 16) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 24) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 32) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 40) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 48) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 56) &&& (0xFF : UInt32)).toUInt8
  emitTextByteArray <| ByteArray.mk arr

def emitTextUInt32 (i : UInt32) : ElfBuilderM Nat := do
  let arr := #[]
  let arr := arr.push <| ((i >>> 0) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 8) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 16) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 24) &&& (0xFF : UInt32)).toUInt8
  emitTextByteArray <| ByteArray.mk arr

def emitTextUInt16 (i : UInt16) : ElfBuilderM Nat := do
  let arr := #[]
  let arr := arr.push <| ((i >>> 0) &&& (0xFF : UInt32)).toUInt8
  let arr := arr.push <| ((i >>> 8) &&& (0xFF : UInt32)).toUInt8
  emitTextByteArray <| ByteArray.mk arr

def emitRegister : Expr .register → UInt8
| Expr.eax => 0
| Expr.rax => 0
| Expr.rcx => 1
| Expr.rdx => 2
| Expr.rbx => 3
| Expr.rsp => 4
| Expr.rbp => 5
| Expr.rsi => 6
| Expr.rdi => 7


def emitAddr : Expr .addr -> ElfBuilderM Unit
| Expr.extern name => do
  let symIx ← emitSymtab { st_name := (← emitStrtabString name), st_info := 0, st_other := 0, st_shndx := SHN_UNDEF, st_value := 0, st_size := 0 }
  let relaIx ← emitRela <| { r_offset := (← get).textSection.tell, r_addend := 0 : Elf64_Rela }.setSymbolAndType symIx R_X86_64_PC32
| Expr.data data => do
  let dataIx ← emitDataByteArray data
  let symIx ← emitSymtab { st_name := 0, st_info := 0, st_other := 0, st_shndx := dataSectionHeaderIndex, st_value := dataIx, st_size := 0 }
  let relaIx ← emitRela <| { r_offset := (← get).textSection.tell, r_info := 0, r_addend := 0 : Elf64_Rela }.setSymbolAndType dataIx R_X86_64_64


def emitInstruction : Instruction → ElfBuilderM Unit
| Instruction.Label name => do
  let textSectionOffset := (← get).textSection.tell
  IO.println s!"Label {name} at {textSectionOffset}"
  let nameIx ← emitStrtabString name
  let symIx ← emitSymtab <|
    { st_name := nameIx,
      st_other := 0,
      st_shndx := textSectionHeaderIndex ,
      st_value := textSectionOffset,
      st_size := 0 : Elf64_Sym}.setBindingAndType STB_GLOBAL STT_FUNC
| Instruction.Xor r s => do
  let rex_prefix : UInt8 := 0x48  -- REX prefix for 64-bit operation
  let opcode : UInt8 := 0x31  -- Opcode for XOR
  let modrm : UInt8 := (0xC0 : UInt8) ||| (emitRegister r <<< 3) ||| emitRegister s  --  ModR/M byte
  let _ ← emitTextByteArray <| ByteArray.mk #[rex_prefix, opcode, modrm]
| Instruction.Syscall => do
  let _ ← emitTextByteArray <| ByteArray.mk #[0x0f, 0x05]
| Instruction.MovImm32R (.uint32 v) r => do
  let opcode_base : UInt8 := 0xB8  -- Base opcode for mov to RAX with immediate value. Others are B8+reg
  let opcode := opcode_base + emitRegister r
  let _ ← emitTextByteArray <| ByteArray.mk #[opcode]
  let _ ← emitTextUInt32 v

-- 'E' in hex is: 0x45
-- 'L' in hex is: 0x4c
-- 'F' in hex is: 0x46
def emitHeaderAndSectionHeadersPlaceholder  : ElfBuilderM Unit := do
  let header := (← get).header
  ElfWriterM.write header
  -- create strtab entries for names of all sections.
  -- text str
  let textSectionNameStrtabOffset ← emitStrtabString  ".text"
  IO.println s!"textSectionNameStrtabOffset={textSectionNameStrtabOffset}"
  -- let _ ← emitSymtab { st_name := textSectionNameStrtabOffset, st_shndx := SHN_UNDEF, st_ }
  modify fun s => { s with textSection := { s.textSection with nameOffset := textSectionNameStrtabOffset } }

  let dataSectionNameStrtabOffset ← emitStrtabString  ".data"
  modify fun s => { s with dataSection := { s.dataSection with nameOffset := dataSectionNameStrtabOffset } }

  let symtabSectionNameStrtabOffset ← emitStrtabString  ".symtab"
  modify fun s => { s with symtabSection := { s.symtabSection with nameOffset := symtabSectionNameStrtabOffset } }

  ElfWriterM.seek (header.e_ehsize + header.e_shentsize * header.e_shnum).toNat

/-- Write Sections[1]: Text -/
def emitTextSection : ElfBuilderM Unit := do
  let section_offset ← ElfWriterM.tell
  ElfWriterM.write (← get).textSection.instructions
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with textSection := { s.textSection with offset := section_offset, size := section_size } }

/-- Write Sections[2]: Data -/
def emitDataSection : ElfBuilderM Unit := do
  let section_offset ← ElfWriterM.tell
  ElfWriterM.write (← get).dataSection.array
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with dataSection := { s.dataSection with offset := section_offset, size := section_size } }

/-- Write Sections[3] : Rela -/
def emitRelaTextSection : ElfBuilderM Unit := do
  let section_offset ← ElfWriterM.tell
  for rela in (← get).relaTextSection.array do
    ElfWriterM.write rela
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with relaTextSection := { s.relaTextSection with offset := section_offset, size := section_size } }

/-- Write Sections[4] : symtab -/
def emitSymtabSection : ElfBuilderM Unit := do
  let section_offset ← ElfWriterM.tell
  for sym in (← get).symtabSection.array do
    ElfWriterM.write sym
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with symtabSection := { s.symtabSection with offset := section_offset, size := section_size }
  }

/-- Write Sections[5]: Strtab -/
def emitStrtabSection : ElfBuilderM Unit := do
  let section_offset ← ElfWriterM.tell
  ElfWriterM.write (← get).strtabSection.array
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with strtabSection := { s.strtabSection with offset := section_offset, size := section_size } }

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
    sh_flags := SHF_ALLOC ||| SHF_WRITE,
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
  IO.println s!"symtab header name offset: {(← get).symtabSection.nameOffset}"
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
  -- headers MUST be the last thing to be emitted.
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
