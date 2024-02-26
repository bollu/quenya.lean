import Lean
import Elfbuilder.Raw
open Lean IO FS

inductive Register
| rax
| rdi

structure Ptr (name : Name) where
  ix : Nat

abbrev Addr := Nat
def Addr.toUint64 (a : Addr) : UInt64 := a.toUInt64

structure VirtualAddr where
  addrIx : Nat
  symtabIx? : Option Nat := none

deriving Inhabited, Repr, BEq, Hashable

inductive VirtualAddr? where
| concrete (v : Addr)
| virtual (s : VirtualAddr)

instance : Coe Addr VirtualAddr? where coe := .concrete
instance : Coe (VirtualAddr) (VirtualAddr?) where coe := .virtual

-- X86 instruction set.
inductive Instruction
| Label (name : VirtualAddr) -- strictly speaking, not an instruction, but something that can occur in the text section nonetheless.
| MovAddr (r : Register) (a : VirtualAddr?)
| Call (fn : VirtualAddr?)
| Xor (r : Register) (s : Register)
| Syscall (opcode : UInt32)

-- Sections

structure Section where
  offset : Nat := 0
  size : Nat := 0

structure TextSection extends Section where
structure DataSection extends Section where
structure RelaTextSection extends Section where
structure SymtabSection extends Section where

structure ElfBuilderState where
  nAddrIxs : Nat := 0
  nSymtaxIxs : Nat := 0
  label2vaddr : HashMap String (VirtualAddr) := {}
  extern2vaddr : HashMap String (VirtualAddr) := {}
  str2vaddr : HashMap String (VirtualAddr) := {}
  instructions : Array Instruction := #[]

abbrev ElfBuilderM := StateT ElfBuilderState IO

namespace ElfBuilderM

/-- generate a new addr index. -/
def _genAddrIx : ElfBuilderM (Nat) := do
  modifyGet fun s => (s.nAddrIxs, { s with nAddrIxs := s.nAddrIxs + 1 })

/-- generate a new symtab ix -/
def _genSymtabIx : ElfBuilderM (Nat) := do
  modifyGet fun s => (s.nSymtaxIxs, { s with nSymtaxIxs := s.nSymtaxIxs + 1 })

/-- Allocate a new virtual address with no symbol table data . -/
def _allocateVirtualAddrNoSymtab : ElfBuilderM (VirtualAddr) := do
  pure ({ addrIx := (← _genAddrIx) : VirtualAddr })

/-- Allocate a new virtual address with no symbol table data . -/
def _allocateVirtualAddrWithSymtab : ElfBuilderM (VirtualAddr) := do
  pure ({ addrIx := (← _genAddrIx), symtabIx? := .some (← _genSymtabIx) : VirtualAddr })

/-- Create a virtual address for a named label with no symbol table entry -/
def createLabel (name : String) : ElfBuilderM (VirtualAddr) := do
  match (← get).label2vaddr.find? name with
  | some v => pure v
  | none => do
    let v ← _allocateVirtualAddrNoSymtab
    modify fun s => { s with label2vaddr := s.label2vaddr.insert name v }
    pure v

/-- Create a named (potentially) external symbol and return its address and an entry in the symbol table -/
def createExternalSymbol (name : String) : ElfBuilderM (VirtualAddr) := do
  match (← get).extern2vaddr.find? name with
  | some v => pure v
  | none => do
    let v ← _allocateVirtualAddrWithSymtab
    modify fun s => { s with extern2vaddr := s.extern2vaddr.insert name v }
    pure v

/-- Create a string and return its symbolic address and an entry in the symbol table. -/
def createString (str : String) : ElfBuilderM (VirtualAddr) := do
  match (← get).str2vaddr.find? str with
  | some v => pure v
  | none => do
    let v ← _allocateVirtualAddrWithSymtab
    modify fun s => { s with extern2vaddr := s.extern2vaddr.insert str v }
    pure v


def emitInstruction (i : Instruction) : ElfBuilderM Unit := do
  modify fun s => { s with instructions := s.instructions.push i }

def emitLabel (name : String) : ElfBuilderM Unit := do
  let v ← createLabel name
  emitInstruction (Instruction.Label v)

def emitMovAddr (r : Register) (a : VirtualAddr?) : ElfBuilderM Unit :=
  emitInstruction (Instruction.MovAddr r a)

def emitCall (fn : VirtualAddr?) : ElfBuilderM Unit :=
  emitInstruction (Instruction.Call fn)

def emitXor (r : Register) (s : Register) : ElfBuilderM Unit :=
  emitInstruction (Instruction.Xor r s)

def emitSyscall (opcode : UInt32) : ElfBuilderM Unit :=
  emitInstruction (Instruction.Syscall opcode)

def runAndGetElfBuilderState (builder : ElfBuilderM Unit) : IO ElfBuilderState := do
  let ((), state) ← builder.run {}
  return state


end ElfBuilderM

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


def Elf64_Ehdr.default : Elf64_Ehdr  := {
  e_ident := #[0x7f, 0x45, 0x4c, 0x46, ELFCLASS64, ELFDATA2LSB, EV_CURRENT.toUInt8]
  e_type := ET_REL
  e_machine := EM_X86_64
  e_version := EV_CURRENT
  e_ehsize := 64 -- sizeof (Elf64_Ehdr)
  e_shentsize := 64 -- sizeof (Elf64_Shdr)
  e_shnum := 6 --  [null, text, data, rela.text, symtab, string]
  e_shstrndx := 3 -- rela index
  e_phoff := 0
  e_shoff := 0
  e_entry := 0
  e_flags := 0
  e_phentsize := 0
  e_phnum := 0
}

structure ElfTwoPassLinkerState extends ElfBuilderState where
  header : Elf64_Ehdr := Elf64_Ehdr.default
  -- sections
  textSection : TextSection := {}
  dataSection : DataSection := {}
  relaTextSection : RelaTextSection := {}
  symtabSection : SymtabSection := {}
  /- resolved virtual index to uint64. -/
  virtualAddrResponses : HashMap VirtualAddr RealValue := {}
  /- add a request to write a virtual value `vix` at index `nat`, as kind `kind` (32/64 bit). -/
  virtualAddrRequests : Array (VirtualAddr × Nat × RealValueKind) := {}
  /- add a request to write a relocation entry to relocate symbol `sym` with relocation info `relocInfo` -/
  relocWriteRequests : Array (Elf64_Rela) := {}

def ElfTwoPassLinkerState.ofElfBuilderState (builderState : ElfBuilderState) : ElfTwoPassLinkerState :=
  { toElfBuilderState := builderState }


/-- A two pass algorithm to write out what has been built by ELFBuilder -/
abbrev ElfTwoPassLinkerM := StateT ElfTwoPassLinkerState ElfWriterM

namespace ElfTwoPassLinkerM

def addVirtualAddrWriteRequest (vaddr : VirtualAddr) (kind : RealValueKind) : ElfTwoPassLinkerM Unit := do
  let ix ← ElfWriterM.tell
  modify fun s => { s with virtualAddrRequests := s.virtualAddrRequests.push (vaddr, ix, kind) }

def addRelocWriteRequest (symtabIx : Nat) (symtabType : Elf64_Word) (offset addend : Nat) : ElfTwoPassLinkerM Unit := do
  let rela : Elf64_Rela := { r_offset := offset.toUInt64, r_addend := addend.toUInt64 }
  let rela := rela.setSymbolAndType symtabIx.toUInt32 symtabType
  modify fun s => { s with relocWriteRequests := s.relocWriteRequests.push rela }

def resolveVirtualAddr (vaddr : VirtualAddr) (value : RealValue) : ElfTwoPassLinkerM Unit := do
  modify fun s => { s with virtualAddrResponses := s.virtualAddrResponses.insert vaddr value }

def resolveVirtualAddrAsCurrentAddr (vaddr : VirtualAddr) : ElfTwoPassLinkerM Unit := do
  resolveVirtualAddr vaddr (.uint64 (← ElfWriterM.tell).toUInt64)

-- 'E' in hex is: 0x45
-- 'L' in hex is: 0x4c
-- 'F' in hex is: 0x46
def emitHeaderAndSectionHeadersPlaceholder  : ElfTwoPassLinkerM Unit := do
  let header := (← get).header
  ElfWriterM.write header
  let sh_offset ← ElfWriterM.tell
  modify fun s => { s with header.e_shoff := sh_offset.toUInt64 }
  ElfWriterM.seek (header.e_ehsize + header.e_shentsize * header.e_shnum).toNat

-- returns the symbol table address if we have one.
def emitVirtualAddr? (v : VirtualAddr?)
  (kind : RealValueKind)
  (symtabType : Elf64_Word)
  (offset addend : Nat) : ElfTwoPassLinkerM Unit := do
  match v with
  | .concrete v =>
    ElfWriterM.write <| (RealValue.uint64 v.toUint64).toKind kind
  | .virtual vaddr =>
      addVirtualAddrWriteRequest vaddr kind
      match vaddr.symtabIx? with
      | .none => pure ()
      | .some symtabIx =>
        addRelocWriteRequest symtabIx symtabType offset addend

/-- Emit placeholders to calculate locations of labels. -/
def emitInstruction : Instruction → ElfTwoPassLinkerM Unit
| Instruction.Label name => do
  resolveVirtualAddrAsCurrentAddr name
  return ()
| .MovAddr r a => do
    let code : Array UInt8 := #[0x48, 0xbf] ++ (List.replicate 8 0x00).toArray
    ElfWriterM.write code
| .Call f => do
    let code : Array UInt8 := #[0xe8] ++ (List.replicate 4 0x00).toArray
    ElfWriterM.write code
| .Xor r s => do
    let code : Array UInt8 := #[0x48, 0x31, 0xff]
    ElfWriterM.write code
| .Syscall opcode => do
    let code : Array UInt8 := #[0x0f, 0x05]
    ElfWriterM.write code
/-- Write Sections[1]: Text -/
def emitTextSection : ElfTwoPassLinkerM Unit := do
  let section_offset ← ElfWriterM.tell
  for instr in  (← get).instructions do
    emitInstruction instr
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with textSection := { offset := section_offset, size := section_size } }

/-- Write Sections[2]: Data -/
def emitDataSection : ElfTwoPassLinkerM Unit := do
  let section_offset ← ElfWriterM.tell
  let strs : ByteArray ← (← get).str2vaddr.foldM (init := ByteArray.empty) fun strs str vaddr => do
    resolveVirtualAddrAsCurrentAddr vaddr
    let strs := strs ++ str.toUTF8.push 0x00.toUInt8
    return strs
  ElfWriterM.write strs
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with dataSection := { offset := section_offset, size := section_size } }

/-- Write Sections[3] : Rela -/
def emitRelaTextSection : ElfTwoPassLinkerM Unit := do
  let section_offset ← ElfWriterM.tell
  for rela in (← get).relocWriteRequests do
    ElfWriterM.write rela
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with relaTextSection := { offset := section_offset, size := section_size } }

/-- Write Sections[4] : symtab -/
def emitSymtabSection : ElfTwoPassLinkerM Unit := do
  let section_offset ← ElfWriterM.tell
  (← get).str2vaddr.forM fun str vaddr => do
    pure ()
  (← get).extern2vaddr.forM fun extern vaddr => do
    pure ()
  let section_size := (← ElfWriterM.tell) - section_offset
  modify fun s => { s with
    relaTextSection := {
      offset := section_offset,
      size := section_size
    }
  }

/-- Once all data has been written, perform the second pass and fixup all requests. -/
def fixupVirtualAddrRequests : ElfTwoPassLinkerM Unit := do
  for (vix, ix, writeKind) in (← get).virtualAddrRequests do
    let value ← match (← get).virtualAddrResponses.find? vix with
      | .some v => pure v
      | .none => throw $ IO.userError "Unresolved virtual value"
    ElfWriterM.withSeek ix do
      match writeKind with
      | .uint64 => ElfWriterM.write value.toUInt64
      | .uint32 => ElfWriterM.write value.toUInt32
  return ()

def emitSectionHeaders : ElfTwoPassLinkerM Unit := sorry

namespace Internal
def linkerScript : ElfTwoPassLinkerM Unit := do
  emitHeaderAndSectionHeadersPlaceholder
  emitTextSection
  emitDataSection
  emitRelaTextSection
  emitSymtabSection
  -- TODO: emitSectionHeaders
  emitSectionHeaders
end Internal

def runAndGetWriterM (state : ElfBuilderState) : ElfWriterM Unit := do
  StateT.run' Internal.linkerScript (ElfTwoPassLinkerState.ofElfBuilderState state)

def runAndGetByteArray (state : ElfBuilderState) : IO ByteArray := do
  (runAndGetWriterM state).writeToBuffer

def runAndWriteToFileHandle (state : ElfBuilderState) (handle : IO.FS.Handle) : IO Unit := do
  (runAndGetWriterM state).writeToFileHandle handle

def runAndWriteToFilePath (state : ElfBuilderState) (path : System.FilePath) : IO Unit := do
  (runAndGetWriterM state).writeToFilePath path
end ElfTwoPassLinkerM

namespace ElfBuilderM

def runAndGetByteArray (builder : ElfBuilderM Unit) : IO ByteArray := do
  ElfTwoPassLinkerM.runAndGetByteArray (← builder.runAndGetElfBuilderState )

def runAndWriteToFileHandle (builder : ElfBuilderM Unit) (handle : IO.FS.Handle) : IO Unit := do
  ElfTwoPassLinkerM.runAndWriteToFileHandle (← builder.runAndGetElfBuilderState) handle

def runAndWriteToFilePath (builder : ElfBuilderM Unit) (path : System.FilePath) : IO Unit := do
  ElfTwoPassLinkerM.runAndWriteToFilePath (← builder.runAndGetElfBuilderState) path

end ElfBuilderM


