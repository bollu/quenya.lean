import Lean

open Lean IO FS

inductive Register
| rax
| rdi

-- A symbolic value of type `t`.
structure SymbolicValue (t : Type) where
  ix : UInt32 -- symbolic handle index.

inductive Symbolic? (t : Type) where
| concrete (v : t)
| symbolic (s : SymbolicValue t)

instance : Coe t (Symbolic? t) where
  coe := .concrete

instance : Coe (SymbolicValue t) (Symbolic? t) where
  coe := .symbolic

abbrev Addr := UInt32

-- X86 instruction set.
inductive Instruction
| MovAddr (r : Register) (a : Symbolic? Addr)
| Call (fn : Symbolic? Addr)
| Xor (r : Register) (s : Register)
| Syscall (opcode : UInt32)

structure BuilderHandle (n : Name) where
  ix : UInt32

structure ElfBuilderState where
  nextHandle : UInt32
  symbolicAddrs : HashMap Name (SymbolicValue Addr)
  strings : Array String
  instructions : Array Instruction

abbrev ELFBuilderM := StateT ElfBuilderState IO

/-- Allocate a new handle. Implementation detail. -/
def ElfBuilderM._allocateHandle : ELFBuilderM UInt32 := do
  let ix := (← get).nextHandle
  modify fun s => { s with nextHandle := s.nextHandle + 1 }
  pure ix

/-- Create a named (potentially) external symbol and return its address -/
def ElfBuilderM.getOrCreateSymbol (n : Name) : ELFBuilderM (SymbolicValue Addr) := do
  match (← get).symbolicAddrs.find? n with
  | some v => pure v
  | none => do
    let ix ← _allocateHandle
    let v : SymbolicValue Addr := { ix := ix }
    modify fun s => { s with symbolicAddrs := s.symbolicAddrs.insert n v }
    pure v

/-- Create a string and return its symbolic address. -/
def ElfBuilderM.createString (str : String) : ELFBuilderM (SymbolicValue Addr) := do
  let ix ← _allocateHandle
  modify fun s => { s with strings := s.strings.push str }
  pure { ix := ix }

def ElfBuilderM.emitInstruction (i : Instruction) : ELFBuilderM Unit := do
  modify fun s => { s with instructions := s.instructions.push i }

def ElfBuilderM.emitMovAddr (r : Register) (a : Symbolic? Addr) : ELFBuilderM Unit :=
  emitInstruction (Instruction.MovAddr r a)

def ElfBuilderM.emitCall (fn : Symbolic? Addr) : ELFBuilderM Unit :=
  emitInstruction (Instruction.Call fn)

def ElfBuilderM.emitXor (r : Register) (s : Register) : ELFBuilderM Unit :=
  emitInstruction (Instruction.Xor r s)

def ElfBuilderM.emitSyscall (opcode : UInt32) : ELFBuilderM Unit :=
  emitInstruction (Instruction.Syscall opcode)


/-
    // Create and initialize ELF header
    Elf64_Ehdr ehdr = {
        .e_ident = {0x7f, 'E', 'L', 'F', ELFCLASS64, ELFDATA2LSB, EV_CURRENT},
        .e_type = ET_REL,
        .e_machine = EM_X86_64,
        .e_version = EV_CURRENT,
        .e_ehsize = sizeof(Elf64_Ehdr),
        .e_shentsize = sizeof(Elf64_Shdr),
        .e_shnum = 6, // Number of sections (including null section) [null, text, data, rela.text, symtab]
        .e_shstrndx = 3, // No section name string table
        .e_phoff = 0,
        .e_shoff = 0,
    };
    fwrite(&ehdr, sizeof(ehdr), 1, fp);
-/

def hello := "world"
