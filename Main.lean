import Elfbuilder

open ElfBuilderM in
def eg1 : ElfBuilderM Unit := do
  emitInstruction <| .Label "_start"
  emitInstruction <| .Xor .rdi .rdi
  emitInstruction <| .MovImm32R (.uint32 60) .eax
  emitInstruction <| .Syscall
  return ()

def main : IO Unit :=
  eg1.runAndWriteToFilePath "eg1.o"
