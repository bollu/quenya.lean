import Elfbuilder

open ElfBuilderM in
def eg1 : ElfBuilderM Unit := do
  emitInstruction <| .Label "start"
  emitInstruction <| .Xor .rax .rax
  emitInstruction <| .MovImm32R (.uint32 0x60) .rdi
  emitInstruction <| .Syscall
  return ()

def main : IO Unit :=
  eg1.runAndWriteToFilePath "hello.exe"
