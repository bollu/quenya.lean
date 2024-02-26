import Elfbuilder

def eg1 : ElfBuilderM Unit := do
  return ()

def main : IO Unit :=
  eg1.runAndWriteToFilePath "hello.exe"
