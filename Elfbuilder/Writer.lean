/- a writer for writing data -/

abbrev Int32 := UInt32 -- this is a hack until we get proper a Int32 type.
abbrev Int64 := UInt64  -- this is a hack until we get proper a Int64 type.

def Nat.toInt32 (x : Nat) : Int32 := x.toUInt32
def Nat.toInt64 (x : Nat) : Int64 := x.toUInt64

structure ElfWriterState where
  elf : ElfBuilderState -- the ELF we are writing.
  ptr : Nat -- stack of indeces. Topmost index is the index we are currently writing to.
  arr : ByteArray -- the buffer we are writing to.

abbrev ElfWriterM := StateT ElfWriterState IO

def ElfWriterM.tell : ElfWriterM Nat := do
  return (← get).ptr

/-- Return current seek location. -/
def ElfWriterM.seek (ix : Nat) : ElfWriterM Unit := do
  modify fun s => { s with ptr := ix }

/-- Write computattion `x` at index `ix`, restoring the old location after. -/
def ElfWriterM.withSeek [Monad m] (ix : Nat) (x : ElfWriterM α) : ElfWriterM α := do
  let oldix ← tell
  seek ix
  let a ← x
  seek oldix
  pure a


def ElfWriterM.writeByteArray (b : ByteArray) : ElfWriterM Unit := do
  let ix ← tell
  modify fun s => { s with
    arr := ByteArray.copySlice (src := s.arr) (srcOff := 0) (dest := b) (destOff := ix) (len := b.size)
    ptr := s.ptr + b.size
  }

def ElfWriterM.writeUInt8 (x : UInt8) : ElfWriterM Unit := do
  modify fun s => { s with
    arr := s.arr.set! s.ptr x
    ptr := s.ptr + 1
  }
def ElfWriterM.writeUInt16 (x : UInt16) : ElfWriterM Unit := sorry
def ElfWriterM.writeUInt32 (x : UInt32) : ElfWriterM Unit := sorry
def ElfWriterM.writeInt32 (x : Int32) : ElfWriterM Unit := sorry
def ElfWriterM.writeUInt64 (x : UInt64) : ElfWriterM Unit := sorry
def ElfWriterM.writeInt64 (x : Int64) : ElfWriterM Unit := sorry


class ElfWriteable (α : Type) where
  write : α → ElfWriterM Unit

instance : ElfWriteable UInt8 where
  write x := ElfWriterM.writeUInt8 x

instance : ElfWriteable UInt16 where
  write x := ElfWriterM.writeUInt16 x

instance : ElfWriteable UInt32 where
  write x := ElfWriterM.writeUInt32 x

instance : ElfWriteable UInt64 where
  write x := ElfWriterM.writeUInt64 x

instance [ElfWriteable α] : ElfWriteable (List α) where
  write xs := xs.forM fun x => ElfWriteable.write x

instance [ElfWriteable α] : ElfWriteable (Array α) where
  write xs := xs.forM fun x => ElfWriteable.write x

instance [ElfWriteable α] : ElfWriteable (Option α) where
  write
  | none => pure ()
  | some x => ElfWriteable.write x
