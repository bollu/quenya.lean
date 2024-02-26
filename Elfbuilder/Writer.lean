/- a writer for writing data -/

abbrev Int32 := UInt32 -- this is a hack until we get proper a Int32 type.
abbrev Int64 := UInt64  -- this is a hack until we get proper a Int64 type.

def Nat.toInt32 (x : Nat) : Int32 := x.toUInt32
def Nat.toInt64 (x : Nat) : Int64 := x.toUInt64

structure ElfWriterState where
  ptr : Nat := 0 -- stack of indeces. Topmost index is the index we are currently writing to.
  arr : ByteArray := ByteArray.empty -- the buffer we are writing to.

abbrev ElfWriterM := StateT ElfWriterState IO

def ElfWriterM.tell : ElfWriterM Nat := do
  return (← get).ptr

/-- Return current seek location. -/
def ElfWriterM.seek (ix : Nat) : ElfWriterM Unit := do
  modify fun s => { s with
    ptr := ix
    arr := Id.run do
      let mut arr := s.arr
      for _ in [0:ix - s.arr.size] do arr ← arr.push 0
      arr
  }

/-- Write computattion `x` at index `ix`, restoring the old location after. -/
def ElfWriterM.withSeek (ix : Nat) (x : ElfWriterM α) : ElfWriterM α := do
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


/-- Write any writeable value. -/
def ElfWriterM.write [ElfWriteable α] (x : α) : ElfWriterM Unit := ElfWriteable.write x

instance : ElfWriteable ByteArray where
  write x := ElfWriterM.writeByteArray x

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


/-- Write into a ByteArray. -/
def ElfWriterM.writeToBuffer (writer : ElfWriterM Unit) : IO ByteArray := do
  let ((), state) ← writer.run {}
  return state.arr

/-- Write into a file handle. -/
def ElfWriterM.writeToFileHandle (writer : ElfWriterM Unit) (handle : IO.FS.Handle) : IO Unit := do
  handle.write (← writeToBuffer writer)

/-- Write into a file path. Creates file if it does not exist, truncates existing file.
  Note that a `System.FilePath` can be created from a `String`. -/
def ElfWriterM.writeToFilePath (writer : ElfWriterM Unit) (path : System.FilePath) : IO Unit := do
  IO.FS.writeBinFile path (← writeToBuffer writer)


