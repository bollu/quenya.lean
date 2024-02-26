/- a writer for writing data -/

abbrev Int32 := UInt32 -- this is a hack until we get proper a Int32 type.
abbrev Int64 := UInt64  -- this is a hack until we get proper a Int64 type.

def Int32.toUInt32 : Int32 → UInt32
| x => x

def Int64.toUInt64 : Int64 → UInt64
| x => x

def Nat.toInt32 (x : Nat) : Int32 := x.toUInt32
def Nat.toInt64 (x : Nat) : Int64 := x.toUInt64

structure ElfWriterState where
  ptr : Nat := 0 -- stack of indeces. Topmost index is the index we are currently writing to.
  arr : ByteArray := ByteArray.empty -- the buffer we are writing to.

abbrev ElfWriterM := StateT ElfWriterState IO

namespace ElfWriterM

def tell : ElfWriterM Nat := do
  return (← get).ptr

/-- Return current seek location. -/
def seek (ix : Nat) : ElfWriterM Unit := do
  modify fun s => { s with
    ptr := ix
    arr := Id.run do
      let mut arr := s.arr
      for _ in [0:ix - s.arr.size] do arr ← arr.push 0
      arr
  }

/-- Write computattion `x` at index `ix`, restoring the old location after. -/
def withSeek (ix : Nat) (x : ElfWriterM α) : ElfWriterM α := do
  let oldix ← tell
  seek ix
  let a ← x
  seek oldix
  pure a


def writeByteArray (b : ByteArray) : ElfWriterM Unit := do
  let ix ← tell
  modify fun s => { s with
    arr := ByteArray.copySlice (src := s.arr) (srcOff := 0) (dest := b) (destOff := ix) (len := b.size)
    ptr := s.ptr + b.size
  }

def writeUInt8 (x : UInt8) : ElfWriterM Unit := do
  modify fun s => { s with
    arr := s.arr.set! s.ptr x
    ptr := s.ptr + 1
  }

def writeUInt16 (x : UInt16) : ElfWriterM Unit := do
  writeUInt8 (x &&& (0xff : UInt16)).toUInt8
  writeUInt8 ((x >>> 8) &&& (0xff : UInt16)).toUInt8

def writeUInt32 (x : UInt32) : ElfWriterM Unit := do
  for i in [0:4] do
    let i : UInt32 := i.toUInt32
    writeUInt8 ((x >>> (i*8)) &&& (0xff : UInt32)).toUInt8

def writeInt32 (x : Int32) : ElfWriterM Unit :=
  writeUInt32 (x : UInt32)

def writeUInt64 (x : UInt64) : ElfWriterM Unit := do
  for i in [0:8] do
    let i : UInt64 := i.toUInt64
    writeUInt8 ((x >>> (i*8)) &&& (0xff : UInt64)).toUInt8

def writeInt64 (x : Int64) : ElfWriterM Unit := do
  writeUInt64 (x : UInt64)

end ElfWriterM

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

namespace ElfWriterM

/-- Write into a ByteArray. -/
def writeToBuffer (writer : ElfWriterM Unit) : IO ByteArray := do
  let ((), state) ← writer.run {}
  return state.arr

/-- Write into a file handle. -/
def writeToFileHandle (writer : ElfWriterM Unit) (handle : IO.FS.Handle) : IO Unit := do
  handle.write (← writeToBuffer writer)

/-- Write into a file path. Creates file if it does not exist, truncates existing file.
  Note that a `System.FilePath` can be created from a `String`. -/
def writeToFilePath (writer : ElfWriterM Unit) (path : System.FilePath) : IO Unit := do
  IO.FS.writeBinFile path (← writeToBuffer writer)

end ElfWriterM

