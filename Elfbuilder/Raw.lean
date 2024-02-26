/- Raw definitions of data from ELF.h and their associated writers -/
-- https://github.com/llvm/llvm-project/blob/1e98d4883d78ac2c65b87e24694e8b2f1dc9f02d/llvm/include/llvm/BinaryFormat/ELF.h
-- https://github.com/llvm/llvm-project/blob/1e98d4883d78ac2c65b87e24694e8b2f1dc9f02d/llvm/include/llvm/Object/ELF.h
import Elfbuilder.Writer


-- Type for a 16-bit quantity.
abbrev Elf64_Half := UInt16
instance : OfNat Elf64_Half n where ofNat := n.toUInt16

instance : ElfWriteable Elf64_Half where
  write (x : Elf64_Half) := ElfWriterM.writeUInt16 x

-- Types for signed and unsigned 32-bit quantities.
abbrev Elf64_Word := UInt32
abbrev Elf64_Sword := Int32

instance : OfNat Elf64_Word n where ofNat := n.toUInt32
instance : OfNat Elf64_Sword n where ofNat := n.toInt32

instance : ElfWriteable Elf64_Word where
  write (x : Elf64_Word) := ElfWriterM.writeUInt32 x

instance : ElfWriteable Elf64_Sword where
  write (x : Elf64_Sword) := ElfWriterM.writeInt32 x

-- Types for signed and unsigned 64-bit quantities.
abbrev Elf64_Xword := UInt64
abbrev Elf64_Sxword := Int64

instance : OfNat Elf64_Xword n where ofNat := n.toUInt64
instance : OfNat Elf64_Sxword n where ofNat := n.toInt64

instance : ElfWriteable Elf64_Xword where
  write (x : Elf64_Xword) := ElfWriterM.writeUInt64 x
instance : ElfWriteable Elf64_Sxword where
  write (x : Elf64_Sxword) := ElfWriterM.writeInt64 x

-- Type of addresses.
abbrev Elf64_Addr := UInt64

instance : OfNat Elf64_Addr n where ofNat := n.toUInt64

instance : ElfWriteable Elf64_Addr where
  write (x : Elf64_Addr) := ElfWriterM.writeUInt64 x

-- Type of file offsets.
abbrev Elf64_Off := UInt64

instance : OfNat Elf64_Off n where ofNat := n.toUInt64

instance : ElfWriteable Elf64_Off where
  write (x : Elf64_Off) := ElfWriterM.writeUInt64 x

-- Type for section indices, which are 16-bit quantities.
abbrev Elf64_Section := UInt16

instance : OfNat Elf64_Section n where ofNat := n.toUInt16

instance : ElfWriteable Elf64_Section where
  write (x : Elf64_Section) := ElfWriterM.writeUInt16 x

-- Type for version symbol information.
abbrev Elf64_Versym := Elf64_Half

instance : OfNat Elf64_Versym n where ofNat := n.toUInt16

instance : ElfWriteable Elf64_Versym where
  write (x : Elf64_Versym) := ElfWriterM.writeUInt16 x

-- https://github.com/llvm/llvm-project/blob/1e98d4883d78ac2c65b87e24694e8b2f1dc9f02d/llvm/include/llvm/BinaryFormat/ELF.h#L44
abbrev ElfMagic : Array UInt8 := #[0x7f, 0x45, 0x4c, 0x46, 0x00]

-- https://github.com/llvm/llvm-project/blob/1e98d4883d78ac2c65b87e24694e8b2f1dc9f02d/llvm/include/llvm/BinaryFormat/ELF.h#L46C1-L60C1
/-# e_ident size and indices -/
/-- File identification index -/
def EI_MAG0 : Nat := 0
/-- File identification index -/
def EI_MAG1 : Nat := 1
/-- File identification index -/
def EI_MAG2 : Nat := 2
/-- File identification index -/
def EI_MAG3 : Nat := 3
/-- File class -/
def EI_CLASS : Nat := 4
/-- Data encoding -/
def EI_DATA : Nat := 5
/-- File version -/
def EI_VERSION : Nat := 6
/-- OS/ABI identification -/
def EI_OSABI : Nat := 7
/-- ABI version -/
def EI_ABIVERSION : Nat := 8
/-- Start of padding bytes -/
def EI_PAD : Nat := 9
/-- Number of bytes in e_ident -/
def EI_NIDENT : Nat := 16

-- https://github.com/llvm/llvm-project/blob/1e98d4883d78ac2c65b87e24694e8b2f1dc9f02d/llvm/include/llvm/BinaryFormat/ELF.h#L87
structure Elf64_Ehdr : Type :=
    e_ident : Array UInt8 -- ELF identification bytes
    e_type : Elf64_Half -- Type of file (see ET_* below)
    e_machine : Elf64_Half -- Required architecture for this file (see EM_*)
    e_version : Elf64_Word -- Must be equal to 1
    e_entry : Elf64_Addr -- Address to jump to in order to start program
    e_phoff : Elf64_Off --  -- Program header table's file offset, in bytes
    e_shoff : Elf64_Off -- Section header table's file offset, in bytes
    e_flags : Elf64_Word -- Architecture-specific flags
    e_ehsize : Elf64_Half -- Size of ELF header, in bytes
    e_phentsize : Elf64_Half -- Size of an entry in the program header table
    e_phnum : Elf64_Half -- Number of entries in the program header table
    e_shentsize : Elf64_Half -- Size of an entry in the section header table
    e_shnum : Elf64_Half -- Number of entries in the section header table
    e_shstrndx : Elf64_Half -- Section header table index of section name string table

instance : ElfWriteable Elf64_Ehdr where
  write (hdr : Elf64_Ehdr) := do
    ElfWriteable.write hdr.e_ident
    ElfWriteable.write hdr.e_type
    ElfWriteable.write hdr.e_machine
    ElfWriteable.write hdr.e_version
    ElfWriteable.write hdr.e_entry
    ElfWriteable.write hdr.e_phoff
    ElfWriteable.write hdr.e_shoff
    ElfWriteable.write hdr.e_flags
    ElfWriteable.write hdr.e_ehsize
    ElfWriteable.write hdr.e_phentsize
    ElfWriteable.write hdr.e_phnum
    ElfWriteable.write hdr.e_shentsize
    ElfWriteable.write hdr.e_shnum
    ElfWriteable.write hdr.e_shstrndx

def Elf64_Ehdr.fileClass (hdr : Elf64_Ehdr) : UInt8 := hdr.e_ident[EI_CLASS]!
def Elf64_Ehdr.dataEncoding (hdr : Elf64_Ehdr) : UInt8 := hdr.e_ident[EI_DATA]!


def ET_NONE : Elf64_Half := 0 -- No file type
def ET_REL : Elf64_Half := 1 -- Relocatable file
def ET_EXEC : Elf64_Half := 2 -- Executable file
def ET_DYN : Elf64_Half := 3 -- Shared object file
def ET_CORE : Elf64_Half := 4 -- Core file
def ET_LOOS : Elf64_Half := 0xfe00 -- Beginning of Operating system-specific
def ET_HIOS : Elf64_Half := 0xfeff -- Operating system-specific
def ET_LOPROC : Elf64_Half := 0xff00 -- Beginning of Processor-specific
def ET_HIPROC : Elf64_Half := 0xffff -- Processor-specific

def EV_NONE : Elf64_Word := 0 -- Invalid version
def EV_CURRENT : Elf64_Word := 1 -- Current version

-- https://github.com/llvm/llvm-project/blob/main/llvm/include/llvm/BinaryFormat/ELF.h#L129-L323
def EM_X86_64 : Elf64_Half := 62 -- AMD x86-64 architecture


-- https://github.com/llvm/llvm-project/blob/main/llvm/include/llvm/BinaryFormat/ELF.h#L325-L330
/-# Object file classes -/

def ELFCLASSNONE : UInt8 := 0 -- Invalid class
def ELFCLASS32 : UInt8 := 1 -- 32-bit objects
def ELFCLASS64 : UInt8 := 2 -- 64-bit objects

/-# Object file byte orderings -/
def ELFDATANONE : UInt8 := 0 -- Invalid data encoding
def ELFDATA2LSB : UInt8 := 1 -- Little-endian
def ELFDATA2MSB : UInt8 := 2 -- Big-endian

/-- Relocation entry, without explicit addend -/
-- https://github.com/llvm/llvm-project/blob/main/llvm/include/llvm/BinaryFormat/ELF.h#L1392
structure Elf64_Rel where
    r_offset : Elf64_Addr -- Location (file byte offset, or program virtual addr) to apply the relocation action
    r_info : Elf64_Xword -- Symbol table index and type of relocation to apply

instance : ElfWriteable Elf64_Rel where
  write (rel : Elf64_Rel) := do
    ElfWriteable.write rel.r_offset
    ElfWriteable.write rel.r_info

def Elf64_Rel.symbol (rel : Elf64_Rel) : Elf64_Word := (rel.r_info >>> 32).toUInt32
def Elf64_Rel.type (rel : Elf64_Rel) : Elf64_Word := (rel.r_info &&& 0xffffffff).toUInt32
def Elf64_Rel.setSymbolAndType (rel : Elf64_Rel) (s : Elf64_Word) (t : Elf64_Word) : Elf64_Rel :=
    { rel with r_info := ((s.toUInt64 <<< 32) + (t.toUInt64 &&& 0xffffffff)) }
def Elf64_Rel.setSymbol (rel : Elf64_Rel) (s : Elf64_Word) : Elf64_Rel :=
  rel.setSymbolAndType s rel.type
def Elf64_Rel.setType (rel : Elf64_Rel) (t : Elf64_Word) : Elf64_Rel :=
  rel.setSymbolAndType rel.symbol t
-- https://github.com/llvm/llvm-project/blob/1e98d4883d78ac2c65b87e24694e8b2f1dc9f02d/llvm/include/llvm/BinaryFormat/ELF.h#L1407-L1422
/-- Relocation entry with explicit addend. -/
structure Elf64_Rela where
    r_offset : Elf64_Addr := 0-- Location (file byte offset, or program virtual addr) to apply the relocation action
    r_info : Elf64_Xword := 0-- Symbol table index and type of relocation to apply
    r_addend : Elf64_Sxword := 0 -- Explicit addend to compute value to be stored in the relocatable field

instance : ElfWriteable Elf64_Rela where
  write (rela : Elf64_Rela) := do
    ElfWriteable.write rela.r_offset
    ElfWriteable.write rela.r_info
    ElfWriteable.write rela.r_addend

def Elf64_Rela.symbol (rela : Elf64_Rela) : Elf64_Word := (rela.r_info >>> 32).toUInt32
def Elf64_Rela.type (rela : Elf64_Rela) : Elf64_Word := (rela.r_info &&& 0xffffffff).toUInt32
def Elf64_Rela.setSymbolAndType (rela : Elf64_Rela) (s : Elf64_Word) (t : Elf64_Word) : Elf64_Rela :=
  { rela with r_info := ((s.toUInt64 <<< 32) + (t.toUInt64 &&& 0xffffffff)) }
def Elf64_Rela.setSymbol (rela : Elf64_Rela) (s : Elf64_Word) : Elf64_Rela :=
  rela.setSymbolAndType s rela.type
def Elf64_Rela.setType (rela : Elf64_Rela) (t : Elf64_Word) : Elf64_Rela :=
  rela.setSymbolAndType rela.symbol t

/-
ELF_RELOC(R_X86_64_NONE,        0)
ELF_RELOC(R_X86_64_64,          1)
ELF_RELOC(R_X86_64_PC32,        2)
ELF_RELOC(R_X86_64_GOT32,       3)
ELF_RELOC(R_X86_64_PLT32,       4)
ELF_RELOC(R_X86_64_COPY,        5)
ELF_RELOC(R_X86_64_GLOB_DAT,    6)
ELF_RELOC(R_X86_64_JUMP_SLOT,   7)
ELF_RELOC(R_X86_64_RELATIVE,    8)
ELF_RELOC(R_X86_64_GOTPCREL,    9)
ELF_RELOC(R_X86_64_32,          10)
ELF_RELOC(R_X86_64_32S,         11)
ELF_RELOC(R_X86_64_16,          12)
ELF_RELOC(R_X86_64_PC16,        13)
ELF_RELOC(R_X86_64_8,           14)
ELF_RELOC(R_X86_64_PC8,         15)
ELF_RELOC(R_X86_64_DTPMOD64,    16)
ELF_RELOC(R_X86_64_DTPOFF64,    17)
ELF_RELOC(R_X86_64_TPOFF64,     18)
ELF_RELOC(R_X86_64_TLSGD,       19)
ELF_RELOC(R_X86_64_TLSLD,       20)
ELF_RELOC(R_X86_64_DTPOFF32,    21)
ELF_RELOC(R_X86_64_GOTTPOFF,    22)
ELF_RELOC(R_X86_64_TPOFF32,     23)
ELF_RELOC(R_X86_64_PC64,        24)
ELF_RELOC(R_X86_64_GOTOFF64,    25)
ELF_RELOC(R_X86_64_GOTPC32,     26)
ELF_RELOC(R_X86_64_GOT64,       27)
ELF_RELOC(R_X86_64_GOTPCREL64,  28)
ELF_RELOC(R_X86_64_GOTPC64,     29)
ELF_RELOC(R_X86_64_GOTPLT64,    30)
ELF_RELOC(R_X86_64_PLTOFF64,    31)
ELF_RELOC(R_X86_64_SIZE32,      32)
ELF_RELOC(R_X86_64_SIZE64,      33)
ELF_RELOC(R_X86_64_GOTPC32_TLSDESC,  34)
ELF_RELOC(R_X86_64_TLSDESC_CALL,     35)
ELF_RELOC(R_X86_64_TLSDESC,     36)
ELF_RELOC(R_X86_64_IRELATIVE,   37)
ELF_RELOC(R_X86_64_GOTPCRELX,   41)
ELF_RELOC(R_X86_64_REX_GOTPCRELX,    42)
-/
-- ELF relocations for x86-64
-- https://github.com/llvm/llvm-project/blob/cc53707a5c104eb7789829ecdb2e3ae2be1a42da/llvm/include/llvm/BinaryFormat/ELFRelocs/x86_64.def#L6
def R_X86_64_NONE : Elf64_Word := 0 -- No reloc
def R_X86_64_64 : Elf64_Word := 1 -- Direct 64 bit
def R_X86_64_PC32 : Elf64_Word := 2 -- PC relative 32 bit signed
def R_X86_64_GOT32 : Elf64_Word := 3 -- 32 bit GOT entry
def R_X86_64_PLT32 : Elf64_Word := 4 -- 32 bit PLT address
def R_X86_64_COPY : Elf64_Word := 5 -- Copy symbol at runtime
def R_X86_64_GLOB_DAT : Elf64_Word := 6 -- Create GOT entry
def R_X86_64_JUMP_SLOT : Elf64_Word := 7
def R_X86_64_RELATIVE : Elf64_Word := 8
def R_X86_64_GOTPCREL : Elf64_Word := 9
def R_X86_64_32 : Elf64_Word := 10
def R_X86_64_32S : Elf64_Word := 11
def R_X86_64_16 : Elf64_Word := 12
def R_X86_64_PC16 : Elf64_Word := 13
def R_X86_64_8 : Elf64_Word := 14
def R_X86_64_PC8 : Elf64_Word := 15
def R_X86_64_DTPMOD64 : Elf64_Word := 16
def R_X86_64_DTPOFF64 : Elf64_Word := 17
def R_X86_64_TPOFF64 : Elf64_Word := 18
def R_X86_64_TLSGD : Elf64_Word := 19
def R_X86_64_TLSLD : Elf64_Word := 20
def R_X86_64_DTPOFF32 : Elf64_Word := 21
def R_X86_64_GOTTPOFF : Elf64_Word := 22
def R_X86_64_TPOFF32 : Elf64_Word := 23
def R_X86_64_PC64 : Elf64_Word := 24
def R_X86_64_GOTOFF64 : Elf64_Word := 25
def R_X86_64_GOTPC32 : Elf64_Word := 26
def R_X86_64_GOT64 : Elf64_Word := 27
def R_X86_64_GOTPCREL64 : Elf64_Word := 28
def R_X86_64_GOTPC64 : Elf64_Word := 29
def R_X86_64_GOTPLT64 : Elf64_Word := 30
def R_X86_64_PLTOFF64 : Elf64_Word := 31
def R_X86_64_SIZE32 : Elf64_Word := 32
def R_X86_64_SIZE64 : Elf64_Word := 33
def R_X86_64_GOTPC32_TLSDESC : Elf64_Word := 34
def R_X86_64_TLSDESC_CALL : Elf64_Word := 35
def R_X86_64_TLSDESC : Elf64_Word := 36
def R_X86_64_IRELATIVE : Elf64_Word := 37
def R_X86_64_GOTPCRELX : Elf64_Word := 41
def R_X86_64_REX_GOTPCRELX : Elf64_Word := 42

-- https://github.com/llvm/llvm-project/blob/1e98d4883d78ac2c65b87e24694e8b2f1dc9f02d/llvm/include/llvm/BinaryFormat/ELF.h#L1280-L1298
structure Elf64_Sym where
    st_name : Elf64_Word -- Symbol name (index into string table)
    st_info : UInt8 -- Symbol's type and binding attributes
    st_other : UInt8 -- Must be zero; reserved
    st_shndx : Elf64_Half -- Which section (header tbl index) it's defined in
    st_value : Elf64_Addr -- Value or address associated with the symbol
    st_size : Elf64_Xword -- Size of the symbol

instance : ElfWriteable Elf64_Sym where
  write (sym : Elf64_Sym) := do
    ElfWriteable.write sym.st_name
    ElfWriteable.write sym.st_info
    ElfWriteable.write sym.st_other
    ElfWriteable.write sym.st_shndx
    ElfWriteable.write sym.st_value
    ElfWriteable.write sym.st_size

def Elf64_Sym.binding (sym : Elf64_Sym) : UInt8 := sym.st_info >>> 4
def Elf64_Sym.type (sym : Elf64_Sym) : UInt8 := sym.st_info &&& 0x0f
def Elf64_Sym.setBinding (sym : Elf64_Sym) (b : UInt8) : Elf64_Sym := { sym with st_info := (b <<< 4) + (sym.type &&& 0x0f) }
def Elf64_Sym.setType (sym : Elf64_Sym) (t : UInt8) : Elf64_Sym := { sym with st_info := (sym.binding <<< 4) + (t &&& 0x0f) }
def Elf64_Sym.setBindingAndType (sym : Elf64_Sym) (b : UInt8) (t : UInt8) : Elf64_Sym := { sym with st_info := (b <<< 4) + (t &&& 0x0f) }

-- https://github.com/llvm/llvm-project/blob/1e98d4883d78ac2c65b87e24694e8b2f1dc9f02d/llvm/include/llvm/BinaryFormat/ELF.h#L1307-L1316
-- Symbol bindings

/-
// Symbol bindings.
enum {
  STB_LOCAL = 0,  // Local symbol, not visible outside obj file containing def
  STB_GLOBAL = 1, // Global symbol, visible to all object files being combined
  STB_WEAK = 2,   // Weak symbol, like global but lower-precedence
  STB_GNU_UNIQUE = 10,
  STB_LOOS = 10,   // Lowest operating system-specific binding type
  STB_HIOS = 12,   // Highest operating system-specific binding type
  STB_LOPROC = 13, // Lowest processor-specific binding type
  STB_HIPROC = 15  // Highest processor-specific binding type
};
-/

def STB_LOCAL : UInt8 := 0
def STB_GLOBAL : UInt8 := 1
def STB_WEAK : UInt8 := 2
def STB_GNU_UNIQUE : UInt8 := 10
def STB_LOOS : UInt8 := 10
def STB_HIOS : UInt8 := 12
def STB_LOPROC : UInt8 := 13
def STB_HIPROC : UInt8 := 15


/- https://github.com/llvm/llvm-project/blob/1e98d4883d78ac2c65b87e24694e8b2f1dc9f02d/llvm/include/llvm/BinaryFormat/ELF.h#L1306C1-L1335-/
-- Symbol types

def STT_NOTYPE : UInt8 := 0 -- Symbol type is not specified
def STT_OBJECT : UInt8 := 1 -- Symbol is a data object (variable, array, etc.)
def STT_FUNC : UInt8 := 2 -- Symbol is the address of a function
def STT_SECTION : UInt8 := 3 -- Symbol is a section
def STT_FILE : UInt8 := 4 -- Symbol is associated with a file
def STT_COMMON : UInt8 := 5 -- Symbol is a common data object
def STT_TLS : UInt8 := 6 -- Symbol is thread-local data object
def STT_GNU_IFUNC : UInt8 := 10 -- Symbol is indirect code object
def STT_LOOS : UInt8 := 10 -- Lowest operating system-specific symbol type
def STT_HIOS : UInt8 := 12 -- Highest operating system-specific symbol type
def STT_LOPROC : UInt8 := 13 -- Lowest processor-specific symbol type
def STT_HIPROC : UInt8 := 15 -- Highest processor-specific symbol type
def STT_AMDGPU_HSA_KERNEL := 10 -- AMDGPU HSA kernel symbol

def STV_DEFAULT : UInt8 := 0 -- Visibility is specified by binding type
def STV_INTERNAL : UInt8 := 1 -- Defined by processor supplements
def STV_HIDDEN : UInt8 := 2 -- Not visible to other components
def STV_PROTECTED : UInt8 := 3 -- Visible in other components but not preemptable


def STN_UNDEF : Elf64_Half := 0 -- Undefined section

structure Elf64_Shdr where
    sh_name : Elf64_Word -- Section name (index into the section header string table)
    sh_type : Elf64_Word -- Section type (SHT_*)
    sh_flags : Elf64_Xword -- Section flags (SHF_*)
    sh_addr : Elf64_Addr -- Address where section is to be loaded
    sh_offset : Elf64_Off -- File offset of section data in bytes
    sh_size : Elf64_Xword -- Size of section data, in bytes
    sh_link : Elf64_Word -- Section type-specific header table index link
    sh_info : Elf64_Word -- Section type-specific extra information
    sh_addralign : Elf64_Xword -- Address alignment boundary
    sh_entsize : Elf64_Xword -- Size of entries, if section has table

instance : ElfWriteable Elf64_Shdr where
  write (shdr : Elf64_Shdr) := do
    ElfWriteable.write shdr.sh_name
    ElfWriteable.write shdr.sh_type
    ElfWriteable.write shdr.sh_flags
    ElfWriteable.write shdr.sh_addr
    ElfWriteable.write shdr.sh_offset
    ElfWriteable.write shdr.sh_size
    ElfWriteable.write shdr.sh_link
    ElfWriteable.write shdr.sh_info
    ElfWriteable.write shdr.sh_addralign
    ElfWriteable.write shdr.sh_entsize

def SHN_UNDEF : Elf64_Half := 0 -- Undefined section
def SHN_LORESERVE : Elf64_Half := 0xff00 -- Start of reserved indices
def SHN_LOPROC : Elf64_Half := 0xff00 -- Start of processor-specific
def SHN_HIPROC : Elf64_Half := 0xff1f -- End of processor-specific
def SHN_LOOS : Elf64_Half := 0xff20 -- Start of OS-specific
def SHN_HIOS : Elf64_Half := 0xff3f -- End of OS-specific
def SHN_ABS : Elf64_Half := 0xfff1 -- Symbol has absolute value; does not need relocation
def SHN_COMMON : Elf64_Half := 0xfff2 -- FORTRAN COMMON or C external global variables
def SHN_XINDEX : Elf64_Half := 0xffff -- Mark that the index is >= SHN_LORESERVE
def SHN_HIRESERVE : Elf64_Half := 0xffff -- End of reserved indices

def SHT_NULL : Elf64_Word := 0 -- No associated section (inactive entry)
def SHT_PROGBITS : Elf64_Word := 1 -- Program-defined contents
def SHT_SYMTAB : Elf64_Word := 2 -- Symbol table
def SHT_STRTAB : Elf64_Word := 3 -- String table
def SHT_RELA : Elf64_Word := 4 -- Relocation entries; explicit addends
def SHT_HASH : Elf64_Word := 5 -- Symbol hash table
def SHT_DYNAMIC : Elf64_Word := 6 -- Information for dynamic linking
def SHT_NOTE : Elf64_Word := 7 -- Information about the file
def SHT_NOBITS : Elf64_Word := 8 -- Data occupies no space in the file
def SHT_REL : Elf64_Word := 9 -- Relocation entries; no explicit addends
def SHT_SHLIB : Elf64_Word := 10 -- Reserved; unspecified semantics
def SHT_DYNSYM : Elf64_Word := 11 -- Symbol table
def SHT_INIT_ARRAY : Elf64_Word := 14 -- Pointers to initialization functions
def SHT_FINI_ARRAY : Elf64_Word := 15 -- Pointers to termination functions
def SHT_PREINIT_ARRAY : Elf64_Word := 16 -- Pointers to pre-init functions
def SHT_GROUP : Elf64_Word := 17 -- Section group
def SHT_SYMTAB_SHNDX : Elf64_Word := 18 -- Indices for SHN_XINDEX entries
def SHT_LOOS : Elf64_Word := 0x60000000 -- Start OS-specific

def SHF_WRITE : Elf64_Xword := 0x1 -- Section data should be writable during execution
def SHF_ALLOC : Elf64_Xword := 0x2 -- Section occupies memory during program execution
def SHF_EXECINSTR : Elf64_Xword := 0x4 -- Section contains executable machine instructions
def SHF_MERGE : Elf64_Xword := 0x10 -- Data in this section may be merged
def SHF_STRINGS : Elf64_Xword := 0x20 -- Section contains null-terminated strings
def SHF_INFO_LINK : Elf64_Xword := 0x40 -- sh_info holds section index
def SHF_LINK_ORDER : Elf64_Xword := 0x80 -- Preserve order after combining
def SHF_OS_NONCONFORMING : Elf64_Xword := 0x100 -- OS-specific processing required
def SHF_GROUP : Elf64_Xword := 0x200 -- Member of section group
def SHF_TLS : Elf64_Xword := 0x400 -- Section contains TLS data
def SHF_COMPRESSED : Elf64_Xword := 0x800 -- Section is compressed
def SHF_MASKOS : Elf64_Xword := 0x0ff00000 -- OS-specific
def SHF_MASKPROC : Elf64_Xword := 0xf0000000 -- Processor-specific
