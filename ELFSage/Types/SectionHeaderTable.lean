import ELFSage.Types.Sizes
import ELFSage.Types.StringTable
import ELFSage.Util.ByteArray
import ELFSage.Types.ELFHeader
import ELFSage.Util.Hex

class SectionHeaderTableEntry (α : Type) where
  /-- Name of the section -/
  sh_name      : α → Nat
  /-- Type of the section and its semantics -/
  sh_type      : α → Nat
  /-- Flags associated with the section -/
  sh_flags     : α → Nat
  /-- Address of first byte of section in memory image -/
  sh_addr      : α → Nat
  /-- Offset from beginning of file of first byte of section -/
  sh_offset    : α → Nat
  /-- Section size in bytes -/
  sh_size      : α → Nat
  /-- Section header table index link -/
  sh_link      : α → Nat
  /-- Extra information, contents depends on type of section -/
  sh_info      : α → Nat
  /-- Alignment constraints for section -/
  sh_addralign : α → Nat
  /-- Size of each entry in table, if section is composed of entries. Otherwise zero. -/
  sh_entsize   : α → Nat
  /-- Underlying Bytes, requires Endianness --/
  bytes        : α → (isBigendian : Bool) → ByteArray

def SectionHeaderTableEntry.sh_end [SectionHeaderTableEntry α] (shte : α) :=
  sh_offset shte + sh_size shte

def SectionHeaderTableEntry.getSectionNames
  (shstrndx : Nat)
  [SectionHeaderTableEntry α] (sht : List α)
  (bytes : ByteArray)
  : Except String ELFStringTable := if h : shstrndx ≥ sht.length
    then .error "the shstrndx from the elf header requests a non-existent section"
    else
      let shstr_start := sh_offset sht[shstrndx]
      let shstr_end := shstr_start + sh_size sht[shstrndx]
      if shstr_end > bytes.size
      then .error "the section header string table runs off the end of the binary"
      else .ok ⟨bytes.extract shstr_start shstr_end⟩

-- Alignment requirments from man 5 elf
def SectionHeaderTableEntry.checkAlignment
  [SectionHeaderTableEntry α] (sh : α)
  : Except String Unit :=
  if sh_addralign sh < 2 then pure ()
  else do
    if sh_addralign sh % 2 != 0 then .error $
      s! "Corrupted sh_addralign: value is {Hex.toHex $ sh_addralign sh}, which is not a power of 2"
    if sh_addr sh % sh_addralign sh != 0 then .error $
      s! "Misaligned section: sh_addr is {Hex.toHex $ sh_addr sh}, " ++
      s! "but sh_addralign is {Hex.toHex $ sh_addralign sh}."

structure ELF64SectionHeaderTableEntry where
  /-- Name of the section -/
  sh_name      : elf64_word
  /-- Type of the section and its semantics -/
  sh_type      : elf64_word
  /-- Flags associated with the section -/
  sh_flags     : elf64_xword
  /-- Address of first byte of section in memory image -/
  sh_addr      : elf64_addr
  /-- Offset from beginning of file of first byte of section -/
  sh_offset    : elf64_off
  /-- Section size in bytes -/
  sh_size      : elf64_xword
  /-- Section header table index link -/
  sh_link      : elf64_word
  /-- Extra information, contents depends on type of section -/
  sh_info      : elf64_word
  /-- Alignment constraints for section -/
  sh_addralign : elf64_xword
  /-- Size of each entry in table, if section is composed of entries. Otherwise zero. -/
  sh_entsize   : elf64_xword
  deriving Repr

def mkELF64SectionHeaderTableEntry
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x40) :
  ELF64SectionHeaderTableEntry := {
    sh_name      := getUInt32from (offset + 0x00) (by omega),
    sh_type      := getUInt32from (offset + 0x04) (by omega),
    sh_flags     := getUInt64from (offset + 0x08) (by omega),
    sh_addr      := getUInt64from (offset + 0x10) (by omega),
    sh_offset    := getUInt64from (offset + 0x18) (by omega),
    sh_size      := getUInt64from (offset + 0x20) (by omega),
    sh_link      := getUInt32from (offset + 0x28) (by omega),
    sh_info      := getUInt32from (offset + 0x2C) (by omega),
    sh_addralign := getUInt64from (offset + 0x30) (by omega),
    sh_entsize   := getUInt64from (offset + 0x38) (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom

def mkELF64SectionHeaderTableEntry?
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  : Except String ELF64SectionHeaderTableEntry :=
  if h : bs.size - offset ≥ 0x40
  then .ok $ mkELF64SectionHeaderTableEntry isBigEndian bs offset h
  else .error $
    s!"The section header table entry offset 0x{Hex.toHex offset}, doesn't leave enough space for the entry. " ++
    s!"The entry requires 0x40 bytes, but the file ends at 0x{Hex.toHex bs.size}."

def ELF64SectionHeaderTableEntry.bytes (shte : ELF64SectionHeaderTableEntry) (isBigEndian : Bool) :=
  getBytes32 shte.sh_name ++
  getBytes32 shte.sh_type ++
  getBytes64 shte.sh_flags ++
  getBytes64 shte.sh_addr ++
  getBytes64 shte.sh_offset ++
  getBytes64 shte.sh_size ++
  getBytes32 shte.sh_link ++
  getBytes32 shte.sh_info ++
  getBytes64 shte.sh_addralign ++
  getBytes64 shte.sh_entsize
  where getBytes32 := if isBigEndian then UInt32.getBytesBEfrom else UInt32.getBytesLEfrom
        getBytes64 := if isBigEndian then UInt64.getBytesBEfrom else UInt64.getBytesLEfrom

instance : SectionHeaderTableEntry ELF64SectionHeaderTableEntry where
  sh_name sh      := sh.sh_name.toNat
  sh_type sh      := sh.sh_type.toNat
  sh_flags sh     := sh.sh_flags.toNat
  sh_addr sh      := sh.sh_addr.toNat
  sh_offset sh    := sh.sh_offset.toNat
  sh_size sh      := sh.sh_size.toNat
  sh_link sh      := sh.sh_link.toNat
  sh_info sh      := sh.sh_info.toNat
  sh_addralign sh := sh.sh_addralign.toNat
  sh_entsize sh   := sh.sh_entsize.toNat
  bytes sh        := sh.bytes

def ELF64Header.mkELF64SectionHeaderTable?
  (eh : ELF64Header)
  (bytes : ByteArray)
  : Except String (List ELF64SectionHeaderTableEntry) :=
  let isBigendian := ELFHeader.isBigendian eh
  let shoffsets := (List.range (ELFHeader.e_shnum eh)).map λidx ↦ ELFHeader.e_shoff eh + ELFHeader.e_shentsize eh * idx
  List.mapM (λoffset ↦ mkELF64SectionHeaderTableEntry? isBigendian bytes offset) shoffsets

structure ELF32SectionHeaderTableEntry where
  /-- Name of the section -/
  sh_name      : elf32_word
  /-- Type of the section and its semantics -/
  sh_type      : elf32_word
  /-- Flags associated with the section -/
  sh_flags     : elf32_word
  /-- Address of first byte of section in memory image -/
  sh_addr      : elf32_addr
  /-- Offset from beginning of file of first byte of section -/
  sh_offset    : elf32_off
  /-- Section size in bytes -/
  sh_size      : elf32_word
  /-- Section header table index link -/
  sh_link      : elf32_word
  /-- Extra information, contents depends on type of section -/
  sh_info      : elf32_word
  /-- Alignment constraints for section -/
  sh_addralign : elf32_word
  /-- Size of each entry in table, if section is composed of entries. Otherwise zero. -/
  sh_entsize   : elf32_word
  deriving Repr

def mkELF32SectionHeaderTableEntry
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x28) :
  ELF32SectionHeaderTableEntry := {
    sh_name      := getUInt32from (offset + 0x00) (by omega),
    sh_type      := getUInt32from (offset + 0x04) (by omega),
    sh_flags     := getUInt32from (offset + 0x08) (by omega),
    sh_addr      := getUInt32from (offset + 0x0C) (by omega),
    sh_offset    := getUInt32from (offset + 0x10) (by omega),
    sh_size      := getUInt32from (offset + 0x14) (by omega),
    sh_link      := getUInt32from (offset + 0x18) (by omega),
    sh_info      := getUInt32from (offset + 0x1C) (by omega),
    sh_addralign := getUInt32from (offset + 0x20) (by omega),
    sh_entsize   := getUInt32from (offset + 0x24) (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom

def mkELF32SectionHeaderTableEntry?
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  : Except String ELF32SectionHeaderTableEntry :=
  if h : bs.size - offset ≥ 0x28
  then .ok $ mkELF32SectionHeaderTableEntry isBigEndian bs offset h
  else .error $
    s!"The section header table entry offset {Hex.toHex offset}, doesn't leave enough space for the entry. " ++
      "The entry requires 0x28 bytes, but the file ends at {Hex.toHex bs.size}."

def ELF32SectionHeaderTableEntry.bytes (shte : ELF32SectionHeaderTableEntry) (isBigEndian : Bool) :=
  getBytes32 shte.sh_name ++
  getBytes32 shte.sh_type ++
  getBytes32 shte.sh_flags ++
  getBytes32 shte.sh_addr ++
  getBytes32 shte.sh_offset ++
  getBytes32 shte.sh_size ++
  getBytes32 shte.sh_link ++
  getBytes32 shte.sh_info ++
  getBytes32 shte.sh_addralign ++
  getBytes32 shte.sh_entsize
  where getBytes32 := if isBigEndian then UInt32.getBytesBEfrom else UInt32.getBytesLEfrom

instance : SectionHeaderTableEntry ELF32SectionHeaderTableEntry where
  sh_name sh      := sh.sh_name.toNat
  sh_type sh      := sh.sh_type.toNat
  sh_flags sh     := sh.sh_flags.toNat
  sh_addr sh      := sh.sh_addr.toNat
  sh_offset sh    := sh.sh_offset.toNat
  sh_size sh      := sh.sh_size.toNat
  sh_link sh      := sh.sh_link.toNat
  sh_info sh      := sh.sh_info.toNat
  sh_addralign sh := sh.sh_addralign.toNat
  sh_entsize sh   := sh.sh_entsize.toNat
  bytes sh        := sh.bytes

def ELF32Header.mkELF32SectionHeaderTable?
  (eh : ELF32Header)
  (bytes : ByteArray)
  : Except String (List ELF32SectionHeaderTableEntry) :=
  let isBigendian := ELFHeader.isBigendian eh
  let shoffsets := (List.range (ELFHeader.e_shnum eh)).map λidx ↦ ELFHeader.e_shoff eh + ELFHeader.e_shentsize eh * idx
  List.mapM (λoffset ↦ mkELF32SectionHeaderTableEntry? isBigendian bytes offset) shoffsets


inductive RawSectionHeaderTableEntry :=
  | elf32 : ELF32SectionHeaderTableEntry → RawSectionHeaderTableEntry
  | elf64 : ELF64SectionHeaderTableEntry → RawSectionHeaderTableEntry
  deriving Repr

instance : SectionHeaderTableEntry RawSectionHeaderTableEntry where
  sh_name sh      := match sh with | .elf64 sh => sh.sh_name.toNat      | .elf32 sh => sh.sh_name.toNat
  sh_type sh      := match sh with | .elf64 sh => sh.sh_type.toNat      | .elf32 sh => sh.sh_type.toNat
  sh_flags sh     := match sh with | .elf64 sh => sh.sh_flags.toNat     | .elf32 sh => sh.sh_flags.toNat
  sh_addr sh      := match sh with | .elf64 sh => sh.sh_addr.toNat      | .elf32 sh => sh.sh_addr.toNat
  sh_offset sh    := match sh with | .elf64 sh => sh.sh_offset.toNat    | .elf32 sh => sh.sh_offset.toNat
  sh_size sh      := match sh with | .elf64 sh => sh.sh_size.toNat      | .elf32 sh => sh.sh_size.toNat
  sh_link sh      := match sh with | .elf64 sh => sh.sh_link.toNat      | .elf32 sh => sh.sh_link.toNat
  sh_info sh      := match sh with | .elf64 sh => sh.sh_info.toNat      | .elf32 sh => sh.sh_info.toNat
  sh_addralign sh := match sh with | .elf64 sh => sh.sh_addralign.toNat | .elf32 sh => sh.sh_addralign.toNat
  sh_entsize sh   := match sh with | .elf64 sh => sh.sh_entsize.toNat   | .elf32 sh => sh.sh_entsize.toNat
  bytes sh        := match sh with | .elf64 sh => sh.bytes              | .elf32 sh => sh.bytes

def mkRawSectionHeaderTableEntry?
  (bs : ByteArray)
  (is64Bit : Bool)
  (isBigendian : Bool)
  (offset : Nat)
  : Except String RawSectionHeaderTableEntry :=
  if is64Bit
  then .elf64 <$> mkELF64SectionHeaderTableEntry? isBigendian bs offset
  else .elf32 <$> mkELF32SectionHeaderTableEntry? isBigendian bs offset

inductive RawSectionHeaderTable :=
  | elf32 : List ELF32SectionHeaderTableEntry → RawSectionHeaderTable
  | elf64 : List ELF64SectionHeaderTableEntry → RawSectionHeaderTable
