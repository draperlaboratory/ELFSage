import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

class Relocation (α : Type) where
  /-- Address at which to relocate -/
  r_offset : α → Nat
  /-- Symbol table index or type of relocation to apply -/
  r_info   : α → Nat
  /-- Underlying Bytes, requires Endianness --/
  bytes    : α → (isBigendian : Bool) → ByteArray

class RelocationA (α : Type) where
  /-- Address at which to relocate -/
  ra_offset : α → Nat
  /-- Symbol table index or type of relocation to apply -/
  ra_info   : α → Nat
  /-- Addend used to compute value to be stored -/
  ra_addend : α → Int
  /-- Underlying Bytes, requires Endianness --/
  bytes    : α → (isBigendian : Bool) → ByteArray

structure ELF32Relocation where
  /-- Address at which to relocate -/
  r_offset : elf32_addr
  /-- Symbol table index or type of relocation to apply -/
  r_info   : elf32_word
  deriving Repr

def mkELF32Relocation
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x8) :
  ELF32Relocation := {
    r_offset := getUInt32from (offset + 0x0) (by omega)
    r_info   := getUInt32from (offset + 0x4) (by omega)
  } where
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom

def ELF32Relocation.bytes (r : ELF32Relocation) (isBigEndian : Bool) :=
  getBytes32 r.r_offset ++ getBytes32 r.r_info
  where getBytes32 := if isBigEndian then UInt32.getBytesBEfrom else UInt32.getBytesLEfrom

instance : Relocation ELF32Relocation where
    r_offset r := r.r_offset.toNat
    r_info   r := r.r_info.toNat
    bytes    r := r.bytes

structure ELF32RelocationA where
  /-- Address at which to relocate -/
  ra_offset : elf32_addr
  /-- Symbol table index or type of relocation to apply -/
  ra_info   : elf32_word
  /-- Addend used to compute value to be stored -/
  ra_addend : elf32_sword
  deriving Repr

def mkELF32RelocationA
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0xc) :
  ELF32RelocationA := {
    ra_offset := getUInt32from (offset + 0x0) (by omega)
    ra_info   := getUInt32from (offset + 0x4) (by omega)
    ra_addend := ⟨getUInt32from (offset + 0x8) (by omega)⟩
  } where
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom

def ELF32RelocationA.bytes (ra: ELF32RelocationA) (isBigEndian : Bool) :=
  getBytes32 ra.ra_offset
  ++ getBytes32 ra.ra_info
  ++ getBytes32 ra.ra_addend.bytes
  where getBytes32 := if isBigEndian then UInt32.getBytesBEfrom else UInt32.getBytesLEfrom

instance : RelocationA ELF32RelocationA where
    ra_offset ra := ra.ra_offset.toNat
    ra_info   ra := ra.ra_info.toNat
    ra_addend ra := ra.ra_addend.toInt
    bytes     ra := ra.bytes

structure ELF64Relocation where
  /-- Address at which to relocate -/
  r_offset : elf64_addr
  /-- Symbol table index or type of relocation to apply -/
  r_info   : elf64_xword
  deriving Repr

def mkELF64Relocation
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x10) :
  ELF64Relocation := {
    r_offset := getUInt64from (offset + 0x0) (by omega)
    r_info   := getUInt64from (offset + 0x8) (by omega)
  } where
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom

def ELF64Relocation.bytes (r : ELF64Relocation) (isBigEndian : Bool) :=
  getBytes64 r.r_offset ++ getBytes64 r.r_info
  where getBytes64 := if isBigEndian then UInt64.getBytesBEfrom else UInt64.getBytesLEfrom

instance : Relocation ELF64Relocation where
    r_offset r := r.r_offset.toNat
    r_info   r := r.r_info.toNat
    bytes    r := r.bytes

structure ELF64RelocationA where
  /-- Address at which to relocate -/
  ra_offset : elf64_addr
  /-- Symbol table index or type of relocation to apply -/
  ra_info   : elf64_xword
  /-- Addend used to compute value to be stored -/
  ra_addend : elf64_sxword
  deriving Repr

def mkELF64RelocationA
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x18) :
  ELF64RelocationA := {
    ra_offset := getUInt64from (offset + 0x0) (by omega)
    ra_info   := getUInt64from (offset + 0x8) (by omega)
    ra_addend := ⟨getUInt64from (offset + 0x10) (by omega)⟩
  } where
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom

def ELF64RelocationA.bytes (ra: ELF64RelocationA) (isBigEndian : Bool) :=
  getBytes64 ra.ra_offset
  ++ getBytes64 ra.ra_info
  ++ getBytes64 ra.ra_addend.bytes
  where getBytes64 := if isBigEndian then UInt64.getBytesBEfrom else UInt64.getBytesLEfrom

instance : RelocationA ELF64RelocationA where
    ra_offset ra := ra.ra_offset.toNat
    ra_info   ra := ra.ra_info.toNat
    ra_addend ra := ra.ra_addend.toInt
    bytes     ra := ra.bytes

inductive RawRelocation :=
  | elf64 : ELF64Relocation → RawRelocation
  | elf32 : ELF32Relocation → RawRelocation
  deriving Repr

instance : Relocation RawRelocation where
    r_offset r := match r with | .elf64 r => r.r_offset.toNat | .elf32 r => r.r_offset.toNat
    r_info   r := match r with | .elf64 r => r.r_info.toNat   | .elf32 r => r.r_info.toNat
    bytes    r := match r with | .elf64 r => r.bytes          | .elf32 r => r.bytes

def mkRawRelocation?
  (bs : ByteArray)
  (is64Bit : Bool)
  (isBigendian : Bool)
  (offset : Nat)
  : Except String RawRelocation :=
  match is64Bit with
  | true   =>
    if h : bs.size - offset ≥ 0x10
    then pure (.elf64 $ mkELF64Relocation isBigendian bs offset h)
    else throw $ err 0x10
  | false  =>
    if h : bs.size - offset ≥ 0x08
    then pure (.elf32 $ mkELF32Relocation isBigendian bs offset h)
    else throw $ err 0xd
  where
    err size :=
    s! "Tried to find a relocation at {offset}, but that doesn't leave enough space for the entry, " ++
    s! "which requires {size} bytes."

inductive RawRelocationA :=
  | elf64 : ELF64RelocationA → RawRelocationA
  | elf32 : ELF32RelocationA → RawRelocationA
  deriving Repr

instance : RelocationA RawRelocationA where
    ra_offset ra := match ra with | .elf64 ra => ra.ra_offset.toNat | .elf32 ra => ra.ra_offset.toNat
    ra_info   ra := match ra with | .elf64 ra => ra.ra_info.toNat   | .elf32 ra => ra.ra_info.toNat
    ra_addend ra := match ra with | .elf64 ra => ra.ra_addend.toInt | .elf32 ra => ra.ra_addend.toInt
    bytes     ra := match ra with | .elf64 ra => ra.bytes           | .elf32 ra => ra.bytes

def mkRawRelocationA?
  (bs : ByteArray)
  (is64Bit : Bool)
  (isBigendian : Bool)
  (offset : Nat)
  : Except String RawRelocationA :=
  match is64Bit with
  | true   =>
    if h : bs.size - offset ≥ 0x18
    then pure (.elf64 $ mkELF64RelocationA isBigendian bs offset h)
    else throw $ err 0x10
  | false  =>
    if h : bs.size - offset ≥ 0x0c
    then pure (.elf32 $ mkELF32RelocationA isBigendian bs offset h)
    else throw $ err 0xd
  where
    err size :=
    s! "Tried to find a relocation at {offset}, but that doesn't leave enough space for the entry, " ++
    s! "which requires {size} bytes."
