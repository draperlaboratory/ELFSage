import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure RawRelocation where
  /-- Address at which to relocate -/
  r_offset : Nat
  /-- Symbol table index or type of relocation to apply -/
  r_info   : Nat
  deriving Repr

structure RawRelocationA where
  /-- Address at which to relocate -/
  ra_offset : Nat
  /-- Symbol table index or type of relocation to apply -/
  ra_info   : Nat
  /-- Addend used to compute value to be stored -/
  ra_addend : Int
  deriving Repr

structure ELF32Relocation where
  /-- Address at which to relocate -/
  r_offset : elf32_addr
  /-- Symbol table index or type of relocation to apply -/
  r_info   : elf32_word

structure ELF32RelocationA where
  /-- Address at which to relocate -/
  ra_offset : elf32_addr
  /-- Symbol table index or type of relocation to apply -/
  ra_info   : elf32_word
  /-- Addend used to compute value to be stored -/
  ra_addend : elf32_sword

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

def ELF32Relocation.toRawRelocation
  (r : ELF32Relocation)
  : RawRelocation := {
    r_offset := r.r_offset.toNat
    r_info   := r.r_info.toNat
  }

def ELF32RelocationA.toRawRelocationA
  (ra : ELF32RelocationA)
  : RawRelocationA := {
    ra_offset := ra.ra_offset.toNat
    ra_info   := ra.ra_info.toNat
    ra_addend := ra.ra_addend.toInt
  }

structure ELF64Relocation where
  /-- Address at which to relocate -/
  r_offset : elf64_addr
  /-- Symbol table index or type of relocation to apply -/
  r_info   : elf64_xword

structure ELF64RelocationA where
  /-- Address at which to relocate -/
  ra_offset : elf64_addr
  /-- Symbol table index or type of relocation to apply -/
  ra_info   : elf64_xword
  /-- Addend used to compute value to be stored -/
  ra_addend : elf64_sxword

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

def ELF64Relocation.toRawRelocation
  (r : ELF64Relocation)
  : RawRelocation := {
    r_offset := r.r_offset.toNat
    r_info   := r.r_info.toNat
  }

def ELF64RelocationA.toRawRelocationA
  (ra : ELF64RelocationA)
  : RawRelocationA := {
    ra_offset := ra.ra_offset.toNat
    ra_info   := ra.ra_info.toNat
    ra_addend := ra.ra_addend.toInt
  }

def mkRawRelocation?
  (bs : ByteArray)
  (is64Bit : Bool)
  (isBigendian : Bool)
  (offset : Nat)
  : Except String RawRelocation := 
  match is64Bit with
  | true   => 
    if h : bs.size - offset ≥ 0x10
    then pure (mkELF64Relocation isBigendian bs offset h).toRawRelocation
    else throw $ err 0x10
  | false  => 
    if h : bs.size - offset ≥ 0x08
    then pure (mkELF32Relocation isBigendian bs offset h).toRawRelocation
    else throw $ err 0xd
  where
    err size := s!
      "Tried to find a relocation at {offset}, but that doesn't leave enough space for the entry, " ++
      "which requires {size} bytes."

def mkRawRelocationA?
  (bs : ByteArray)
  (is64Bit : Bool)
  (isBigendian : Bool)
  (offset : Nat)
  : Except String RawRelocationA := 
  match is64Bit with
  | true   => 
    if h : bs.size - offset ≥ 0x18
    then pure (mkELF64RelocationA isBigendian bs offset h).toRawRelocationA
    else throw $ err 0x10
  | false  => 
    if h : bs.size - offset ≥ 0x0c
    then pure (mkELF32RelocationA isBigendian bs offset h).toRawRelocationA
    else throw $ err 0xd
  where
    err size := s!
      "Tried to find a relocation at {offset}, but that doesn't leave enough space for the entry, " ++
      "which requires {size} bytes."
