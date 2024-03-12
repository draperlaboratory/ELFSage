import ELFSage.Types.Sizes
import ELFSage.Types.ELF32Header
import ELFSage.Types.ELF64Header
import ELFSage.Util.ByteArray

/-- Represents the ELF header's ident field structure -/
class ELFIdent (α : Type) where
  /-- is the ELF binary bigendian? -/
  isBigendian : α → Bool
  /-- Does the ELF file use 64 bit addresses? -/
  is64Bit     : α → Bool

instance : ELFIdent ELF64Header where
  isBigendian h := let ⟨bytes, _⟩ := h.ident; bytes[0x5] == 2
  is64Bit h := let ⟨bytes, _⟩ := h.ident; bytes[0x4] == 2 --should never be false

instance : ELFIdent ELF32Header where
  isBigendian h := let ⟨bytes, _⟩ := h.ident; bytes[0x5] == 2
  is64Bit h := let ⟨bytes, _⟩ := h.ident; bytes[0x4] == 2 --should never be false
