import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

class NoteEntry (α : Type) where
  /-- The size of the name field -/
  note_namesz : α → Nat
  /-- The size of the description field -/
  note_descsz : α → Nat
  /-- The type of note -/
  note_type : α → Nat
  /-- The note name as a ByteArray-/
  note_name : α → ByteArray
  /-- The note description as a ByteArray -/
  note_desc : α → ByteArray

structure ELF64NoteEntry where
  /-- The size of the name field -/
  note_namesz : elf64_word
  /-- The size of the description field -/
  note_descsz : elf64_word
  /-- The type of note -/
  note_type : elf64_word
  /-- The note name as a ByteArray-/
  note_name : ByteArray
  /-- The note description as a ByteArray -/
  note_desc : ByteArray
  deriving Repr

-- TODO this should check that the note doesn't overrun the end of the note section
def mkELF64NoteEntry?
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0xc) 
  : Except String ELF64NoteEntry :=
  let namesz := getUInt32from (offset + 0x0) (by omega)
  let descsz := getUInt32from (offset + 0x4) (by omega)
  if bs.size ≥ alignTo4 (offset + 0xc + namesz.toNat) + alignTo4 descsz.toNat
  then .ok {
    note_namesz := namesz
    note_descsz := descsz
    note_type := getUInt32from (offset + 0x8) (by omega)
    note_name := bs.extract (offset + 0xc) (offset + 0xc + namesz.toNat)
    note_desc := bs.extract (alignTo4 $ offset + 0xc + namesz.toNat) ((alignTo4 $ offset + 0xc + namesz.toNat) + descsz.toNat)
  } 
  else 
    .error "Note section specifies a name and description size that runs past the end of the binary"
  where
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
    -- the desc and name fields are required to have 4 byte alignment
    alignTo4 n := n + (n % 4)

-- Because ELF32 and ELF64 words are the same size, the two note types are really one and the same
abbrev ELF32NoteEntry := ELF64NoteEntry

instance : NoteEntry ELF64NoteEntry where
    note_namesz ne := ne.note_namesz.toNat
    note_descsz ne := ne.note_descsz.toNat
    note_type ne := ne.note_type.toNat
    note_name ne := ne.note_name
    note_desc ne := ne.note_desc

inductive RawNoteEntry where
  | elf64 : ELF64NoteEntry → RawNoteEntry
  | elf32 : ELF32NoteEntry → RawNoteEntry
  deriving Repr

instance : NoteEntry RawNoteEntry where
    note_namesz ne := match ne with | .elf64 ne => ne.note_namesz.toNat | .elf32 ne => ne.note_namesz.toNat 
    note_descsz ne := match ne with | .elf64 ne => ne.note_descsz.toNat | .elf32 ne => ne.note_descsz.toNat 
    note_type ne   := match ne with | .elf64 ne => ne.note_type.toNat   | .elf32 ne => ne.note_type.toNat   
    note_name ne   := match ne with | .elf64 ne => ne.note_name         | .elf32 ne => ne.note_name         
    note_desc ne   := match ne with | .elf64 ne => ne.note_desc         | .elf32 ne => ne.note_desc         

def mkRawNoteEntry? 
  (bs : ByteArray)
  (isBigendian : Bool)
  (is64Bit : Bool)
  (offset : Nat)
  : Except String RawNoteEntry := 
  if h : bs.size - offset ≥ 0xc 
  then if is64Bit then .elf64 <$> mkELF64NoteEntry? isBigendian bs offset h else .elf32 <$> mkELF64NoteEntry? isBigendian bs offset h
  else .error "Note entry offset {offset} doesn't leave enough space for the note, which requires at least 12 bytes"
