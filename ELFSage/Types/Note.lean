import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure RawNoteEntry where
  /-- The size of the name field -/
  note_namesz : Nat
  /-- The size of the description field -/
  note_descsz : Nat
  /-- The type of note -/
  note_type : Nat
  /-- The note name as a ByteArray-/
  note_name : ByteArray
  /-- The note description as a ByteArray -/
  note_desc : ByteArray
  deriving Repr

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

def ELF64NoteEntry.toRawNoteEntry
  (ne : ELF64NoteEntry) 
  : RawNoteEntry :=
  {
    note_namesz := ne.note_namesz.toNat
    note_descsz := ne.note_descsz.toNat
    note_type := ne.note_type.toNat
    note_name := ne.note_name
    note_desc := ne.note_desc
  }

def mkRawNoteEntry? 
  (bs : ByteArray)
  (isBigendian : Bool)
  (offset : Nat)
  : Except String RawNoteEntry := 
  if h : bs.size - offset ≥ 0xc 
  then ELF64NoteEntry.toRawNoteEntry <$> mkELF64NoteEntry? isBigendian bs offset h
  else .error "Note entry offset {offset} doesn't leave enough space for the note, which requires at least 12 bytes"
