import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure ELFStringTable where
  strings : ByteArray

def mkELFStringTable
  (bs : ByteArray)
  (offset : Nat)
  (size : Nat)
  (_ : bs.size ≥ offset + size)
  : ELFStringTable
  := ⟨ bs.extract offset (offset + size) ⟩

def ELFStringTable.stringAt (st : ELFStringTable) (idx : Nat) : String :=
  match st.strings.findIdx? (· == 0) idx with
  | Option.none =>
    let range := st.strings.extract idx st.strings.size
    let chars := range.toList.map (λbyte => Char.ofNat byte.toNat)
    String.mk chars
  | Option.some idx₂ =>
    let range := st.strings.extract idx idx₂
    let chars := range.toList.map (λbyte => Char.ofNat byte.toNat)
    String.mk chars

structure StringTableEntry where
  /-- String associated with a string table entry --/
  string : String
  /-- Offset within the associated string table --/
  offset : Nat
  deriving Repr

-- Get all the string entries in an ELFStringTable as a list
def ELFStringTable.allStrings (st : ELFStringTable) : List StringTableEntry := Id.run do
  let mut idx := 0
  let mut idx₂ := 0
  let mut rslt := []
  for ptr in st.strings do
    if ptr = 0 then do
      let range := st.strings.extract idx idx₂
      let chars := range.toList.map (λbyte => Char.ofNat byte.toNat)
      rslt := ⟨String.mk chars, idx⟩::rslt
      idx := idx₂ + 1 -- point to the head of the next string, if there is one
    idx₂ := idx₂ + 1 -- sync with ptr
  return rslt

-- Get the offset associated with a given string inside of a string table
def ELFStringTable.stringToOffset (st : ELFStringTable) (str : String) : Option Nat := do
  let strings := st.allStrings
  let entry ← strings.find? (·.string = str)
  return entry.offset
