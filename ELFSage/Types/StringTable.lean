import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure ELFStringTable where
  strings : ByteArray

def mkELFStringTable
  (bs : ByteArray)
  (offset : Nat)
  (size : Nat)
  (_ : bs.size ≥ offset + size)
  := bs.extract offset (offset + size)

def ELFStringTable.stringAt (st : ELFStringTable) (idx : Nat) : String :=
  match st.strings.findIdx? (· == 0) idx with
  | Option.none =>
    let range := st.strings.extract idx st.strings.size
    let chars := range.toList.map (λbyte => Char.ofNat byte.toNat)
    chars.toString
  | Option.some idx₂ =>
    let range := st.strings.extract idx idx₂
    let chars := range.toList.map (λbyte => Char.ofNat byte.toNat)
    chars.toString
