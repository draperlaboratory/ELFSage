import ELFSage.Util.List

namespace String

-- from Mathlib

/-- Pad `s : String` with repeated occurrences of `c : Char` until it's of length `n`.
  If `s` is initially larger than `n`, just return `s`. -/
def leftpad (n : Nat) (c : Char) (s : String) : String :=
  ⟨List.leftpad n c s.data⟩

end String
