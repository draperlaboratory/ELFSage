namespace Flags

/--
Get the list of indices that match the set bits in a bitvector of length `numBits`.
The bitvector is represented as a Nat. The indices are zero-indexed.
-/
def getFlagBits (flags: Nat) (numBits: Nat) : List Nat :=
  (List.range numBits).filter (λ n => flags.testBit n)

end Flags
