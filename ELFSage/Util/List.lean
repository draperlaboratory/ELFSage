namespace List

-- from Std

/--
Pads `l : List α` with repeated occurrences of `a : α` until it is of length `n`.
If `l` is initially larger than `n`, just return `l`.
-/
def leftpad (n : Nat) (a : α) (l : List α) : List α := replicate (n - length l) a ++ l


-- homegrown

/--
Wrapper for Array.insertionSort.
-/
def insertionSort (a : List α) (lt : α → α → Bool) : List α :=
  (a.toArray.insertionSort lt).toList

end List
