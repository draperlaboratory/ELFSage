namespace List

-- from Std

/--
Pads `l : List α` with repeated occurrences of `a : α` until it is of length `n`.
If `l` is initially larger than `n`, just return `l`.
-/
def leftpad (n : Nat) (a : α) (l : List α) : List α := replicate (n - length l) a ++ l

end List
