namespace List

-- homegrown

/--
Wrapper for Array.insertionSort.
-/
def insertionSort (a : List α) (lt : α → α → Bool) : List α :=
  (a.toArray.insertionSort lt).toList

end List
