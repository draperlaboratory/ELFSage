def dumpBytesAsHex (bytes : ByteArray) : IO Unit := do
    let mut idx := 0
    for byte in bytes do
      if idx % 8 == 0 then do IO.print " "
      if idx % 16 == 0 then do IO.print "\n"
      IO.print $ (Nat.toDigits 16 (byte.toNat))
        |> (λl ↦ if l.length == 1 then '0' :: l else l)
        |> List.asString
      IO.print " "
      idx ← pure $ idx + 1
