--BigEndian

def ByteArray.getUInt64BEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 8) : UInt64 := 
  (bs.get ⟨offset + 0, by omega⟩).toUInt64 <<< 0x38 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt64 <<< 0x30 |||
  (bs.get ⟨offset + 2, by omega⟩).toUInt64 <<< 0x28 |||
  (bs.get ⟨offset + 3, by omega⟩).toUInt64 <<< 0x20 |||
  (bs.get ⟨offset + 4, by omega⟩).toUInt64 <<< 0x18 |||
  (bs.get ⟨offset + 5, by omega⟩).toUInt64 <<< 0x10 |||
  (bs.get ⟨offset + 6, by omega⟩).toUInt64 <<< 0x08 |||
  (bs.get ⟨offset + 7, by omega⟩).toUInt64

def ByteArray.getUInt32BEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 4) : UInt32 := 
  (bs.get ⟨offset + 0, by omega⟩).toUInt32 <<< 0x18 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt32 <<< 0x10 |||
  (bs.get ⟨offset + 2, by omega⟩).toUInt32 <<< 0x08 |||
  (bs.get ⟨offset + 3, by omega⟩).toUInt32

def ByteArray.getUInt16BEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 2) : UInt16 := 
  (bs.get ⟨offset + 0, by omega⟩).toUInt16 <<< 0x08 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt16

--LittleEndian

def ByteArray.getUInt64LEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 8) : UInt64 := 
  (bs.get ⟨offset + 7, by omega⟩).toUInt64 <<< 0x38 |||
  (bs.get ⟨offset + 6, by omega⟩).toUInt64 <<< 0x30 |||
  (bs.get ⟨offset + 5, by omega⟩).toUInt64 <<< 0x28 |||
  (bs.get ⟨offset + 4, by omega⟩).toUInt64 <<< 0x20 |||
  (bs.get ⟨offset + 3, by omega⟩).toUInt64 <<< 0x18 |||
  (bs.get ⟨offset + 2, by omega⟩).toUInt64 <<< 0x10 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt64 <<< 0x08 |||
  (bs.get ⟨offset + 0, by omega⟩).toUInt64

def ByteArray.getUInt32LEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 4) : UInt32 := 
  (bs.get ⟨offset + 3, by omega⟩).toUInt32 <<< 0x18 |||
  (bs.get ⟨offset + 2, by omega⟩).toUInt32 <<< 0x10 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt32 <<< 0x08 |||
  (bs.get ⟨offset + 0, by omega⟩).toUInt32

def ByteArray.getUInt16LEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 2) : UInt16 := 
  (bs.get ⟨offset + 1, by omega⟩).toUInt16 <<< 0x08 |||
  (bs.get ⟨offset + 0, by omega⟩).toUInt16

/- An alternative approach is to define a byte parsing monad, à la:

https://github.com/risc0/risc0-lean4/blob/31c956fc9246bbfc84359021d66ed94972afd86b/R0sy/ByteDeserial.lean

And just have some runtime error possibilities, rather than proof obligations. 
-/

def ByteArray.getEntriesFrom
  (bs : ByteArray)
  (offset : Nat)
  (num : Nat)
  (entsize : Nat)
  (entreq : Nat)
  (suffSize : entsize ≥ entreq)
  (spaceAvail : bs.size ≥ offset + num * entsize)
  (toEnt :  (offset₂ : Nat) → (bs.size - offset₂ ≥ entreq) → α)
  : List α := recur num [] (by omega)
  where recur (idx : Nat) acc (h₁ : idx ≤ num) : List α 
  := match idx with
    | 0 => acc
    | i + 1 =>
      let ent := toEnt (offset + (i * entsize)) $ by
        have h₂ : num * entsize ≥ i * entsize + entsize := by
          conv => rhs; rw [Nat.mul_comm]
          rw [←Nat.mul_succ]
          conv => rhs; rw [Nat.mul_comm]
          exact Nat.mul_le_mul_right entsize h₁
        omega
      recur i (ent :: acc) (by omega)

structure NByteArray (n : Nat) where
  bytes: ByteArray
  sized: bytes.size = n

instance : Repr (NByteArray n) where
  reprPrec nbs := reprPrec $ nbs.bytes.toList.map λbyte ↦ 
    Nat.toDigits 16 byte.toNat 
    |> (λl ↦ if l.length == 1 then '0' :: l else l) 
    |> List.asString

instance : Repr ByteArray where
  reprPrec nbs := reprPrec $ nbs.data.toList.map λbyte ↦ 
    Nat.toDigits 16 byte.toNat 
    |> (λl ↦ if l.length == 1 then '0' :: l else l) 
    |> List.asString

/- 

 These are some simple theorems related to Arrays and ByteArrays. They start
 from the function Array.extract.loop, associated with Array.extract:

 https://github.com/leanprover/lean4/blob/6fce8f7d5cd18a4419bca7fd51780c71c9b1cc5a/src/Init/Prelude.lean#L2722

 `Array.extract.loop src i j dst` essentially returns dst if i is 0 or
 j ≥ src.size, and otherwise calls itself as `Array.extract.loop src (i - 1) (j
 + 1) (dst ++ src[j])`.

 Some things it would be nice to prove, since it would give us an instance of

 https://leanprover-community.github.io/mathlib4_docs/Mathlib/Algebra/Group/Defs.html#AddCancelMonoid

 ∀{a b: ByteArray}, (a ++ b).size = a.size + b.size
 ∀{a b c: ByteArray}, a ++ b = c ++ b ↔ a = c
 ∀{a b c: ByteArray}, a ++ b = a ++ c ↔ b = c
 ∀{a b c: ByteArray}, (a ++ b) ++ c = a ++ (b ++ c)
 ∀{a: ByteArray}, a ++ ByteArray.empty = a
 ∀{a: ByteArray}, ByteArray.empty ++ a = a

 It would also be good to improve NByteArray extract to take an offset in addition to a length
-/ 

theorem Array.extract_len_aux {src: Array α} :
   ∀b l dst, (b + l ≤ src.size) →
   List.length (Array.extract.loop src b l dst).data = b + dst.size:= by
   intro b
   induction b
   case zero => simp [Array.extract.loop]
   case succ => 
     rename_i n ih
     intro l dst
     unfold Array.extract.loop
     intro lt
     simp; split
     · have : n + l + 1 ≤ size src := by omega
       rw [ih (l + 1) (push dst src[l]) this]
       simp_arith
     · omega

theorem Array.extract_loop_len {src : Array α} :
  ∀ b l dst,
   (b + l ≤ src.size) →
   (Array.extract.loop src b l dst).size = b + dst.size:= by
  intro i s e h
  rw [Array.size]
  apply Array.extract_len_aux _ _ _ h

def NByteArray.extract (bs : ByteArray) (n : Nat) (h : bs.size ≥ n) : NByteArray n :=
  let bytes := bs.extract 0 n
  let proof : bytes.size = n := by
      simp [ ByteArray.size
           , ByteArray.extract
           , ByteArray.copySlice
           , Array.extract
           , ByteArray.empty
           , ByteArray.mkEmpty
           ]
      have : ∀α, ∀n : Nat, @Array.extract.loop α #[] 0 n #[] = #[] := by 
        unfold Array.extract.loop
        split; simp; contradiction
      rw [this, this]
      have : ∀α, ∀a b : Array α, (a ++ b).size = a.size + b.size := by
        simp [Array.size]
      rw [this, this]
      simp
      have :  0 + (min n (List.length bs.data.data)) ≤ bs.size := by
        rw [Nat.min_def]
        split <;> omega
      rw [Array.extract_loop_len (min n (List.length bs.data.data)) 0 #[]]
      simp [Nat.min_def, ByteArray.size, Array.size] at *
      · omega
      · simp [Nat.min_def]; split
        · assumption
        · simp [Nat.min_def, ByteArray.size, Array.size] at *
    ⟨bytes, proof⟩
