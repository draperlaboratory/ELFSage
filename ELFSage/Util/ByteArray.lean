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


def UInt64.getBytesBEfrom (i : UInt64) : ByteArray :=
  ⟨#[ (i >>> 0x38).toUInt8
    , (i >>> 0x30).toUInt8
    , (i >>> 0x28).toUInt8
    , (i >>> 0x20).toUInt8
    , (i >>> 0x18).toUInt8
    , (i >>> 0x10).toUInt8
    , (i >>> 0x8).toUInt8
    , (i >>> 0x0).toUInt8
    ]⟩

def ByteArray.getUInt32BEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 4) : UInt32 :=
  (bs.get ⟨offset + 0, by omega⟩).toUInt32 <<< 0x18 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt32 <<< 0x10 |||
  (bs.get ⟨offset + 2, by omega⟩).toUInt32 <<< 0x08 |||
  (bs.get ⟨offset + 3, by omega⟩).toUInt32

def UInt32.getBytesBEfrom (i : UInt32) : ByteArray :=
  ⟨#[ (i >>> 0x18).toUInt8
    , (i >>> 0x10).toUInt8
    , (i >>> 0x8).toUInt8
    , (i >>> 0x0).toUInt8
    ]⟩

def ByteArray.getUInt16BEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 2) : UInt16 :=
  (bs.get ⟨offset + 0, by omega⟩).toUInt16 <<< 0x08 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt16

def UInt16.getBytesBEfrom (i : UInt16) : ByteArray :=
  ⟨#[ (i >>> 0x8).toUInt8
    , (i >>> 0x0).toUInt8
    ]⟩


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

def UInt64.getBytesLEfrom (i : UInt64) : ByteArray :=
  ⟨#[ (i >>> 0x00).toUInt8
    , (i >>> 0x8).toUInt8
    , (i >>> 0x10).toUInt8
    , (i >>> 0x18).toUInt8
    , (i >>> 0x20).toUInt8
    , (i >>> 0x28).toUInt8
    , (i >>> 0x30).toUInt8
    , (i >>> 0x38).toUInt8
    ]⟩

def ByteArray.getUInt32LEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 4) : UInt32 :=
  (bs.get ⟨offset + 3, by omega⟩).toUInt32 <<< 0x18 |||
  (bs.get ⟨offset + 2, by omega⟩).toUInt32 <<< 0x10 |||
  (bs.get ⟨offset + 1, by omega⟩).toUInt32 <<< 0x08 |||
  (bs.get ⟨offset + 0, by omega⟩).toUInt32

def UInt32.getBytesLEfrom (i : UInt32) : ByteArray :=
  ⟨#[ (i >>> 0x00).toUInt8
    , (i >>> 0x8).toUInt8
    , (i >>> 0x10).toUInt8
    , (i >>> 0x18).toUInt8
    ]⟩

def ByteArray.getUInt16LEfrom (bs : ByteArray) (offset : Nat) (h: bs.size - offset ≥ 2) : UInt16 :=
  (bs.get ⟨offset + 1, by omega⟩).toUInt16 <<< 0x08 |||
  (bs.get ⟨offset + 0, by omega⟩).toUInt16

def UInt16.getBytesLEfrom (i : UInt16) : ByteArray :=
  ⟨#[ (i >>> 0x00).toUInt8
    , (i >>> 0x8).toUInt8
    ]⟩

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

theorem UInt16.ByteArray_size : ∀ i : UInt16, i.getBytesBEfrom.size = 2 := by
  intro i
  simp [UInt16.getBytesBEfrom, ByteArray.size]

theorem UInt16.eq_of_val_eq : ∀{i j : UInt16}, i.val = j.val → i = j
  | ⟨_, _⟩, ⟨_, _⟩, rfl => rfl

theorem UInt16.val_eq_of_eq : ∀{i j : UInt16}, i = j → i.val = j.val
  | ⟨_, _⟩, ⟨_, _⟩, rfl => rfl

theorem UInt8.eq_of_val_eq : ∀{i j : UInt8}, i.val = j.val → i = j
  | ⟨_, _⟩, ⟨_, _⟩, rfl => rfl

theorem UInt8.val_eq_of_eq : ∀{i j : UInt8}, i = j → i.val = j.val
  | ⟨_, _⟩, ⟨_, _⟩, rfl => rfl

theorem UInt8_to_UInt16_round: ∀{i : UInt8}, i.toUInt16.toUInt8 = i := by
  intro i
  cases i
  rename_i i
  simp [UInt16.toUInt8, Nat.toUInt8, UInt8.ofNat]
  apply UInt8.eq_of_val_eq
  simp
  cases i
  rename_i val lt
  apply Fin.eq_of_val_eq
  simp [Fin.ofNat, UInt16.toNat, UInt8.toUInt16, Nat.toUInt16, UInt16.ofNat, UInt8.toNat]
  unfold UInt8.size at lt
  simp_arith
  apply Nat.mod_eq_of_lt
  exact lt

theorem UInt16_to_UInt8_round: ∀{i : UInt16}, i.toUInt8.toUInt16 = i % 256:= by
  intro i
  cases i
  rename_i i
  simp [UInt8.toUInt16, Nat.toUInt16, UInt16.ofNat]
  unfold HMod.hMod
  simp [instHMod, Mod.mod, UInt16.mod]
  apply UInt16.eq_of_val_eq
  simp [Fin.ofNat, UInt8.toNat, UInt16.toUInt8, Nat.toUInt8, UInt8.ofNat, UInt8.toNat, UInt16.toNat, Fin.mod]
  apply Nat.mod_eq_of_lt
  have : ↑i % 256 < 256 := @Nat.mod_lt ↑i 256 (by omega)
  omega

theorem Nat.bitwise_sum : ∀{n m k : Nat}, m % 2^n = 0 → k < 2^n → Nat.bitwise or m k = m + k := by
  intro n
  induction n
  case zero =>
    intro m k _ h₂
    simp_arith at *
    rw [h₂]
    simp [Nat.bitwise]
    intro h₃
    apply Eq.symm
    assumption
  case succ n ih =>
    intro m k h₁ h₂
    have l₀ := (Nat.dvd_iff_mod_eq_zero (2 ^ Nat.succ n) m).mpr h₁
    have l₁ : Dvd.dvd 2 m := by
      refine Nat.dvd_of_pow_dvd ?_ l₀
      simp_arith
    have l₂ : m % 2 = 0 := by
      apply Nat.mod_eq_zero_of_dvd
      assumption
    have l₃ : (m / 2) % 2 ^ n = 0 := by
      apply (Nat.dvd_iff_mod_eq_zero (2 ^ n) (m / 2)).mp
      apply Nat.dvd_of_mul_dvd_mul_right (k := 2) (by decide)
      rw [Nat.div_mul_cancel l₁]
      rw [←Nat.pow_succ]
      assumption
    have l₄ : (k /2) < 2 ^ n := by
      simp [Nat.pow_succ] at h₂
      apply Nat.div_lt_of_lt_mul
      rw [Nat.mul_comm]
      assumption
    unfold Nat.bitwise
    split <;> simp_arith
    . apply Eq.symm; assumption
    . split
      . simp_arith; apply Eq.symm; assumption
      . split
        case inr.inr.inl h₃ h₄ h₅ =>
          cases h₅
          case inl h₅ => rw [l₂] at h₅; contradiction
          case inr h₅ =>
            have ih := ih l₃ l₄
            simp_arith [ih]
            rw [Nat.mul_comm, Nat.div_mul_cancel l₁]
            simp_arith
            conv =>
              rhs
              rw [←Nat.mod_add_div k 2]
            simp_arith [h₅]
        case inr.inr.inr h₃ h₄ h₅ =>
            have ih := ih l₃ l₄
            rw [ih]
            simp_arith
            rw [Nat.mul_comm, Nat.div_mul_cancel l₁]
            cases (Nat.mod_two_eq_zero_or_one k)
            case inr rhs => have c : m % 2 = 1 ∨ k % 2 = 1 := Or.inr rhs; contradiction
            case inl lhs =>
              have : Dvd.dvd 2 k := (Nat.dvd_iff_mod_eq_zero 2 k).mpr lhs
              rw [Nat.mul_comm, Nat.div_mul_cancel this]

theorem UInt16.ByteArray_roundtrip :
  ∀ i : UInt16, ∀l, i.getBytesBEfrom.getUInt16BEfrom 0 l = i := by
  intro i l
  simp [
    UInt16.getBytesBEfrom,
    ByteArray.getUInt16BEfrom,
    ByteArray.get,
    Array.get,
    List.get,
  ]
  rw [UInt16_to_UInt8_round, UInt16_to_UInt8_round]

  simp [HOr.hOr, OrOp.or, lor, Fin.lor, Nat.lor]

  cases i; rename_i i; apply UInt16.eq_of_val_eq; simp

  cases i; rename_i i h; apply Fin.eq_of_val_eq; simp

  simp [
    HShiftRight.hShiftRight,
    ShiftRight.shiftRight,
    shiftRight,
    Fin.shiftRight,
    Nat.shiftRight,
    HMod.hMod,
    Mod.mod,
    mod,
    Fin.mod,
  ]

  simp [
    show Nat.mod i size = i from Nat.mod_eq_of_lt h,
    Nat.div_div_eq_div_mul,
  ]

  simp [
    HShiftLeft.hShiftLeft,
    ShiftLeft.shiftLeft,
    shiftLeft,
    Fin.shiftLeft,
    Nat.shiftLeft
  ]

  simp [
    show Nat.mod (Nat.mod (i / 256) size) 256 = Nat.mod (i / 256) 256 by
      apply Nat.mod_mod_of_dvd
      simp_arith,
    show (2 * (2 * (2 * (2 * (2 * (2 * (2 * (2 * (i / 256))))))))
      = 256 * (i / 256)) by simp_arith,
    show Nat.mod (i / 256) 256 = i / 256 by
      apply Nat.mod_eq_of_lt
      apply Nat.div_lt_of_lt_mul
      exact h
  ]

  have : Nat.mod (i / 256) 256 < 256 := Nat.mod_lt (i / 256) (by omega)
  have : 256 * (i / 256) < size := by omega
  have : 256 * (i / 256) % size = 256 * (i / 256) := Nat.mod_eq_of_lt this
  rw [this]

  have l₁ : ((2^8) * (i / 256)) % (2^8) = 0 := Nat.mul_mod_right (2^8) (i / 256)
  have l₂ : Nat.mod i (2^8) < (2^8) := Nat.mod_lt i (by decide)
  simp [Nat.bitwise_sum l₁ l₂]
  have l₃ : Nat.mod i 256 + 256 * (i / 256) = i := Nat.mod_add_div i 256
  rw [Nat.add_comm, l₃]
  have l₄ : Nat.mod i size = i := Nat.mod_eq_of_lt h
  assumption
