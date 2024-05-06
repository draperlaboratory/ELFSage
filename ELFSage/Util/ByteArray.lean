
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
   case succ n ih =>
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

theorem UInt32.eq_of_val_eq : ∀{i j : UInt32}, i.val = j.val → i = j
  | ⟨_, _⟩, ⟨_, _⟩, rfl => rfl

theorem UInt16.val_eq_of_eq : ∀{i j : UInt16}, i = j → i.val = j.val
  | ⟨_, _⟩, ⟨_, _⟩, rfl => rfl

theorem UInt8.eq_of_val_eq : ∀{i j : UInt8}, i.val = j.val → i = j
  | ⟨_, _⟩, ⟨_, _⟩, rfl => rfl

theorem UInt8.val_eq_of_eq : ∀{i j : UInt8}, i = j → i.val = j.val
  | ⟨_, _⟩, ⟨_, _⟩, rfl => rfl

theorem UInt8_to_UInt16_round: ∀{i : UInt8}, i.toUInt16.toUInt8 = i := by
  rintro ⟨i⟩
  simp [UInt16.toUInt8, Nat.toUInt8, UInt8.ofNat]
  apply UInt8.eq_of_val_eq
  simp
  rcases i with ⟨val, lt⟩
  apply Fin.eq_of_val_eq
  simp [Fin.ofNat, UInt16.toNat, UInt8.toUInt16, Nat.toUInt16, UInt16.ofNat, UInt8.toNat]
  unfold UInt8.size at lt
  simp_arith
  apply Nat.mod_eq_of_lt
  exact lt

theorem UInt16_to_UInt8_round: ∀{i : UInt16}, i.toUInt8.toUInt16 = i % 256:= by
  rintro ⟨i⟩
  simp [UInt8.toUInt16, Nat.toUInt16, UInt16.ofNat, HMod.hMod, instHMod, Mod.mod, UInt16.mod]
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
    simp_all [h₂, Nat.bitwise]
  case succ n ih =>
    intro m k h₁ h₂
    have l₀ := (Nat.dvd_iff_mod_eq_zero (2 ^ Nat.succ n) m).mpr h₁
    have l₁ : Dvd.dvd 2 m := by
      refine Nat.dvd_of_pow_dvd ?_ l₀
      simp_arith
    have l₂ : m % 2 = 0 := Nat.mod_eq_zero_of_dvd l₁
    have l₃ : (m / 2) % 2 ^ n = 0 := by
      apply (Nat.dvd_iff_mod_eq_zero (2 ^ n) (m / 2)).mp
      apply Nat.dvd_of_mul_dvd_mul_right (k := 2) (by decide)
      simp_all [Nat.div_mul_cancel l₁, ←Nat.pow_succ]
    have l₄ : (k /2) < 2 ^ n := by
      simp [Nat.pow_succ] at h₂
      apply Nat.div_lt_of_lt_mul
      simp_all [Nat.mul_comm]
    unfold Nat.bitwise
    split <;> simp_arith
    . simp_all
    . split
      . simp_all
      . split
        case inr.inr.inl h₃ h₄ h₅ =>
          cases h₅
          case inl h₅ => rw [l₂] at h₅; contradiction
          case inr h₅ =>
            have ih := ih l₃ l₄
            simp_arith [ih]
            rw [Nat.mul_comm, Nat.div_mul_cancel l₁]
            simp_arith
            conv => rhs; rw [←Nat.mod_add_div k 2]
            simp_arith [h₅]
        case inr.inr.inr h₃ h₄ h₅ =>
          have ih := ih l₃ l₄
          simp_arith [ih]
          rw [Nat.mul_comm, Nat.div_mul_cancel l₁]
          cases (Nat.mod_two_eq_zero_or_one k)
          case inr rhs => exfalso; apply h₅; exact Or.inr rhs
          case inl lhs =>
            have : Dvd.dvd 2 k := (Nat.dvd_iff_mod_eq_zero 2 k).mpr lhs
            rw [Nat.mul_comm, Nat.div_mul_cancel this]


/--
  OK, so the idea for doing these proofs more systematically would be to prove that

  i = ((i >>> 0x8) <<< 0x8 ) ||| i % 256

  for nat, then use this repeatedly (substituting for i >>> 0x8, i >>> 0x10) to get something like

  i = (((((i >>> 0x8) >>> 0x8) <<< 0x8 ) ||| (i >>> 0x8) % 256) <<< 0x8) ||| i % 256
    = (((i >>> 0x10) <<< 0x8 ) ||| (i >>> 0x8) % 256) <<< 0x8 ||| i % 256             --consolidate
    = (((i >>> 0x10) <<< 0x8 ) << 0x8) ||| ((i >>> 0x8) % 256) <<< 0x8 ||| i % 256    --distribute
    = (((i >>> 0x10) <<< 0x10) ||| ((i >>> 0x8) % 256) <<< 0x8 ||| i % 256            --consolidate
    ... and so on

  and then use these to prove the corresponding UInt facts

-/

macro "lower_to_nat" i:Lean.Parser.Tactic.casesTarget: tactic => `(tactic|(
  simp [
    HShiftRight.hShiftRight,
    ShiftRight.shiftRight,
    UInt16.shiftRight,
    UInt32.shiftRight,
    Fin.shiftRight,

    HShiftLeft.hShiftLeft,
    ShiftLeft.shiftLeft,
    UInt32.shiftLeft,
    UInt16.shiftLeft,
    Fin.shiftLeft,

    HMod.hMod,
    Mod.mod,
    UInt16.mod,
    UInt16.modn,
    UInt32.mod,
    UInt32.modn,
    Fin.mod,
    Fin.div,
    show (8 : UInt16).val = 8 by decide,
    show (8 : UInt32).val = 8 by decide,
    show (16 : UInt32).val = 16 by decide,
    show (24 : UInt32).val = 24 by decide,
    show (8 : Fin (2^16)).val = 8 by decide,
    show (8 : Fin (2^32)).val = 8 by decide,
    show (16 : Fin (2^32)).val = 16 by decide,
    show (24 : Fin (2^32)).val = 24 by decide,
    show Nat.mod 8 16 = 8 by decide,
    show Nat.mod 8 32 = 8 by decide,
    show Nat.mod 16 32 = 16 by decide,
    show Nat.mod 24 32 = 24 by decide,
    show Nat.mod 256 65536 = 256 by decide,

    HOr.hOr,
    OrOp.or,
  ]
  rcases $i with ⟨val⟩
  try apply UInt16.eq_of_val_eq
  try apply UInt32.eq_of_val_eq
  simp_arith
  rcases val with ⟨val, lt_val⟩
  apply Fin.eq_of_val_eq
  simp_arith
))

theorem UInt16.nullShift : ∀{i : UInt16}, i >>> 0 = i := by
  intro i
  lower_to_nat i
  apply Nat.mod_eq_of_lt
  assumption

@[simp]
theorem Nat.bitwise_zero_left : bitwise f 0 m = if f false true then m else 0 :=
  rfl

@[simp]
theorem Nat.bitwise_zero_right : bitwise f n 0 = if f true false then n else 0 := by
  unfold bitwise
  simp only [ite_self, decide_False, Nat.zero_div, ite_true, ite_eq_right_iff]
  rintro ⟨⟩
  split <;> rfl

theorem Nat.shiftLeft_toExp : x <<< n = 2^n * x := by
  induction n
  case zero => simp
  case succ n ih =>
    rw [Nat.pow_succ 2 n, Nat.shiftLeft_succ, ih]
    calc
      2 * (2 ^ n * x) = 2 * 2 ^ n * x := by rw [Nat.mul_assoc]
                    _ = 2 ^ n * 2 * x := by rw [Nat.mul_comm (2^n) 2]

theorem Nat.shiftRight_toDiv : x >>> n = x / 2^n := by
  induction n
  case zero => simp
  case succ n ih =>
    rw [Nat.pow_succ 2 n, Nat.shiftRight_succ, ih]
    rw [Nat.div_div_eq_div_mul]

theorem Nat.shiftRightLeft_leq : ∀n : Nat, (x >>> n) <<< n ≤ x := by
  intro n
  rw [Nat.shiftRight_toDiv, Nat.shiftLeft_toExp]
  apply Nat.mul_div_le

private theorem Nat.bitwise_dist_lem : f false false = false → 2 * Nat.bitwise f m n = Nat.bitwise f (2 * m) (2 * n) := by
  intro hyp
  unfold bitwise; split <;> split <;> try split <;> try split <;> try split
  all_goals try simp_all
  case inr.inr.inr.inr or₁ or₂ or₃ or₄ => simp_arith; (conv => rhs; unfold bitwise); simp_all
  all_goals (rename_i or₁ or₂ or₃ or₄; exfalso; apply or₁; cases Nat.mul_eq_zero.mp or₃ <;> simp_all)

theorem Nat.mod_pow_zero : n % m^(succ k) = 0 → n % m = 0 := by
  intro hyp
  rw [Nat.pow_succ] at hyp
  have : n % (m ^ k * m) % m = 0 % m := by rw [hyp]
  simp [Nat.mod_mul_left_mod n (m^k) m] at this
  assumption

theorem Nat.bitwise_mod : Nat.bitwise or m n % 2^k = Nat.bitwise or (m % 2^k) (n % 2^k) := by
  revert n m
  induction k
  case zero => simp_arith [Nat.mod_one]
  case succ k k_ih =>
  intro v n
  unfold bitwise; split <;> split <;> try split <;> try split <;> try split
  all_goals try simp_all <;> split
  all_goals repeat rewrite [show ∀{x}, x + x = 2 * x by simp_arith]
  all_goals rewrite [Nat.pow_succ]
  all_goals try split
  case inr.inr.inl.inl.inr or₁ or₂ or₃ or₄ =>
    conv => lhs; lhs; rw [Nat.mul_comm]
    rw [Nat.mul_mod_mul_right]
    have veven : v % 2 = 0 := Nat.mod_pow_zero or₃
    have neven : n % 2 = 0 := by
      cases Nat.mod_two_eq_zero_or_one n
      · assumption
      · exfalso; apply or₄; apply Or.inr; assumption
    have : v / 2 % 2 ^ k = 0 := by
      apply Nat.mul_right_cancel (show 0 < 2 by decide)
      rw [←Nat.mul_mod_mul_right, ←Nat.pow_succ, Nat.div_mul_cancel]
      assumption
      apply Nat.dvd_of_mod_eq_zero veven
    simp [k_ih, this]
    rw [←Nat.mul_mod_mul_right, ←Nat.pow_succ, Nat.div_mul_cancel]
    apply Nat.dvd_of_mod_eq_zero neven
  case inr.inr.inl.inl.inr or₁ or₂ or₃ or₄ =>
    conv => lhs; lhs; rw [Nat.mul_comm]
    rw [Nat.mul_mod_mul_right]
    have veven : v % 2 = 0 := by
      cases Nat.mod_two_eq_zero_or_one v
      · assumption
      · exfalso; apply or₄; apply Or.inl; assumption
    have neven : n % 2 = 0 := Nat.mod_pow_zero or₃
    have : n / 2 % 2 ^ k = 0 := by
      apply Nat.mul_right_cancel (show 0 < 2 by decide)
      rw [←Nat.mul_mod_mul_right, ←Nat.pow_succ, Nat.div_mul_cancel]
      assumption
      apply Nat.dvd_of_mod_eq_zero neven
    simp [k_ih, this]
    rw [←Nat.mul_mod_mul_right, ←Nat.pow_succ, Nat.div_mul_cancel]
    apply Nat.dvd_of_mod_eq_zero veven
  case inr.inr.inl.inl.inl or₁ or₂ or₃ or₄ =>
    conv => lhs; lhs; rw [Nat.mul_comm]
    conv => lhs; rw [←Nat.mod_add_mod]
    rw [Nat.mul_mod_mul_right]
    have veven : v % 2 = 0 := Nat.mod_pow_zero or₃
    have nodd : n % 2 = 1 := by
      cases or₄
      · rw [veven] at *; contradiction
      · assumption
    have : v / 2 % 2 ^ k = 0 := by
      apply Nat.mul_right_cancel (show 0 < 2 by decide)
      rw [←Nat.mul_mod_mul_right, ←Nat.pow_succ, Nat.div_mul_cancel]
      assumption
      apply Nat.dvd_of_mod_eq_zero veven
    simp [k_ih, this]
    rw [←Nat.mul_mod_mul_right, ←Nat.pow_succ]
    rw [Nat.add_comm, Nat.add_mod_mod]
    rw [Nat.add_comm, Nat.mul_comm, ←nodd, Nat.div_add_mod]
  case inr.inr.inr.inl.inl.inl or₁ or₂ or₃ or₄ or₅ =>
    conv => lhs; lhs; rw [Nat.mul_comm]
    conv => lhs; rw [←Nat.mod_add_mod]
    rw [Nat.mul_mod_mul_right]
    have neven : n % 2 = 0 := Nat.mod_pow_zero or₄
    have vodd : v % 2 = 1 := by
      cases or₅
      · assumption
      · rw [neven] at *; contradiction
    have : n / 2 % 2 ^ k = 0 := by
      apply Nat.mul_right_cancel (show 0 < 2 by decide)
      rw [←Nat.mul_mod_mul_right, ←Nat.pow_succ, Nat.div_mul_cancel]
      assumption
      apply Nat.dvd_of_mod_eq_zero neven
    simp [k_ih, this]
    rw [←Nat.mul_mod_mul_right, ←Nat.pow_succ]
    rw [Nat.add_comm, Nat.add_mod_mod]
    rw [Nat.add_comm, Nat.mul_comm, ←vodd, Nat.div_add_mod]
  case inr.inr.inr.inr.inl.inl or₁ or₂ or₃ or₄ or₅ or₆ =>
    rw [Nat.add_mod]
    rw [Nat.mul_comm]
    rw [Nat.mul_mod_mul_right]
    repeat rw [Nat.mod_mul_left_div_self]
    rw [←k_ih]
    simp_arith
    have modsmall : bitwise or (v / 2) (n / 2) % 2 ^ k < 2 ^ k := by
      apply Nat.mod_lt
      apply Nat.pow_pos
      trivial
    have : ∀{n}, n < 2^k → n * 2 + 1 < 2^k * 2 := by
      intro n hyp
      have : n + 1 ≤ 2^k := Nat.succ_le.mpr hyp
      have : (n + 1) * 2 ≤ 2^k * 2 := Nat.mul_le_mul_right 2 this
      apply Nat.lt_of_lt_of_le _ this
      simp_arith
    have : (bitwise or (v / 2) (n / 2) % 2 ^ k * 2 + 1) < (2 ^ k * 2) := this modsmall
    rw [Nat.mod_eq_of_lt this]
    simp_arith
  case inr.inr.inr.inr.inl.inr or₁ or₂ or₃ or₄ or₅ or₆ =>
    exfalso; apply or₆; simp; assumption
  case inr.inr.inr.inr.inr.inl or₁ or₂ or₃ or₄ or₅ or₆ =>
    exfalso; apply or₅; simp at or₆; assumption
  case inr.inr.inr.inr.inr.inr or₁ or₂ or₃ or₄ or₅ or₆ =>
    rw [Nat.mul_comm]
    rw [Nat.mul_mod_mul_right]
    repeat rw [Nat.mod_mul_left_div_self]
    rw [←k_ih]
    simp_arith



theorem Nat.bitwise_dist : f false false = false → 2^k * Nat.bitwise f m n = Nat.bitwise f (2^k * m) (2^k * n) := by
  induction k
  case zero => simp
  case succ k ih =>
    intro hyp
    simp [Nat.pow_succ, show 2^k * 2 = 2 * 2^k by apply Nat.mul_comm, Nat.mul_assoc]
    conv => rhs; rw [←Nat.bitwise_dist_lem hyp]
    rw [ih hyp]

theorem Nat.or_mul_dist : 2^k * (x ||| y) = 2^k * x ||| 2^k * y := by
  simp [HOr.hOr]; apply Nat.bitwise_dist; simp

theorem Nat.and_mul_dist : 2^k * (x &&& y) = 2^k * x &&& 2^k * y := by
  simp [HAnd.hAnd]; apply Nat.bitwise_dist; simp

theorem Nat.and_xor_dist : 2^k * (x ^^^ y) = 2^k * x ^^^ 2^k * y := by
  simp [HXor.hXor]; apply Nat.bitwise_dist; simp

theorem Nat.shiftLeft_distribute : ∀n : Nat, (x ||| y) <<< n = (x <<< n) ||| (y <<< n) := by
  intro n; simp [Nat.shiftLeft_toExp, Nat.or_mul_dist]

@[simp]
theorem UInt16.byteCeiling :  ∀(i  : UInt16), (i >>> 0x8) % 256 = i >>> 0x8 := by
  intro i; lower_to_nat i; rename_i val lt₁

  suffices (val >>> 0x8) % size % 256 = val >>> 0x8 % size by assumption
  rw [Nat.shiftRight_toDiv]
  have lt₂ : val / 2^8 < 256 := by
    apply (Nat.div_lt_iff_lt_mul (show 0 < 2^8 by decide)).mpr
    exact lt₁
  have lt₃ : val / 2^8 < size := by
    apply Nat.lt_trans lt₂; decide
  simp [Nat.mod_eq_of_lt lt₂, Nat.mod_eq_of_lt lt₃]

theorem Nat.splitBytes : ∀n: Nat, n = n >>> 0x8 <<< 0x8 ||| n % 256 := by
  intro n; simp [Nat.shiftLeft_toExp, Nat.shiftRight_toDiv]
  suffices 256 * (n / 256) ||| n % 256 = 256 * (n / 256) + n % 256 by
    rw [this, Nat.add_comm, Nat.mod_add_div]
  apply Nat.bitwise_sum (n:=8)
  · rw [Nat.mul_comm]; apply Nat.mul_mod_left
  · apply Nat.mod_lt; trivial

theorem UInt16.shiftUnshift : ∀(i  : UInt16), i = (i >>> 0x8 % 256) <<< 0x8 ||| i % 256 := by
  intro i; lower_to_nat i; rename_i val lt₁

  unfold lor; unfold Fin.lor
  simp [
    show ∀x y, Nat.lor x y = x ||| y by intro x y; rfl,
    show ∀x y, Nat.shiftRight x y = x >>> y by intro x y; rfl,
    show ∀x y, Nat.shiftLeft x y = x <<< y by intro x y; rfl,
    show ∀x y, Nat.mod x y = x % y by intro x y; rfl
  ]

  rw [Nat.shiftRight_toDiv]

  have lt₂ : val / 2^8 < 256 := by
    apply (Nat.div_lt_iff_lt_mul (show 0 < 2^8 by decide)).mpr
    exact lt₁

  have lt₃ : val / 2^8 < size := by
    apply Nat.lt_trans lt₂; decide

  simp [Nat.mod_eq_of_lt lt₂, Nat.mod_eq_of_lt lt₃, Nat.shiftLeft_toExp]

  have lt₄ : 256 * (val / 256) < size := by
    cases Nat.le_iff_lt_or_eq.mp (Nat.mul_div_le val 256)
    case inl or => apply Nat.lt_trans or; assumption
    case inr or => rw [or]; assumption

  simp [Nat.mod_eq_of_lt lt₄]

  rw [show 256 = 2^8 by decide, ←Nat.shiftLeft_toExp, ←Nat.shiftRight_toDiv, ←Nat.splitBytes]

  rw [Nat.mod_eq_of_lt lt₁]

theorem UInt32.shiftUnshift : ∀(i  : UInt32),
  i = (i >>> 0x18 % 256) <<< 0x18 |||
      (i >>> 0x10 % 256) <<< 0x10 |||
      (i >>> 0x08 % 256) <<< 0x08 |||
      i % 256 := by
  intro i;
  lower_to_nat i; rename_i val lt₁
  unfold lor
  unfold Fin.lor; simp
  simp [
    show ∀x y, Nat.lor x y = x ||| y by intro x y; rfl,
    show ∀x y, Nat.shiftRight x y = x >>> y by intro x y; rfl,
    show ∀x y, Nat.shiftLeft x y = x <<< y by intro x y; rfl,
    show ∀x y, Nat.mod x y = x % y by intro x y; rfl
  ]

  simp [Nat.shiftRight_toDiv]

  have lt₂ : val / 2^24 < size := by
    apply (Nat.div_lt_iff_lt_mul (show 0 < 2^24 by decide)).mpr
    apply Nat.lt_trans (m:=size)
    · assumption
    · decide

  have lt₃ : val / 2^16 < size := by
    apply (Nat.div_lt_iff_lt_mul (show 0 < 2^16 by decide)).mpr
    apply Nat.lt_trans (m:=size)
    · assumption
    · decide

  have lt₄ : val / 2^8 < size := by
    apply (Nat.div_lt_iff_lt_mul (show 0 < 2^8 by decide)).mpr
    apply Nat.lt_trans (m:=size)
    · assumption
    · decide

  simp [
    Nat.mod_eq_of_lt lt₂,
    Nat.mod_eq_of_lt lt₃,
    Nat.mod_eq_of_lt lt₄,
    Nat.shiftLeft_toExp
  ]

  have lt₅ : 2^24 * (val / 2^24 % 256) < size := by
    apply Nat.lt_of_lt_of_le (m:= 2^24 * 256)
    · apply (Nat.mul_lt_mul_left (show 0 < 2^24 by decide)).mpr
      apply Nat.mod_lt; decide
    · decide

  have lt₆ : 2^16 * (val / 2^16 % 256) < size := by
    apply Nat.lt_of_lt_of_le (m:= 2^16 * 256)
    · apply (Nat.mul_lt_mul_left (show 0 < 2^16 by decide)).mpr
      apply Nat.mod_lt; decide
    · decide

  have lt₇ : 2^8 * (val / 2^8 % 256) < size := by
    apply Nat.lt_of_lt_of_le (m:= 2^8 * 256)
    · apply (Nat.mul_lt_mul_left (show 0 < 2^8 by decide)).mpr
      apply Nat.mod_lt; decide
    · decide

  simp [
    Nat.mod_eq_of_lt lt₅,
    Nat.mod_eq_of_lt lt₆,
    Nat.mod_eq_of_lt lt₇
  ]

  rw [show 256 = 2^8 by decide, ←Nat.shiftLeft_toExp, ←Nat.shiftRight_toDiv]


theorem UInt64.shiftUnshift : ∀(i  : UInt64),
  i = (i >>> 0x38 % 256) <<< 0x38 |||
      (i >>> 0x30 % 256) <<< 0x30 |||
      (i >>> 0x28 % 256) <<< 0x28 |||
      (i >>> 0x20 % 256) <<< 0x20 |||
      (i >>> 0x18 % 256) <<< 0x18 |||
      (i >>> 0x10 % 256) <<< 0x10 |||
      (i >>> 0x08 % 256) <<< 0x08 |||
      i % 256 := sorry

theorem UInt16.ByteArray_roundtrip :
  ∀ i : UInt16, ∀l, i.getBytesBEfrom.getUInt16BEfrom 0 l = i := by
  intro i l

  simp [
    UInt16.getBytesBEfrom,
    ByteArray.getUInt16BEfrom,
    ByteArray.get,
    Array.get,
    List.get,
    UInt16_to_UInt8_round,
    UInt16_to_UInt8_round,
    UInt16.nullShift,
  ]

  apply Eq.symm
  apply UInt16.shiftUnshift i
