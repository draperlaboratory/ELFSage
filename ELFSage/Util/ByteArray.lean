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

def NByteArray.get {n : Nat} (bs : NByteArray n) (m: Nat) (p: m < n) :=
  have size := bs.sized
  bs.bytes.get ⟨m, by omega⟩

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
   List.length (Array.extract.loop src b l dst).toList = b + dst.size:= by
   intro b
   induction b
   case zero => simp [Array.extract.loop]
   case succ n ih =>
     intro l dst
     unfold Array.extract.loop
     intro lt
     simp; split
     · have : n + l + 1 ≤ size src := by omega
       simp only [Array.toList_length] at ih
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
        simp only [Array.append_toList, Array.size, List.length_append, implies_true]
      rw [this, this]
      simp
      have : bs.data.size = bs.size := by
        simp [ByteArray.size]
      have :  (min n (List.length bs.toList)) + 0 ≤ bs.data.size := by
        rw [Nat.min_def]
        split <;> omega
      rw [Array.extract_loop_len (min n bs.data.size) 0 #[]]
      simp only [ByteArray.size, Array.size,
                 ge_iff_le, Array.append_toList,
                 List.length_append, implies_true,
                 Nat.min_def, Nat.zero_add,
                 Array.toList_toArray, List.length_nil,
                 Nat.add_zero, ite_eq_left_iff, Nat.not_le]
        at *
      · omega
      · simp [Nat.min_def]; split
        · assumption
        · simp only [ByteArray.size, Array.size,
                     ge_iff_le, Array.append_toList,
                     List.length_append, implies_true,
                     Nat.min_def, Nat.zero_add,
                     Nat.not_le, Nat.le_refl]
            at *
    ⟨bytes, proof⟩

theorem UInt16.ByteArray_size : ∀ i : UInt16, i.getBytesBEfrom.size = 2 := by
  intro i
  simp [UInt16.getBytesBEfrom, ByteArray.size]

theorem UInt8_to_UInt16_round: ∀{i : UInt8}, i.toUInt16.toUInt8 = i := by
  rintro ⟨i⟩
  simp [UInt16.toUInt8, Nat.toUInt8, UInt8.ofNat]
  apply UInt8.eq_of_val_eq
  simp
  rcases i with ⟨val, lt⟩
  apply Fin.eq_of_val_eq
  simp [Fin.ofNat, UInt16.toNat, UInt8.toUInt16, Nat.toUInt16, UInt16.ofNat, UInt8.toNat]
  unfold UInt8.size at lt
  omega

theorem UInt16_to_UInt8_round: ∀{i : UInt16}, i.toUInt8.toUInt16 = i % 256:= by
  rintro ⟨i⟩
  simp [UInt8.toUInt16, Nat.toUInt16, UInt16.ofNat, HMod.hMod, instHMod, Mod.mod, UInt16.mod]
  apply UInt16.eq_of_val_eq
  simp only [UInt16.toUInt8, Nat.toUInt8, UInt8.ofNat, Fin.ofNat, Nat.succ_eq_add_one,
    Nat.reduceAdd, UInt16.toNat, Fin.mod, Nat.reduceMod]

theorem Nat.bitwise_sum : ∀{n m k : Nat}, m % 2^n = 0 → k < 2^n → m ||| k = m + k := by
  intro n m k eq lt
  have : 2^n * (m / 2^n) = m := by
    apply Nat.mul_div_cancel'
    exact Nat.dvd_of_mod_eq_zero eq
  rw [←this]
  rw [Nat.mul_add_lt_is_or]
  assumption

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
  intro i; lower_to_nat i; apply Nat.mod_eq_of_lt; assumption

@[simp]
theorem Nat.bitwise_zero_left : bitwise f 0 m = if f false true then m else 0 := by
  simp only [bitwise, ↓reduceIte]

@[simp]
theorem Nat.bitwise_zero_right : bitwise f n 0 = if f true false then n else 0 := by
  unfold bitwise
  simp only [ite_self, decide_False, Nat.zero_div, ite_true, ite_eq_right_iff]
  rintro ⟨⟩
  split <;> rfl

theorem Nat.shiftLeft_toExp : x <<< n = 2^n * x := by
  rw [Nat.mul_comm]; apply Nat.shiftLeft_eq

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
  all_goals try omega
  (conv => rhs; unfold bitwise)
  simp_all only [ite_false]
  apply Nat.two_mul

theorem Nat.mod_pow_zero : n % m^(succ k) = 0 → n % m = 0 := by
  intro hyp
  rw [Nat.pow_succ] at hyp
  have : n % (m ^ k * m) % m = 0 % m := by rw [hyp]
  simp [Nat.mod_mul_left_mod n (m^k) m] at this
  assumption

theorem Nat.zero_lt_pow : 0 < n → 0 < n^m := by
  intro hyp; induction m
  case zero => simp
  case succ m ih =>
    simp [Nat.pow_succ]
    have := (Nat.mul_lt_mul_right hyp).mpr ih
    simp at this
    assumption

theorem Nat.mod_pow_lt_inner : ∀{l m n k : Nat}, l ≤ m → n % k^m % k^l = n % k^l := by
  intro l m n k hyp; apply Nat.mod_mod_of_dvd; apply Nat.pow_dvd_pow; assumption

theorem Nat.mod_pow_lt_outer : ∀{l m n k : Nat}, 1 < k → l ≤ m → n % k^l % k^m = n % k^l := by
  intro l m n k hyp₁ hyp₂
  cases Nat.le_iff_lt_or_eq.mp hyp₂
  case inl hyp₂ =>
    apply Nat.mod_eq_of_lt
    apply Nat.lt_trans (m := k ^ l)
    · apply Nat.mod_lt; apply Nat.zero_lt_pow; omega
    · apply Nat.pow_lt_pow_of_lt hyp₁; assumption
  case inr hyp₂ => rw [hyp₂, Nat.mod_mod]

theorem Nat.shiftLeft_mod : w + m ≥ k → (n % 2^w) <<< m % 2^k = n <<< m % 2^k := by
  simp_all [Nat.shiftLeft_toExp]
  revert m
  induction k <;> intro m
  case zero => simp [Nat.mod_one]
  case succ k ih_k =>
    intro hyp
    cases m
    case zero => simp_all; exact Nat.mod_pow_lt_inner hyp
    case succ m =>
      simp [Nat.pow_succ]
      suffices 2 * 2 ^ m  * (n % 2 ^ w) % (2 * 2 ^ k) = 2 * 2 ^ m * n % (2 * 2 ^ k) by
        simp_all [Nat.mul_comm]
      suffices 2 * (2 ^ m  * (n % 2 ^ w)) % (2 * 2 ^ k) = 2 * (2 ^ m * n) % (2 * 2 ^ k) by
        simp_all [Nat.mul_assoc]
      simp [Nat.mul_mod_mul_left]
      rw [ih_k]
      simp_arith at hyp
      assumption

--TODO: this should generalize to arbitrary bitwise ops.
theorem Nat.bitwise_mod : Nat.bitwise or m n % 2^k = Nat.bitwise or (m % 2^k) (n % 2^k) := by
  apply Nat.eq_of_testBit_eq
  intro i
  simp only [testBit_mod_two_pow]
  have h_m := @Nat.mod_lt m (2^k) (by exact Nat.two_pow_pos k)
  have h_n := @Nat.mod_lt n (2^k) (by exact Nat.two_pow_pos k)
  by_cases h : i < k
  case pos =>
    simp only [h, decide_True, Bool.true_and]
    rw [@testBit_bitwise or (by simp only [Bool.or_self]) (m % 2^k) (n % 2^k) i]
    rw [@testBit_bitwise or (by simp only [Bool.or_self]) m n i]
    simp only [testBit_mod_two_pow, h, decide_True, Bool.true_and]
  case neg =>
    simp only [h, decide_False, Bool.false_and, Bool.false_eq]
    have h_k_i : 2^k <= 2^i := by
      apply Nat.pow_le_pow_of_le (by omega) (by omega)
    generalize h_x : (bitwise or (m % 2 ^ k) (n % 2 ^ k)) = x
    have h_lt := @bitwise_lt_two_pow (m % 2^k) k (n % 2^k) or h_m h_n
    apply testBit_lt_two_pow (by omega)

theorem Nat.bitwise_or_mod : (m ||| n) % 2^k = m % 2^k ||| n % 2^k := by
  simp [HOr.hOr]; apply Nat.bitwise_mod

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
  intro n
  suffices 256 * (n / 256) ||| n % 256 = 256 * (n / 256) + n % 256 by
    simp [Nat.shiftLeft_toExp, Nat.shiftRight_toDiv]
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
    show ∀x y, Nat.mod x y = x % y by intro x y; rfl,
  ]
  rw [
    show 256 = 2^8 by decide,
    show size = 2^16 by decide,
    Nat.shiftLeft_mod (show 8 + 8 ≥ 16 by decide),
    Nat.shiftLeft_mod (show 16 + 8 ≥ 16 by decide),
    ←Nat.mod_pow_lt_outer (show 2 > 1 by decide) (show 16 ≥ 8 by decide),
    ←Nat.bitwise_or_mod,
    ←Nat.splitBytes,
    Nat.mod_mod
  ]
  apply Eq.symm
  apply Nat.mod_eq_of_lt
  assumption

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
    show ∀x y, Nat.mod x y = x % y by intro x y; rfl,
  ]
  rw [show 256 = 2^8 by decide]
  rw [show size = 2^32 by decide]
  repeat rw [Nat.mod_pow_lt_inner]
  all_goals try decide
  suffices
    val = (val >>> 24 % 2^8) <<< 24 |||
          (val >>> 16 % 2^8) <<< 16 |||
          (val >>> 8 % 2^8)  <<< 8  |||
          val % 2^8
  by
    have eq : val % 2^32 = val % 2^32 := by rfl
    conv at eq => rhs; rw [this]
    conv at eq => lhs; rw [Nat.mod_eq_of_lt lt₁]
    repeat rw [Nat.bitwise_or_mod]
    rw [←Nat.mod_mod, ←Nat.mod_mod, ←Nat.mod_mod] at eq
    repeat rw [Nat.bitwise_or_mod] at eq
    repeat rw [Nat.mod_mod] at eq
    repeat rw [Nat.mod_mod]
    exact eq
  have : val >>> 24 % 2 ^ 8 = val >>> 24 := by
    apply Nat.mod_eq_of_lt
    apply Nat.lt_of_lt_of_le (m:= size >>> 24)
    · repeat rw [Nat.shiftRight_toDiv]
      apply Nat.div_lt_of_lt_mul
      apply Nat.lt_of_lt_of_le (m:= size)
      · assumption
      · rw [Nat.mul_comm, Nat.div_mul_cancel] <;> simp_arith
    · simp_arith
  rw [
    this,
    Nat.shiftRight_add val 16 8,
    Nat.shiftLeft_add _ 8 16,
    ←Nat.shiftLeft_distribute,
    ←Nat.splitBytes,
    Nat.shiftRight_add val 8 8,
    Nat.shiftLeft_add _ 8 8,
    ←Nat.shiftLeft_distribute,
    ←Nat.splitBytes,
    ←Nat.splitBytes
  ]

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
