import Lean
open Lean Parser Command Syntax

declare_syntax_cat enum_case
syntax atomic((docComment)? " | " ident " => " ident ", " term) : enum_case
syntax (docComment)? " | " ident " => " term : enum_case

private inductive EnumCaseValue
  | literalValue (value : Term)
  | parametricValue (value : Ident) (cond : Term)
  deriving Inhabited

private structure EnumCaseParsed where
  name : Ident
  value : EnumCaseValue
  comment : Lean.TSyntax `Lean.Parser.Command.docComment
  deriving Inhabited

private def makeComment (comment: String) : Lean.TSyntax `Lean.Parser.Command.docComment :=
  let commentLeader : Lean.Syntax := Lean.Syntax.atom SourceInfo.none "/--"
  let commentBody : Lean.Syntax := Lean.Syntax.atom SourceInfo.none s!"{comment} -/"
  Lean.TSyntax.mk $ Lean.Syntax.node SourceInfo.none `Lean.Parser.Command.docComment #[commentLeader, commentBody]

private def parseCase (case : TSyntax `enum_case) : EnumCaseParsed :=
  match case with
    | `(enum_case| | $name => $value) => ⟨name, .literalValue value, emptyComment⟩
    | `(enum_case| | $name => $value, $cond) => ⟨name, .parametricValue value cond, emptyComment⟩
    | `(enum_case| $comment:docComment | $name => $value) => ⟨name, .literalValue value, comment⟩
    | `(enum_case| $comment:docComment | $name => $value, $cond) => ⟨name, .parametricValue value cond, comment⟩
    | _ => unreachable!
  where emptyComment := makeComment "undocumented"

private def parseCases (cases : TSyntaxArray `enum_case) : Array EnumCaseParsed :=
  cases.foldl (init := (#[])) fun (parsedCases) => fun (case) =>
    parsedCases.push (parseCase case)

-- TODO: prove termination.
-- Warning: this is not performant if there are many name collisions.
private partial def getUniqueName (base : String) (names : List String) : String :=
  let rec loop (i : Nat) : String :=
    let name := base ++ (if i == 0 then "" else s!"_{i}")
    if name ∈ names then loop (i + 1) else name
  loop 0

private def addCaseDefaults (n: Ident) (cases : Array EnumCaseParsed) : MacroM (Array EnumCaseParsed) := do
  -- Allow the final case to omit the true condition.
  let ⟨name, value, comment⟩ := cases.back
  let trueValue ← `(true)
  let defaultValue := EnumCaseValue.parametricValue n trueValue
  let (cases, lastValue) := match value with
  | .literalValue v      =>
      if v == n then (cases.pop.push ⟨name, defaultValue, comment⟩, defaultValue)
      else (cases, value)
  | .parametricValue _ _ => (cases, value)

  -- Append a default case if the final case is not a catch-all.
  let names := cases.map (·.name.getId.toString)
  let addDefaultCase (n : Ident) (cases : Array EnumCaseParsed) : Array EnumCaseParsed :=
    let defaultName: Ident := mkIdentFrom n (getUniqueName "default" names.toList)
    cases.push ⟨defaultName, defaultValue, makeComment "default case"⟩
  let cases := match lastValue with
  | .literalValue _      => addDefaultCase n cases
  | .parametricValue _ h => if h == trueValue then cases else addDefaultCase n cases

  pure cases

private def validateCases (n: Ident) (cases : Array EnumCaseParsed) : MacroM Unit :=
  -- validate that parametric values use the same name as the enum value
  for case in cases do
    match case.value with
    | .literalValue _      => pure ()
    | .parametricValue v _ => if v != n then
        Macro.throwErrorAt v s!"parametric value {v.getId.toString} must match the enum parameter {n.getId.toString}"

/-
Make the constructor types for the inductive type that an enum desugars to.
Constructors for non-literal cases bind to a value and a proof that the value is
in the case's range. A case's range is limited by both its local condition and
the conditions of all previous cases (since cases are matched greedily). The net
restriction on a case is the conjunction of its local restriction and the
negations of all prior restrictions. A literal constructor's implied restriction
is that of equality with its literal value.

To make these constructor types, we fold over the cases, building the
constructor type for each case and the net restriction after that case.
-/
private def mkCtorTypes (e n : Ident) (ty : Term) (parsedCases : Array EnumCaseParsed) : MacroM (Array Term) := do
  let mkCtorTypeAndCarriedRestrictions (value : EnumCaseValue) (priorRestrictions : Term) : MacroM (Term × Term) := do
    let thisType ← match value with
      | .literalValue _      => `($e)
      | .parametricValue _ h => `(($n:ident: $ty) → (h: $h ∧ $priorRestrictions) → $e)
    let netRestrictions ← match value with
      | .literalValue v      => `(¬($n:ident = $v) ∧ $priorRestrictions)
      | .parametricValue _ h => `(¬($h) ∧ $priorRestrictions)
    pure ⟨thisType, netRestrictions⟩

  let ⟨ctorTypes, _⟩ ← parsedCases.foldlM (init := (#[], ←`(true))) fun (valuesAcc, restrictions) (case) => do
    let (thisType, netRestrictions) ← mkCtorTypeAndCarriedRestrictions case.value restrictions
    return (valuesAcc.push thisType, netRestrictions)

  pure ctorTypes

/--
The `enum` macro defines an enumeration type over a base type. The enumeration
type defines cases that partition the base type. Each case is associated with a
literal value or a guard condition on values. Cases are matched greedily.

An enum type defines constructors for each of its cases. Constructors for
guarded types take a value and a proof that the value is in the case's range. An
enum type also defines functions to convert between the enum type and values of
the base type, and defines proofs that a round-trip in either direction between
a value of the base type and its corresponding enum value is the identity.

An enum always includes a default case that matches any value not matched by
other cases. If a default case is not explicitly defined, it is added.

Example:
```
enum E (n: Nat)
  | one => 1
  | small => n, 2 ≤ n ∧ n < 4
  | four => 4
  | other => n

#eval E.four -- four
#eval E.ofVal 4 -- four
#eval E.four.val -- 4
#eval E.small 3 (by decide) -- small 3
#eval (E.small 3 (by decide)).val -- 3
#eval (E.ofVal 3).val -- 3
#check E.ofVal_val -- ∀ (e : E), E.ofVal (E.val e) = e
#check E.val_ofVal -- ∀ (n : Nat), E.val (E.ofVal n) = n
```
-/
macro "enum" e:ident "(" n:ident " : " ty:term ")" (" where")? cases:enum_case* : command => do
  -- Must have at least one case. Don't enforce this in the syntax parser since
  -- it's easier to give a good error message here.
  if cases.size == 0 then Macro.throwErrorAt e "enum must have at least one case"

  let eIdent (suffix : Name) := mkIdentFrom e (e.getId ++ suffix)
  let toStrLit (n : Name) := Lean.Syntax.mkStrLit n.toString

  let rawParsedCases := parseCases cases
  let parsedCases ← addCaseDefaults n rawParsedCases
  let _ ← validateCases n parsedCases

  let comments := parsedCases.map (·.comment)
  let names := parsedCases.map (·.name)
  let ctorTypes ← mkCtorTypes e n ty parsedCases

  let reprCases ← parsedCases.mapM fun ⟨name, value, _⟩ => do match value with
    | .literalValue _      => `(Term.matchAltExpr| | .$name:ident => $(toStrLit name.getId))
    | .parametricValue v _ => `(Term.matchAltExpr| | .$name:ident $v _ => s!"{$(toStrLit name.getId)} {$v}")

  let valCases ← parsedCases.mapM fun ⟨name, value, _⟩ => do match value with
    | .literalValue v      => `(Term.matchAltExpr| | .$name:ident => $v)
    | .parametricValue v _ => `(Term.matchAltExpr| | .$name:ident $v _ => $v)

  let ofValCases: Term ← parsedCases.reverse.foldlM (init := ←`(unreachable!)) fun res case => do
    let ⟨name, value, _⟩ := case
    match value with
    | .literalValue v      => `(if _ : $n = $v then .$name:ident else $res)
    | .parametricValue _ h => `(if _ : $h then .$name:ident $n (by simp; omega) else $res)

  `(
    inductive $e:ident
      $[$comments:docComment | $names:ident : $ctorTypes]*
      deriving Inhabited, BEq
    instance : Repr $e where reprPrec := fun e _ => match e with $reprCases:matchAlt*

    def $(eIdent `val) : $e → $ty $valCases:matchAlt*

    def $(eIdent `ofVal) : $ty → $e := fun $n => $ofValCases

    theorem $(eIdent `ofVal_val) (e : $e) : $(eIdent `ofVal) ($(eIdent `val) e) = e := by
      cases e <;> try rfl
      all_goals rename _ => h; simp [$(eIdent `val): ident, $(eIdent `ofVal): ident, h]

    theorem $(eIdent `val_ofVal) (n : $ty) : $(eIdent `val) ($(eIdent `ofVal) n) = n := by
      unfold $(eIdent `ofVal)
      repeat split; simp only [*, $(eIdent `val): ident]
      simp only [*, $(eIdent `val): ident]
      contradiction
  )
