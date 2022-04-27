open import Data.Nat
open import Data.List
open import Data.Sum
open import Data.Product
open import Data.Maybe
open import Data.Unit
open import Data.Bool
open import Relation.Binary.PropositionalEquality

data Type : Set where
    _⇒_ : Type → Type → Type
    Nat : Type
    Boo : Type
    Hole : Type

Ctx = List Type

data Var : Ctx → Type → Set where
    same : ∀{Γ T} → Var (T ∷ Γ) T
    next : ∀{Γ T A} → Var Γ T → Var (A ∷ Γ) T

data Term : Ctx → Type → Set where
    var : ∀{Γ T} → Var Γ T → Term Γ T
    app : ∀{Γ A B} → Term Γ (A ⇒ B) → Term Γ A → Term Γ B
    lambda : ∀{Γ A B} → Term (A ∷ Γ) B → Term Γ (A ⇒ B)
    hole : ∀{Γ} → (T : Type) → Term Γ T

-- TypeChange
data _~>_ : Type → Type → Set where
  NoChange : ∀{T} → T ~> T
  _⇒_ : ∀{A A' B B'}
    → A ~> A' → B ~> B' → (A ⇒ B) ~> (A' ⇒ B')
  Insert : ∀{T} → (A : Type) → T ~> (A ⇒ T)
  Remove : ∀{A T} → (A ⇒ T) ~> T
  Dig : ∀{T} → T ~> Hole

-- data Changes : Ctx → Ctx → Set where
Changes : Ctx → Ctx → Set
Changes Γ₁ Γ₂ = ∀{A}
  → Var Γ₁ A
  → (Σ[ B ∈ Type ] (A ~> B × Var Γ₂ B)) ⊎ ⊤

isNoChange : ∀{A B} → A ~> B → Maybe (A ≡ B)
isNoChange NoChange = just refl
isNoChange (c₁ ⇒ c₂) with isNoChange c₁ | isNoChange c₂
... | just refl | just refl = just refl
... | _ | _ = nothing
isNoChange (Insert A) = nothing
isNoChange Remove = nothing
isNoChange Dig = nothing

tyEq : (A B : Type) → Maybe (A ≡ B)
tyEq = {!   !}

mutual
  applyChange : ∀{Γ₁ Γ₂ A B}
    → Term Γ₁ A → A ~> B → Changes Γ₁ Γ₂ → Term Γ₂ B
  applyChange t (Insert A) chs = lambda (applyChange t NoChange {! chs  !} )
  applyChange t Dig chs = hole Hole
  applyChange (lambda t) NoChange chs = lambda (applyChange t NoChange {! chs  !})
  applyChange (lambda t) (c₁ ⇒ c₂) chs = lambda (applyChange t c₂ {! ? chs c₁  !})
  applyChange (lambda t) Remove chs = applyChange t NoChange {! ? chs  !}
  applyChange {B = B} (hole T) c chs = hole B
  applyChange {B = B} t c chs with inferChange t chs
  ... | (T , tc , t') with tyEq T B
  ...                    | nothing = hole B -- Displace~~~~!!!!
  ...                    | just refl = t' -- Displace~~~~!!!!
  -- applyChange {B = B} (var x) c chs with chs x
  -- ... | inj₂ tt = hole B
  -- ... | inj₁ (T , tc , x') with tyEq T B
  -- ... |                       nothing = hole B -- displace x also????????
  -- ... |                       just refl = var x'
  -- applyChange (app t₁ t₂) c chs with inferChange t₁ chs
  -- ... | .(_ ⇒ _) , NoChange , t' = {!   !}
  -- ... | .(_ ⇒ _) , (tc ⇒ tc₁) , t' = {!   !}
  -- ... | .(A ⇒ (_ ⇒ _)) , Insert A , t' = {!   !}
  -- ... | T , Remove , t' = {!   !}
  -- ... | .Hole , Dig , t' = {!   !}

  inferChange : ∀{Γ₁ Γ₂ A}
    → Term Γ₁ A → Changes Γ₁ Γ₂ → (∃[ B ] (A ~> B × Term Γ₂ B))
  inferChange {A = A} (var x) chs with chs x
  ... | inj₁ (T , tc , x') = T , tc , var x'
  ... | inj₂ tt = A , NoChange , hole A
  inferChange (app t₁ t₂) chs = {!   !}
  inferChange (lambda t) chs = {!   !}
  inferChange (hole _) chs = {!   !}


{-
  x : ?

  f : Bool
  f = ?
-}
