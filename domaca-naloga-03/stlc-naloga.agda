data Ty : Set where
    BOOL : Ty
    _⇒_ : Ty → Ty → Ty
    _×_ : Ty → Ty → Ty
    -- Dodamo nov tip za sezname
    [_]t : Ty → Ty -- Da se izognemo težavam pri precedenci _[_]
    -- In variantne tipe
    _+_ : Ty → Ty → Ty


data Ctx : Set where
    ∅ : Ctx
    _,_ : Ctx → Ty → Ctx

data _∈_ : Ty → Ctx → Set where
    Z : {A : Ty} {Γ : Ctx} → A ∈ (Γ , A)
    S : {A B : Ty} {Γ : Ctx} → A ∈ Γ → A ∈ (Γ , B)

data _⊢_ : Ctx → Ty → Set where

    VAR : {Γ : Ctx} {A : Ty} →
        A ∈ Γ →
        -----
        Γ ⊢ A

    TRUE : {Γ : Ctx} →
        --------
        Γ ⊢ BOOL

    FALSE : {Γ : Ctx} →
        --------
        Γ ⊢ BOOL

    IF_THEN_ELSE_ : {Γ : Ctx} {A : Ty} →
        Γ ⊢ BOOL →
        Γ ⊢ A →
        Γ ⊢ A →
        -----
        Γ ⊢ A

    _∙_ : {Γ : Ctx} {A B : Ty} →
        Γ ⊢ (A ⇒ B) →
        Γ ⊢ A →
        -----
        Γ ⊢ B

    ƛ : {Γ : Ctx} {A B : Ty} →
        (Γ , A) ⊢ B →
        -----------
        Γ ⊢ (A ⇒ B)

    ⟨_,_⟩ : {Γ : Ctx} {A B : Ty} →
        Γ ⊢ A →
        Γ ⊢ B →
        -----------
        Γ ⊢ (A × B)
    
    FST : {Γ : Ctx} {A B : Ty} →
        Γ ⊢ (A × B) →
        -----
        Γ ⊢ A

    SND : {Γ : Ctx} {A B : Ty} →
        Γ ⊢ (A × B) →
        -----
        Γ ⊢ B

    -- Seznami
    [] : {Γ : Ctx} {A : Ty} → 
        Γ ⊢ [ A ]t
    
    -- Simbol: \:: = ∷
    _∷_ : {Γ : Ctx} {A : Ty} →
        Γ ⊢ A → 
        Γ ⊢ [ A ]t → 
        ----
        Γ ⊢ [ A ]t
    
    -- Simbol: \mapsto = ↦
    -- Ker so sedaj spremenljivke poimenovane z indeksi je
    -- tudi match nekoliko drugačen
    MATCH_WITH[]↦_∷↦_ : {Γ : Ctx} {A B : Ty} →
        Γ ⊢ [ A ]t → 
        Γ ⊢ B → 
        ((Γ , A) , [ A ]t ) ⊢ B →
        ----
        Γ ⊢ B
    
    INL_ : {Γ : Ctx} {A B : Ty} →
        Γ ⊢ A →
        ----
        Γ ⊢ (A + B)

    INR_ : {Γ : Ctx} {A B : Ty} →
        Γ ⊢ B →
        ----
        Γ ⊢ (A + B)
    
    MATCH_INL↦_INR↦_ : {Γ : Ctx} {A B C : Ty} → 
        Γ ⊢ (A + B) → 
        (Γ , A) ⊢ C → 
        (Γ , B) ⊢ C →
        ----
        Γ ⊢ C

extend-renaming : {Γ Δ : Ctx}
  → ({A : Ty} → A ∈ Γ → A ∈ Δ)
    --------------------------------------
  → {A B : Ty} → A ∈ (Γ , B) → A ∈ (Δ , B)
extend-renaming ρ Z = Z
extend-renaming ρ (S x) = S (ρ x)

rename : {Γ Δ : Ctx}
  → ({A : Ty} → A ∈ Γ → A ∈ Δ)
    -------------------------
  → {A : Ty} → Γ ⊢ A → Δ ⊢ A
rename ρ (VAR x) = VAR (ρ x)
rename ρ TRUE = TRUE
rename ρ FALSE = FALSE
rename ρ (IF M THEN N₁ ELSE N₂) = 
    IF (rename ρ M) THEN (rename ρ N₁) ELSE (rename ρ N₂)
rename ρ (M ∙ N) = rename ρ M ∙ rename ρ N
rename ρ (ƛ M) = ƛ (rename (extend-renaming ρ) M)
rename ρ ⟨ M , N ⟩ = ⟨ rename ρ M , rename ρ N ⟩
rename ρ (FST M) = FST (rename ρ M)
rename ρ (SND M) = SND (rename ρ M)
rename ρ _ = {!   !}

extend-subst : {Γ Δ : Ctx}
  → ({A : Ty} → A ∈ Γ → Δ ⊢ A)
    ---------------------------------------
  → {A B : Ty} → A ∈ (Γ , B) → (Δ , B) ⊢ A
extend-subst σ Z = VAR Z
extend-subst σ (S x) = rename S (σ x)

subst : {Γ Δ : Ctx}
  → ({A : Ty} → A ∈ Γ → Δ ⊢ A)
    -------------------------
  → {A : Ty} → Γ ⊢ A → Δ ⊢ A
subst σ (VAR x) = σ x
subst σ TRUE = TRUE
subst σ FALSE = FALSE
subst σ (IF M THEN N₁ ELSE N₂) =
    IF (subst σ M) THEN (subst σ N₁) ELSE (subst σ N₂)
subst σ (M ∙ N) = subst σ M ∙ subst σ N
subst σ (ƛ M) = ƛ (subst (extend-subst σ) M)
subst σ ⟨ M , N ⟩ = ⟨ subst σ M , subst σ N ⟩
subst σ (FST M) = FST (subst σ M)
subst σ (SND M) = SND (subst σ M)
subst σ _ = {!   !}

_[_] : {Γ : Ctx} {A B : Ty}
  → (Γ , B) ⊢ A
  → Γ ⊢ B
    -----
  → Γ ⊢ A
_[_] {Γ} {B = B} N M = subst σ N
  where
  σ : ∀ {A : Ty} → A ∈ (Γ , B) → Γ ⊢ A
  σ Z = M
  σ (S x) = VAR x

-- Za evalvacijo seznamov potrebujemo dvojno hkratno substitucijo
_[_][_] : {Γ : Ctx } {A B C : Ty}
  → ((Γ , A) , B) ⊢ C
  → Γ ⊢ A
  → Γ ⊢ B
    -------------
  → Γ ⊢ C
_[_][_] {Γ} {A} {B} N V W =  subst σ N
  where
  σ : ∀ {C} → C ∈ ((Γ , A) , B) → Γ ⊢ C
  σ Z          =  W
  σ (S Z)      =  V
  σ (S (S x))  =  VAR x


data value : {Γ : Ctx} {A : Ty} → Γ ⊢ A → Set where
    value-TRUE : {Γ : Ctx} →
        ----------------
        value (TRUE {Γ})
    value-FALSE : {Γ : Ctx} →
        -----------------
        value (FALSE {Γ})
    value-LAMBDA : {Γ : Ctx} {A B : Ty} {M : (Γ , A) ⊢ B} →
        -----------
        value (ƛ M)
    value-PAIR : {Γ : Ctx} {A B : Ty} {M : Γ ⊢ A} {N : Γ ⊢ B} →
        value M →
        value N →
        -----------
        value ⟨ M , N ⟩

    value-NIL : {Γ : Ctx} {A : Ty} →
        value ([] {Γ} {A})
    value-CONS : {Γ : Ctx} {A : Ty} {M : Γ ⊢ A} {N : Γ ⊢ [ A ]t } →
        value M →
        value N →
        -----------
        value (M ∷ N)

    value-INL : {Γ : Ctx} {A B : Ty} {M : Γ ⊢ A} →
        value M → 
        -----------
        value (INL_ {B = B} M )
    value-INR : {Γ : Ctx} {A B : Ty} {M : Γ ⊢ B} →
        value M → 
        -----------
        value (INR_ {A = A} M)


data _↝_ : {Γ : Ctx} {A : Ty} → Γ ⊢ A → Γ ⊢ A → Set where
    IF-TRUE : {Γ : Ctx} {A : Ty} {M₁ M₂ : Γ ⊢ A} →
        ------------------------------
        (IF TRUE THEN M₁ ELSE M₂) ↝ M₁
    IF-FALSE : {Γ : Ctx} {A : Ty} {M₁ M₂ : Γ ⊢ A} →
        ------------------------------
        (IF FALSE THEN M₁ ELSE M₂) ↝ M₂
    IF-STEP : {Γ : Ctx} {A : Ty} {M M' : Γ ⊢ BOOL} {M₁ M₂ : Γ ⊢ A} →
        M ↝ M' →
        ------------------------------------------------
        (IF M THEN M₁ ELSE M₂) ↝ (IF M' THEN M₁ ELSE M₂)
    APP-STEP1 : {Γ : Ctx} {A B : Ty} {M M' : Γ ⊢ (A ⇒ B)} {N : Γ ⊢ A} →
        M ↝ M' →
        ------------------------------------------------
        (M ∙ N) ↝ (M' ∙ N)
    APP-STEP2 : {Γ : Ctx} {A B : Ty} {M : Γ ⊢ (A ⇒ B)} {N N' : Γ ⊢ A} →
        value M →
        N ↝ N' →
        ------------------------------------------------
        (M ∙ N) ↝ (M ∙ N')
    APP-BETA : {Γ : Ctx} {A B : Ty} {M : (Γ , A) ⊢ B} {N : Γ   ⊢ A} →
        value N →
        ------------------------------------------------
        ((ƛ M) ∙ N) ↝ ( M [ N ])
    PAIR-STEP1 : {Γ : Ctx} {A B : Ty} {M M' : Γ ⊢ A} {N : Γ   ⊢ B} →
        M ↝ M' →
        ------------------------------------------------
      ⟨ M , N ⟩ ↝ ⟨ M' , N ⟩
    PAIR-STEP2 : {Γ : Ctx} {A B : Ty} {M : Γ ⊢ A} {N N' : Γ   ⊢ B} →
        value M →
        N ↝ N' →
        ------------------------------------------------
        ⟨ M , N ⟩ ↝ ⟨ M , N' ⟩
    FST-STEP : {Γ : Ctx} {A B : Ty} {M M' : Γ ⊢ (A × B)} →
        M ↝ M' →
        ------------------------------------------------
        (FST M) ↝ (FST M')
    FST-BETA : {Γ : Ctx} {A B : Ty} {M : Γ ⊢ A} {N : Γ   ⊢ B} →
        value M →
        value N →
        ------------------------------------------------
        FST ⟨ M , N ⟩ ↝ M
    SND-STEP : {Γ : Ctx} {A B : Ty} {M M' : Γ ⊢ (A × B)} →
        M ↝ M' →
        ------------------------------------------------
        (SND M) ↝ (SND M')
    SND-BETA : {Γ : Ctx} {A B : Ty} {M : Γ ⊢ A} {N : Γ   ⊢ B} →
        value M →
        value N →
        ------------------------------------------------
        SND ⟨ M , N ⟩ ↝ N

    --     
    LIST-STEP1 : {Γ : Ctx} {A : Ty} {M M' : Γ ⊢ A} {N : Γ ⊢ [ A ]t } →
        M ↝ M' →
        ------------------------------------------------
        (M ∷ N) ↝ (M' ∷ N)
    LIST-STEP2 : {!   !}

    LIST-MATCH-STEP : {Γ : Ctx} {A B : Ty} {M M' : Γ ⊢ [ A ]t } {N₁ : Γ ⊢ B} {N₂ : ((Γ , A) , [ A ]t ) ⊢ B } →
        M ↝ M' →
        ------------------------------------------------
        (MATCH M WITH[]↦ N₁ ∷↦ N₂) ↝ {!   !}
    
    LIST-MATCH-NIL-BETA : {Γ : Ctx} {A B : Ty} {N₁ : Γ ⊢ B} 
            {N₂ : ((Γ , A) , [ A ]t ) ⊢ B } →
        ------------------------------------------------
        (MATCH [] WITH[]↦ N₁ ∷↦ N₂) ↝ N₁
    LIST-MATCH-CONS-BETA : 
            {Γ : Ctx} {A B : Ty} {M₁ : Γ ⊢ A } 
            {M₂ : Γ ⊢ [ A ]t } {N₁ : Γ ⊢ B} 
            {N₂ : ((Γ , A) , [ A ]t ) ⊢ B } →
        -- Namiga:
        --  - Razmislite, ali potrebujete še kakšne dodatne informacije za M₁ in M₂.
        --  - Za substitucijo uporabite _[_][_].
        {!   !}  

    INL-STEP : {Γ : Ctx} {A B : Ty} {M M' : Γ ⊢ A} → 
        M ↝ M' → 
        ------------------------------------------------
        (INL_ {B = B} M) ↝ (INL M')

    INR-STEP : {Γ : Ctx} {A B : Ty} {M M' : Γ ⊢ B} → 
        M ↝ M' → 
        ------------------------------------------------
        (INR_ {A = A} M) ↝ (INR M')
    
    VARIANT-MATCH-STEP : 
        {!   !}
    
    VARIANT-MATCH-BETA-INL : {Γ : Ctx} {A B C : Ty} {M : Γ ⊢ A } {N₁ : (Γ , A) ⊢ C} {N₂ : (Γ , B) ⊢ C } →
        value M →
        ------------------------------------------------
        (MATCH (INL M) INL↦ N₁ INR↦ N₂) ↝ (N₁ [ M ])
    VARIANT-MATCH-BETA-INR : {!   !}

data progresses : {A : Ty} → ∅ ⊢ A → Set where
    is-value : {A : Ty} {M : ∅ ⊢ A} →
        value M →
        ------------
        progresses M
    steps : {A : Ty} {M M' : ∅ ⊢ A} →
        M ↝ M' →
        ------------
        progresses M

progress : {A : Ty} → (M : ∅ ⊢ A) → progresses M

progress TRUE = is-value value-TRUE
progress FALSE = is-value value-FALSE
progress (IF M THEN N₁ ELSE N₂) with progress M
... | is-value value-TRUE = steps IF-TRUE
... | is-value value-FALSE = steps IF-FALSE
... | steps M↝M' = steps (IF-STEP M↝M')
progress (M ∙ N) with progress M
... | steps M↝M' = steps (APP-STEP1 M↝M')
... | is-value value-LAMBDA with progress N
...     | is-value V = steps (APP-BETA V)
...     | steps N↝N' = steps (APP-STEP2 value-LAMBDA N↝N')
progress (ƛ M) = is-value value-LAMBDA
progress ⟨ M , N ⟩ with progress M
... | steps M↝M' = steps (PAIR-STEP1 M↝M')
... | is-value V with progress N
...     | is-value W = is-value (value-PAIR V W)
...     | steps N↝N' = steps (PAIR-STEP2 V N↝N')
progress (FST M) with progress M
... | is-value (value-PAIR V W) = steps (FST-BETA V W)
... | steps M↝M' = steps (FST-STEP M↝M')
progress (SND M) with progress M
... | is-value (value-PAIR V W) = steps (SND-BETA V W)
... | steps M↝M' = steps (SND-STEP M↝M')
progress _ = {!   !}

