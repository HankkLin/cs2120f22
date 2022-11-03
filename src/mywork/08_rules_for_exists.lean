section pred_logic

variables X Y Z : Prop

/-
Quick review of predicates. 

A predicate is a proposition with one or more parameters.
A proposition is a predicate with no remaining parameters!

You can think of a predicate it as a function that takes
one or more arguments and that reduces to a proposition
*about those particular values*. 

Here, for example, we define a predicate, called isEven,
that takes a natural number, n, as an argument and that
reduces to ("returns") the proposition, n % 2 = 0, *for
that particular n*.
-/

def isEven : ℕ → Prop :=
begin
assume n,
exact (n%2 = 0)
end 

/-
In fact, in Lean and similar logical programming systems,
a predicate *is* a function, and can thus be applied to an
argument of the specified type.
-/

#reduce isEven 0      -- 0 = 0
#reduce isEven 1      -- 1 = 0
#reduce isEven 2      -- 0 = 0
#reduce isEven 3      -- 1 = 0

/-
Note that the n%2 expression is evaluated automatically.
-/

/-
We will say that one or more values "satisfy" a predicate
when the corresponding proposition is true. In constructive
logic, that means when there's a proof of that proposition.
-/

example : isEven 0 :=
begin
simp [isEven],  -- new tactic: simplify by def'n of isEven
exact rfl,      -- forces reduction, tests equality
-- Yay! 0 is even
end

example : isEven 1 :=
begin
exact rfl,      -- no need for simp, no proof of 1=0
-- Ooooh, 1 is not even
end

-- In fact we can prove the negation
example : ¬isEven 1 :=
begin
assume h,
simp [isEven] at h, -- more tactic fun
cases h,            -- no proofs of h so done
-- Yay! 1 is *not* even (proof by negation)
end

example : isEven 2 :=
begin
exact rfl,
-- Yay! 2 is even.
end


/-
Now let's see syntax alternatives for defining
functions in Lean. We'll stick with the same
"predicate function", giving different names to
avoid conflicts.
-/

/- 
You can use tactic scripts, but you can also 
write exact proof terms. However, in the case
where the value being assigned to an identifier
is a function value, you will use a so-called
"lambda" or "function" expression.
-/

def isEven1 : ℕ → Prop := fun n, n % 2 = 0
def isEven2 : ℕ → Prop := λ n,   n % 2 = 0 

/-
You can pronounce the terms to the right of 
the := as "a function that takes an argument,
n, and returns, (the proposition) n % 2 := 0."
You can add type judgments either for clarity
or if Lean can't infer them from the context.
def isEven : ℕ → Prop := λ (n : ℕ), n%2 = 0.

The fundamental purpose of the λ/fun keyword is
to *bind names to arguments* so that they can be
used in the body/definition of the function. In
this case, we use λ/fun to bind the name n to 
the actual parameter value when this function is
applied to some argument. All of the n's in the
definition are then replaced by that value, and
the resuling expression is reduced to produce a
final result.  

The fun and λ keywords are exactly equivalent.
the use of "lambda" notation goes way back to 
the early days of CS. A key insight that you 
should take away is that "functions are values
too," and a "lambda expression" is a constant,
or literal value, the type of which is just a
function of some kind.
-/

/-
In Lean, you can move argument declarations to
the left of the colon and give them names there,
just as you would in Java or Python.
-/

def isEven3 (n : ℕ) : Prop := n % 2 = 0

/-
And as usual, you can leave out type judgments
when Lean can infer them from context.
-/

def isEven4 (n) := n % 2 = 0 -- parens required

/-
Finally, in Lean, you can use a construct called
"pattern matching" to define functions "by cases."
Here's the syntax. We'll use this syntax quite a
bit going forward, so best to get used to it now. 
-/

def isEven5 : ℕ → Prop    -- NB: No := used here
| n := n % 2 = 0

/-
Here, the "n" is bound to any value of the argument
type, and is then used to define the "return value"
to the right of the :=. In general we can use this
method of function definition to define returns for
different values or combinations of argument values.
-/

def my_bool_and : bool → bool → bool 
| tt tt := tt
| ff ff := ff
| tt ff := ff
| ff tt := ff

def my_bool_or : bool → bool 
| tt := ff
| ff := tt

def my_bool_not2 : bool → bool → bool 
| ff ff := ff
| _ _ := tt
/-
Functions in Lean must be "total," which means that
they must be defined to return values of the right
types for *all* possible combinations of arguments.
Go ahead and add the missing cases, then apply your
function!
-/

#eval my_bool_and tt tt
#eval my_bool_and tt ff
#eval my_bool_and ff tt
#eval my_bool_and ff ff

example: my_bool_and tt tt = tt := rfl

/-
You should (almost must) use this "by cases" syntax
to define functions recursive functions. If you use
other syntax, you'll find that you won't be able to
have the function body call the function itself.
-/

def factorial' (n : ℕ) : ℕ :=
  if n = 0 
  then 1 
  else n * factorial' (n-1)      -- factorial not defined

def factorial : ℕ → ℕ           -- remember, no := here
| 0 := 1
| (n + 1) := (n + 1) * factorial n

#eval factorial 5

-- ∃
def exists_intro := ∀ {P : X → Prop} (w : X), P w → (∃ (x : X), P x) 
def exists_elim := ∀ {P : X → Prop}, (∃ (x : X), P x) → (∀ (x : X), P x → Y) → Y 

/-
That's it for the fundamental rules of higher-order predicate
logic. The constructive logic versions of the remaining inference
rules we saw in propositional logic are actually theorems, which
means that they can be proved using only the fundamental rules,
which we accept as axioms. An axiom is a proposition accepted as
true without a proof. The inference rules of a logic are accepted
as axioms. The truth of any other proposition in predicate logic
(the foundation for most of mathematics) is proved by applying 
fundamental axioms and previously proved theorems..  
-/


example
  (Person: Type)
  (loves:Person → Person → Prop)
  :
  (∃ (a : Person), ∀ (p: Person), loves p a) → (∀ (p: Person), ∃ (a : Person), loves p a)
  := 
begin
  assume h,
  cases h with someonePopular everyone_loves_person,
  assume any_person,
  apply exists.intro someonePopular _,
  /-show any_person love someonePopular-/
  apply everyone_loves_person any_person,  
end

/-
Translate to English:
If there exist a person that everyone loves, then everyone must love someone.
assume a person that everyone loves,LLenny, with that we can induce a proof that everyone loves Lenny.
Then assume there is a random person, Bruce. 
apply the proof that everyone loves Lenny to Bruce to get that that Bruce loves Lenny.
Therefore, we can use the proof that everyone loves lenny to induce that there must exist a person that Bruce loves
QED
-/

def id_string : String -> String
| s := s

def id_boolean: bool -> bool
| b := b

def id_T (T: Type): T → T
| t := t



example : ¬ negation tt tt := 
begin 
 --proof bby negation
 assume h, -- assume the hypothesis is true
  cases h,
end


def negation : bool → bool → Prop
| tt ff := true
| ff tt := true
| _ _ := false

example : negation ff tt :=
begin
 unfold negation
end

example : ¬ negation tt tt := 
begin 
 --proof by negation
 assume h, -- assume the hypothesis is true
  cases h,
end
----------------------------------------------------------------------------------------------
example: ∀ (b1 b2), negation b1 b2 ↔ bnot b1 = b2 :=
begin
  assume b1 b2,
  split,
  -- What should be our proof strategy from here?
  -- Exercise: complete this proof.
  intro h,
  cases b1,
  cases b2,
  cases h, -- ff ff
  exact rfl, -- ff tt

  cases b2, -- again cuz now is second part of b1
  exact rfl, -- tt ff
  cases h, -- tt tt
  -- first direction complee, now the opposite direction

  assume h,
  cases b1,
  cases b2,
  cases h, -- ff ff
  unfold negation, -- ff tt

  cases b2,
  unfold negation, -- tt ff
  cases h,

end 


def StringLength (n : ℕ )(s : string) : Prop :=  s.length = n

example : StringLength 0 "" := rfl
example : StringLength 3 "aBC" := rfl
example : StringLength 10 "ABCDEFGHIJ" := rfl


def string2len : string → nat → Prop := λ s n, s.length = n

example: string2len "Lean!" 5 :=
begin
unfold string2len,
exact rfl,
end
