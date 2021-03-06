(*  Title:      HOL/ex/Reflection_Examples.thy
    Author:     Amine Chaieb, TU Muenchen
*)

section \<open>Examples for generic reflection and reification\<close>

theory Reflection_Examples
imports Complex_Main "HOL-Library.Reflection"
begin

text \<open>This theory presents two methods: reify and reflection\<close>

text \<open>
Consider an HOL type \<open>\<sigma>\<close>, the structure of which is not recongnisable
on the theory level.  This is the case of \<^typ>\<open>bool\<close>, arithmetical terms such as \<^typ>\<open>int\<close>,
\<^typ>\<open>real\<close> etc \dots  In order to implement a simplification on terms of type \<open>\<sigma>\<close> we
often need its structure.  Traditionnaly such simplifications are written in ML,
proofs are synthesized.

An other strategy is to declare an HOL datatype \<open>\<tau>\<close> and an HOL function (the
interpretation) that maps elements of \<open>\<tau>\<close> to elements of \<open>\<sigma>\<close>.

The functionality of \<open>reify\<close> then is, given a term \<open>t\<close> of type \<open>\<sigma>\<close>,
to compute a term \<open>s\<close> of type \<open>\<tau>\<close>.  For this it needs equations for the
interpretation.

N.B: All the interpretations supported by \<open>reify\<close> must have the type
\<open>'a list \<Rightarrow> \<tau> \<Rightarrow> \<sigma>\<close>.  The method \<open>reify\<close> can also be told which subterm
of the current subgoal should be reified.  The general call for \<open>reify\<close> is
\<open>reify eqs (t)\<close>, where \<open>eqs\<close> are the defining equations of the interpretation
and \<open>(t)\<close> is an optional parameter which specifies the subterm to which reification
should be applied to.  If \<open>(t)\<close> is abscent, \<open>reify\<close> tries to reify the whole
subgoal.

The method \<open>reflection\<close> uses \<open>reify\<close> and has a very similar signature:
\<open>reflection corr_thm eqs (t)\<close>.  Here again \<open>eqs\<close> and \<open>(t)\<close>
are as described above and \<open>corr_thm\<close> is a theorem proving
\<^prop>\<open>I vs (f t) = I vs t\<close>.  We assume that \<open>I\<close> is the interpretation
and \<open>f\<close> is some useful and executable simplification of type \<open>\<tau> \<Rightarrow> \<tau>\<close>.
The method \<open>reflection\<close> applies reification and hence the theorem \<^prop>\<open>t = I xs s\<close>
and hence using \<open>corr_thm\<close> derives \<^prop>\<open>t = I xs (f s)\<close>.  It then uses
normalization by equational rewriting to prove \<^prop>\<open>f s = s'\<close> which almost finishes
the proof of \<^prop>\<open>t = t'\<close> where \<^prop>\<open>I xs s' = t'\<close>.
\<close>

text \<open>Example 1 : Propositional formulae and NNF.\<close>
text \<open>The type \<open>fm\<close> represents simple propositional formulae:\<close>

datatype form = TrueF | FalseF | Less nat nat
  | And form form | Or form form | Neg form | ExQ form

primrec interp :: "form \<Rightarrow> ('a::ord) list \<Rightarrow> bool"
where
  "interp TrueF vs \<longleftrightarrow> True"
| "interp FalseF vs \<longleftrightarrow> False"
| "interp (Less i j) vs \<longleftrightarrow> vs ! i < vs ! j"
| "interp (And f1 f2) vs \<longleftrightarrow> interp f1 vs \<and> interp f2 vs"
| "interp (Or f1 f2) vs \<longleftrightarrow> interp f1 vs \<or> interp f2 vs"
| "interp (Neg f) vs \<longleftrightarrow> \<not> interp f vs"
| "interp (ExQ f) vs \<longleftrightarrow> (\<exists>v. interp f (v # vs))"

lemmas interp_reify_eqs = interp.simps
declare interp_reify_eqs [reify]

lemma "\<exists>x. x < y \<and> x < z"
  apply reify
  oops

datatype fm = And fm fm | Or fm fm | Imp fm fm | Iff fm fm | Not fm | At nat

primrec Ifm :: "fm \<Rightarrow> bool list \<Rightarrow> bool"
where
  "Ifm (At n) vs \<longleftrightarrow> vs ! n"
| "Ifm (And p q) vs \<longleftrightarrow> Ifm p vs \<and> Ifm q vs"
| "Ifm (Or p q) vs \<longleftrightarrow> Ifm p vs \<or> Ifm q vs"
| "Ifm (Imp p q) vs \<longleftrightarrow> Ifm p vs \<longrightarrow> Ifm q vs"
| "Ifm (Iff p q) vs \<longleftrightarrow> Ifm p vs = Ifm q vs"
| "Ifm (Not p) vs \<longleftrightarrow> \<not> Ifm p vs"

lemma "Q \<longrightarrow> (D \<and> F \<and> ((\<not> D) \<and> (\<not> F)))"
  apply (reify Ifm.simps)
oops

text \<open>Method \<open>reify\<close> maps a \<^typ>\<open>bool\<close> to an \<^typ>\<open>fm\<close>.  For this it needs the 
semantics of \<open>fm\<close>, i.e.\ the rewrite rules in \<open>Ifm.simps\<close>.\<close>

text \<open>You can also just pick up a subterm to reify.\<close>
lemma "Q \<longrightarrow> (D \<and> F \<and> ((\<not> D) \<and> (\<not> F)))"
  apply (reify Ifm.simps ("((\<not> D) \<and> (\<not> F))"))
oops

text \<open>Let's perform NNF. This is a version that tends to generate disjunctions\<close>
primrec fmsize :: "fm \<Rightarrow> nat"
where
  "fmsize (At n) = 1"
| "fmsize (Not p) = 1 + fmsize p"
| "fmsize (And p q) = 1 + fmsize p + fmsize q"
| "fmsize (Or p q) = 1 + fmsize p + fmsize q"
| "fmsize (Imp p q) = 2 + fmsize p + fmsize q"
| "fmsize (Iff p q) = 2 + 2* fmsize p + 2* fmsize q"

lemma [measure_function]: "is_measure fmsize" ..

fun nnf :: "fm \<Rightarrow> fm"
where
  "nnf (At n) = At n"
| "nnf (And p q) = And (nnf p) (nnf q)"
| "nnf (Or p q) = Or (nnf p) (nnf q)"
| "nnf (Imp p q) = Or (nnf (Not p)) (nnf q)"
| "nnf (Iff p q) = Or (And (nnf p) (nnf q)) (And (nnf (Not p)) (nnf (Not q)))"
| "nnf (Not (And p q)) = Or (nnf (Not p)) (nnf (Not q))"
| "nnf (Not (Or p q)) = And (nnf (Not p)) (nnf (Not q))"
| "nnf (Not (Imp p q)) = And (nnf p) (nnf (Not q))"
| "nnf (Not (Iff p q)) = Or (And (nnf p) (nnf (Not q))) (And (nnf (Not p)) (nnf q))"
| "nnf (Not (Not p)) = nnf p"
| "nnf (Not p) = Not p"

text \<open>The correctness theorem of \<^const>\<open>nnf\<close>: it preserves the semantics of \<^typ>\<open>fm\<close>\<close>
lemma nnf [reflection]:
  "Ifm (nnf p) vs = Ifm p vs"
  by (induct p rule: nnf.induct) auto

text \<open>Now let's perform NNF using our \<^const>\<open>nnf\<close> function defined above.  First to the
  whole subgoal.\<close>
lemma "A \<noteq> B \<and> (B \<longrightarrow> A \<noteq> (B \<or> C \<and> (B \<longrightarrow> A \<or> D))) \<longrightarrow> A \<or> B \<and> D"
  apply (reflection Ifm.simps)
oops

text \<open>Now we specify on which subterm it should be applied\<close>
lemma "A \<noteq> B \<and> (B \<longrightarrow> A \<noteq> (B \<or> C \<and> (B \<longrightarrow> A \<or> D))) \<longrightarrow> A \<or> B \<and> D"
  apply (reflection Ifm.simps only: "B \<or> C \<and> (B \<longrightarrow> A \<or> D)")
oops


text \<open>Example 2: Simple arithmetic formulae\<close>

text \<open>The type \<open>num\<close> reflects linear expressions over natural number\<close>
datatype num = C nat | Add num num | Mul nat num | Var nat | CN nat nat num

text \<open>This is just technical to make recursive definitions easier.\<close>
primrec num_size :: "num \<Rightarrow> nat" 
where
  "num_size (C c) = 1"
| "num_size (Var n) = 1"
| "num_size (Add a b) = 1 + num_size a + num_size b"
| "num_size (Mul c a) = 1 + num_size a"
| "num_size (CN n c a) = 4 + num_size a "

lemma [measure_function]: "is_measure num_size" ..

text \<open>The semantics of num\<close>
primrec Inum:: "num \<Rightarrow> nat list \<Rightarrow> nat"
where
  Inum_C  : "Inum (C i) vs = i"
| Inum_Var: "Inum (Var n) vs = vs!n"
| Inum_Add: "Inum (Add s t) vs = Inum s vs + Inum t vs "
| Inum_Mul: "Inum (Mul c t) vs = c * Inum t vs "
| Inum_CN : "Inum (CN n c t) vs = c*(vs!n) + Inum t vs "

text \<open>Let's reify some nat expressions \dots\<close>
lemma "4 * (2 * x + (y::nat)) + f a \<noteq> 0"
  apply (reify Inum.simps ("4 * (2 * x + (y::nat)) + f a"))
oops
text \<open>We're in a bad situation! \<open>x\<close>, \<open>y\<close> and \<open>f\<close> have been recongnized
as constants, which is correct but does not correspond to our intuition of the constructor C.
It should encapsulate constants, i.e. numbers, i.e. numerals.\<close>

text \<open>So let's leave the \<open>Inum_C\<close> equation at the end and see what happens \dots\<close>
lemma "4 * (2 * x + (y::nat)) \<noteq> 0"
  apply (reify Inum_Var Inum_Add Inum_Mul Inum_CN Inum_C ("4 * (2 * x + (y::nat))"))
oops
text \<open>Hm, let's specialize \<open>Inum_C\<close> with numerals.\<close>

lemma Inum_number: "Inum (C (numeral t)) vs = numeral t" by simp
lemmas Inum_eqs = Inum_Var Inum_Add Inum_Mul Inum_CN Inum_number

text \<open>Second attempt\<close>
lemma "1 * (2 * x + (y::nat)) \<noteq> 0"
  apply (reify Inum_eqs ("1 * (2 * x + (y::nat))"))
oops

text\<open>That was fine, so let's try another one \dots\<close>

lemma "1 * (2 * x + (y::nat) + 0 + 1) \<noteq> 0"
  apply (reify Inum_eqs ("1 * (2 * x + (y::nat) + 0 + 1)"))
oops

text \<open>Oh!! 0 is not a variable \dots\ Oh! 0 is not a \<open>numeral\<close> \dots\ thing.
The same for 1. So let's add those equations, too.\<close>

lemma Inum_01: "Inum (C 0) vs = 0" "Inum (C 1) vs = 1" "Inum (C(Suc n)) vs = Suc n"
  by simp_all

lemmas Inum_eqs'= Inum_eqs Inum_01

text\<open>Third attempt:\<close>

lemma "1 * (2 * x + (y::nat) + 0 + 1) \<noteq> 0"
  apply (reify Inum_eqs' ("1 * (2 * x + (y::nat) + 0 + 1)"))
oops

text \<open>Okay, let's try reflection. Some simplifications on \<^typ>\<open>num\<close> follow. You can
  skim until the main theorem \<open>linum\<close>.\<close>
  
fun lin_add :: "num \<Rightarrow> num \<Rightarrow> num"
where
  "lin_add (CN n1 c1 r1) (CN n2 c2 r2) =
    (if n1 = n2 then 
      (let c = c1 + c2
       in (if c = 0 then lin_add r1 r2 else CN n1 c (lin_add r1 r2)))
     else if n1 \<le> n2 then (CN n1 c1 (lin_add r1 (CN n2 c2 r2))) 
     else (CN n2 c2 (lin_add (CN n1 c1 r1) r2)))"
| "lin_add (CN n1 c1 r1) t = CN n1 c1 (lin_add r1 t)"  
| "lin_add t (CN n2 c2 r2) = CN n2 c2 (lin_add t r2)" 
| "lin_add (C b1) (C b2) = C (b1 + b2)"
| "lin_add a b = Add a b"

lemma lin_add:
  "Inum (lin_add t s) bs = Inum (Add t s) bs"
  apply (induct t s rule: lin_add.induct, simp_all add: Let_def)
  apply (case_tac "c1+c2 = 0",case_tac "n1 \<le> n2", simp_all)
  apply (case_tac "n1 = n2", simp_all add: algebra_simps)
  done

fun lin_mul :: "num \<Rightarrow> nat \<Rightarrow> num"
where
  "lin_mul (C j) i = C (i * j)"
| "lin_mul (CN n c a) i = (if i=0 then (C 0) else CN n (i * c) (lin_mul a i))"
| "lin_mul t i = (Mul i t)"

lemma lin_mul:
  "Inum (lin_mul t i) bs = Inum (Mul i t) bs"
  by (induct t i rule: lin_mul.induct) (auto simp add: algebra_simps)

fun linum:: "num \<Rightarrow> num"
where
  "linum (C b) = C b"
| "linum (Var n) = CN n 1 (C 0)"
| "linum (Add t s) = lin_add (linum t) (linum s)"
| "linum (Mul c t) = lin_mul (linum t) c"
| "linum (CN n c t) = lin_add (linum (Mul c (Var n))) (linum t)"

lemma linum [reflection]:
  "Inum (linum t) bs = Inum t bs"
  by (induct t rule: linum.induct) (simp_all add: lin_mul lin_add)

text \<open>Now we can use linum to simplify nat terms using reflection\<close>

lemma "Suc (Suc 1) * (x + Suc 1 * y) = 3 * x + 6 * y"
  apply (reflection Inum_eqs' only: "Suc (Suc 1) * (x + Suc 1 * y)")
oops

text \<open>Let's lift this to formulae and see what happens\<close>

datatype aform = Lt num num  | Eq num num | Ge num num | NEq num num | 
  Conj aform aform | Disj aform aform | NEG aform | T | F

primrec linaformsize:: "aform \<Rightarrow> nat"
where
  "linaformsize T = 1"
| "linaformsize F = 1"
| "linaformsize (Lt a b) = 1"
| "linaformsize (Ge a b) = 1"
| "linaformsize (Eq a b) = 1"
| "linaformsize (NEq a b) = 1"
| "linaformsize (NEG p) = 2 + linaformsize p"
| "linaformsize (Conj p q) = 1 + linaformsize p + linaformsize q"
| "linaformsize (Disj p q) = 1 + linaformsize p + linaformsize q"

lemma [measure_function]: "is_measure linaformsize" ..

primrec is_aform :: "aform => nat list => bool"
where
  "is_aform T vs = True"
| "is_aform F vs = False"
| "is_aform (Lt a b) vs = (Inum a vs < Inum b vs)"
| "is_aform (Eq a b) vs = (Inum a vs = Inum b vs)"
| "is_aform (Ge a b) vs = (Inum a vs \<ge> Inum b vs)"
| "is_aform (NEq a b) vs = (Inum a vs \<noteq> Inum b vs)"
| "is_aform (NEG p) vs = (\<not> (is_aform p vs))"
| "is_aform (Conj p q) vs = (is_aform p vs \<and> is_aform q vs)"
| "is_aform (Disj p q) vs = (is_aform p vs \<or> is_aform q vs)"

text\<open>Let's reify and do reflection\<close>
lemma "(3::nat) * x + t < 0 \<and> (2 * x + y \<noteq> 17)"
  apply (reify Inum_eqs' is_aform.simps) 
oops

text \<open>Note that reification handles several interpretations at the same time\<close>
lemma "(3::nat) * x + t < 0 \<and> x * x + t * x + 3 + 1 = z * t * 4 * z \<or> x + x + 1 < 0"
  apply (reflection Inum_eqs' is_aform.simps only: "x + x + 1") 
oops

text \<open>For reflection we now define a simple transformation on aform: NNF + linum on atoms\<close>

fun linaform:: "aform \<Rightarrow> aform"
where
  "linaform (Lt s t) = Lt (linum s) (linum t)"
| "linaform (Eq s t) = Eq (linum s) (linum t)"
| "linaform (Ge s t) = Ge (linum s) (linum t)"
| "linaform (NEq s t) = NEq (linum s) (linum t)"
| "linaform (Conj p q) = Conj (linaform p) (linaform q)"
| "linaform (Disj p q) = Disj (linaform p) (linaform q)"
| "linaform (NEG T) = F"
| "linaform (NEG F) = T"
| "linaform (NEG (Lt a b)) = Ge a b"
| "linaform (NEG (Ge a b)) = Lt a b"
| "linaform (NEG (Eq a b)) = NEq a b"
| "linaform (NEG (NEq a b)) = Eq a b"
| "linaform (NEG (NEG p)) = linaform p"
| "linaform (NEG (Conj p q)) = Disj (linaform (NEG p)) (linaform (NEG q))"
| "linaform (NEG (Disj p q)) = Conj (linaform (NEG p)) (linaform (NEG q))"
| "linaform p = p"

lemma linaform: "is_aform (linaform p) vs = is_aform p vs"
  by (induct p rule: linaform.induct) (auto simp add: linum)

lemma "(Suc (Suc (Suc 0)) * ((x::nat) + Suc (Suc 0)) + Suc (Suc (Suc 0)) *
  (Suc (Suc (Suc 0))) * ((x::nat) + Suc (Suc 0))) < 0 \<and> Suc 0 + Suc 0 < 0"
  apply (reflection Inum_eqs' is_aform.simps rules: linaform)  
oops

declare linaform [reflection]

lemma "(Suc (Suc (Suc 0)) * ((x::nat) + Suc (Suc 0)) + Suc (Suc (Suc 0)) *
  (Suc (Suc (Suc 0))) * ((x::nat) + Suc (Suc 0))) < 0 \<and> Suc 0 + Suc 0 < 0"
  apply (reflection Inum_eqs' is_aform.simps)
oops

text \<open>We now give an example where interpretaions have zero or more than only
  one envornement of different types and show that automatic reification also deals with
  bindings\<close>
  
datatype rb = BC bool | BAnd rb rb | BOr rb rb

primrec Irb :: "rb \<Rightarrow> bool"
where
  "Irb (BC p) \<longleftrightarrow> p"
| "Irb (BAnd s t) \<longleftrightarrow> Irb s \<and> Irb t"
| "Irb (BOr s t) \<longleftrightarrow> Irb s \<or> Irb t"

lemma "A \<and> (B \<or> D \<and> B) \<and> A \<and> (B \<or> D \<and> B) \<or> A \<and> (B \<or> D \<and> B) \<or> A \<and> (B \<or> D \<and> B)"
  apply (reify Irb.simps)
oops

datatype rint = IC int | IVar nat | IAdd rint rint | IMult rint rint
  | INeg rint | ISub rint rint

primrec Irint :: "rint \<Rightarrow> int list \<Rightarrow> int"
where
  Irint_Var: "Irint (IVar n) vs = vs ! n"
| Irint_Neg: "Irint (INeg t) vs = - Irint t vs"
| Irint_Add: "Irint (IAdd s t) vs = Irint s vs + Irint t vs"
| Irint_Sub: "Irint (ISub s t) vs = Irint s vs - Irint t vs"
| Irint_Mult: "Irint (IMult s t) vs = Irint s vs * Irint t vs"
| Irint_C: "Irint (IC i) vs = i"

lemma Irint_C0: "Irint (IC 0) vs = 0"
  by simp

lemma Irint_C1: "Irint (IC 1) vs = 1"
  by simp

lemma Irint_Cnumeral: "Irint (IC (numeral x)) vs = numeral x"
  by simp

lemmas Irint_simps = Irint_Var Irint_Neg Irint_Add Irint_Sub Irint_Mult Irint_C0 Irint_C1 Irint_Cnumeral

lemma "(3::int) * x + y * y - 9 + (- z) = 0"
  apply (reify Irint_simps ("(3::int) * x + y * y - 9 + (- z)"))
  oops

datatype rlist = LVar nat | LEmpty | LCons rint rlist | LAppend rlist rlist

primrec Irlist :: "rlist \<Rightarrow> int list \<Rightarrow> int list list \<Rightarrow> int list"
where
  "Irlist (LEmpty) is vs = []"
| "Irlist (LVar n) is vs = vs ! n"
| "Irlist (LCons i t) is vs = Irint i is # Irlist t is vs"
| "Irlist (LAppend s t) is vs = Irlist s is vs @ Irlist t is vs"

lemma "[(1::int)] = []"
  apply (reify Irlist.simps Irint_simps ("[1] :: int list"))
  oops

lemma "([(3::int) * x + y * y - 9 + (- z)] @ []) @ xs = [y * y - z - 9 + (3::int) * x]"
  apply (reify Irlist.simps Irint_simps ("([(3::int) * x + y * y - 9 + (- z)] @ []) @ xs"))
  oops

datatype rnat = NC nat| NVar nat| NSuc rnat | NAdd rnat rnat | NMult rnat rnat
  | NNeg rnat | NSub rnat rnat | Nlgth rlist

primrec Irnat :: "rnat \<Rightarrow> int list \<Rightarrow> int list list \<Rightarrow> nat list \<Rightarrow> nat"
where
  Irnat_Suc: "Irnat (NSuc t) is ls vs = Suc (Irnat t is ls vs)"
| Irnat_Var: "Irnat (NVar n) is ls vs = vs ! n"
| Irnat_Neg: "Irnat (NNeg t) is ls vs = 0"
| Irnat_Add: "Irnat (NAdd s t) is ls vs = Irnat s is ls vs + Irnat t is ls vs"
| Irnat_Sub: "Irnat (NSub s t) is ls vs = Irnat s is ls vs - Irnat t is ls vs"
| Irnat_Mult: "Irnat (NMult s t) is ls vs = Irnat s is ls vs * Irnat t is ls vs"
| Irnat_lgth: "Irnat (Nlgth rxs) is ls vs = length (Irlist rxs is ls)"
| Irnat_C: "Irnat (NC i) is ls vs = i"

lemma Irnat_C0: "Irnat (NC 0) is ls vs = 0"
  by simp

lemma Irnat_C1: "Irnat (NC 1) is ls vs = 1"
  by simp

lemma Irnat_Cnumeral: "Irnat (NC (numeral x)) is ls vs = numeral x"
  by simp

lemmas Irnat_simps = Irnat_Suc Irnat_Var Irnat_Neg Irnat_Add Irnat_Sub Irnat_Mult Irnat_lgth
  Irnat_C0 Irnat_C1 Irnat_Cnumeral

lemma "Suc n * length (([(3::int) * x + y * y - 9 + (- z)] @ []) @ xs) = length xs"
  apply (reify Irnat_simps Irlist.simps Irint_simps
     ("Suc n * length (([(3::int) * x + y * y - 9 + (- z)] @ []) @ xs)"))
  oops

datatype rifm = RT | RF | RVar nat
  | RNLT rnat rnat | RNILT rnat rint | RNEQ rnat rnat
  | RAnd rifm rifm | ROr rifm rifm | RImp rifm rifm| RIff rifm rifm
  | RNEX rifm | RIEX rifm | RLEX rifm | RNALL rifm | RIALL rifm | RLALL rifm
  | RBEX rifm | RBALL rifm

primrec Irifm :: "rifm \<Rightarrow> bool list \<Rightarrow> int list \<Rightarrow> (int list) list \<Rightarrow> nat list \<Rightarrow> bool"
where
  "Irifm RT ps is ls ns \<longleftrightarrow> True"
| "Irifm RF ps is ls ns \<longleftrightarrow> False"
| "Irifm (RVar n) ps is ls ns \<longleftrightarrow> ps ! n"
| "Irifm (RNLT s t) ps is ls ns \<longleftrightarrow> Irnat s is ls ns < Irnat t is ls ns"
| "Irifm (RNILT s t) ps is ls ns \<longleftrightarrow> int (Irnat s is ls ns) < Irint t is"
| "Irifm (RNEQ s t) ps is ls ns \<longleftrightarrow> Irnat s is ls ns = Irnat t is ls ns"
| "Irifm (RAnd p q) ps is ls ns \<longleftrightarrow> Irifm p ps is ls ns \<and> Irifm q ps is ls ns"
| "Irifm (ROr p q) ps is ls ns \<longleftrightarrow> Irifm p ps is ls ns \<or> Irifm q ps is ls ns"
| "Irifm (RImp p q) ps is ls ns \<longleftrightarrow> Irifm p ps is ls ns \<longrightarrow> Irifm q ps is ls ns"
| "Irifm (RIff p q) ps is ls ns \<longleftrightarrow> Irifm p ps is ls ns = Irifm q ps is ls ns"
| "Irifm (RNEX p) ps is ls ns \<longleftrightarrow> (\<exists>x. Irifm p ps is ls (x # ns))"
| "Irifm (RIEX p) ps is ls ns \<longleftrightarrow> (\<exists>x. Irifm p ps (x # is) ls ns)"
| "Irifm (RLEX p) ps is ls ns \<longleftrightarrow> (\<exists>x. Irifm p ps is (x # ls) ns)"
| "Irifm (RBEX p) ps is ls ns \<longleftrightarrow> (\<exists>x. Irifm p (x # ps) is ls ns)"
| "Irifm (RNALL p) ps is ls ns \<longleftrightarrow> (\<forall>x. Irifm p ps is ls (x#ns))"
| "Irifm (RIALL p) ps is ls ns \<longleftrightarrow> (\<forall>x. Irifm p ps (x # is) ls ns)"
| "Irifm (RLALL p) ps is ls ns \<longleftrightarrow> (\<forall>x. Irifm p ps is (x#ls) ns)"
| "Irifm (RBALL p) ps is ls ns \<longleftrightarrow> (\<forall>x. Irifm p (x # ps) is ls ns)"

lemma " \<forall>x. \<exists>n. ((Suc n) * length (([(3::int) * x + f t * y - 9 + (- z)] @ []) @ xs) = length xs) \<and> m < 5*n - length (xs @ [2,3,4,x*z + 8 - y]) \<longrightarrow> (\<exists>p. \<forall>q. p \<and> q \<longrightarrow> r)"
  apply (reify Irifm.simps Irnat_simps Irlist.simps Irint_simps)
oops

text \<open>An example for equations containing type variables\<close>

datatype prod = Zero | One | Var nat | Mul prod prod 
  | Pw prod nat | PNM nat nat prod

primrec Iprod :: " prod \<Rightarrow> ('a::linordered_idom) list \<Rightarrow>'a" 
where
  "Iprod Zero vs = 0"
| "Iprod One vs = 1"
| "Iprod (Var n) vs = vs ! n"
| "Iprod (Mul a b) vs = Iprod a vs * Iprod b vs"
| "Iprod (Pw a n) vs = Iprod a vs ^ n"
| "Iprod (PNM n k t) vs = (vs ! n) ^ k * Iprod t vs"

datatype sgn = Pos prod | Neg prod | ZeroEq prod | NZeroEq prod | Tr | F 
  | Or sgn sgn | And sgn sgn

primrec Isgn :: "sgn \<Rightarrow> ('a::linordered_idom) list \<Rightarrow> bool"
where 
  "Isgn Tr vs \<longleftrightarrow> True"
| "Isgn F vs \<longleftrightarrow> False"
| "Isgn (ZeroEq t) vs \<longleftrightarrow> Iprod t vs = 0"
| "Isgn (NZeroEq t) vs \<longleftrightarrow> Iprod t vs \<noteq> 0"
| "Isgn (Pos t) vs \<longleftrightarrow> Iprod t vs > 0"
| "Isgn (Neg t) vs \<longleftrightarrow> Iprod t vs < 0"
| "Isgn (And p q) vs \<longleftrightarrow> Isgn p vs \<and> Isgn q vs"
| "Isgn (Or p q) vs \<longleftrightarrow> Isgn p vs \<or> Isgn q vs"

lemmas eqs = Isgn.simps Iprod.simps

lemma "(x::'a::{linordered_idom}) ^ 4 * y * z * y ^ 2 * z ^ 23 > 0"
  apply (reify eqs)
  oops

end

