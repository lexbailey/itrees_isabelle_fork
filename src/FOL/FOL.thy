(*  Title:      FOL/FOL.thy
    ID:         $Id$
    Author:     Lawrence C Paulson and Markus Wenzel
*)

header {* Classical first-order logic *}

theory FOL = IFOL
files
  ("FOL_lemmas1.ML") ("cladata.ML") ("blastdata.ML")
  ("simpdata.ML") ("FOL_lemmas2.ML"):


subsection {* The classical axiom *}

axioms
  classical: "(~P ==> P) ==> P"


subsection {* Lemmas and proof tools *}

use "FOL_lemmas1.ML"
theorems case_split = case_split_thm [case_names True False]

use "cladata.ML"
setup Cla.setup
setup clasetup

use "blastdata.ML"
setup Blast.setup
use "FOL_lemmas2.ML"

use "simpdata.ML"
setup simpsetup
setup "Simplifier.method_setup Splitter.split_modifiers"
setup Splitter.setup
setup Clasimp.setup


subsection {* Proof by cases and induction *}

text {* Proper handling of non-atomic rule statements. *}

constdefs
  induct_forall :: "('a => o) => o"
  "induct_forall(P) == \<forall>x. P(x)"
  induct_implies :: "o => o => o"
  "induct_implies(A, B) == A --> B"
  induct_equal :: "'a => 'a => o"
  "induct_equal(x, y) == x = y"

lemma induct_forall_eq: "(!!x. P(x)) == Trueprop(induct_forall(\<lambda>x. P(x)))"
  by (simp only: atomize_all induct_forall_def)

lemma induct_implies_eq: "(A ==> B) == Trueprop(induct_implies(A, B))"
  by (simp only: atomize_imp induct_implies_def)

lemma induct_equal_eq: "(x == y) == Trueprop(induct_equal(x, y))"
  by (simp only: atomize_eq induct_equal_def)

lemmas induct_atomize = induct_forall_eq induct_implies_eq induct_equal_eq
lemmas induct_rulify1 = induct_atomize [symmetric, standard]
lemmas induct_rulify2 = induct_forall_def induct_implies_def induct_equal_def

hide const induct_forall induct_implies induct_equal


text {* Method setup. *}

ML {*
  structure InductMethod = InductMethodFun
  (struct
    val dest_concls = FOLogic.dest_concls;
    val cases_default = thm "case_split";
    val conjI = thm "conjI";
    val atomize = thms "induct_atomize";
    val rulify1 = thms "induct_rulify1";
    val rulify2 = thms "induct_rulify2";
  end);
*}

setup InductMethod.setup


subsection {* Calculational rules *}

lemma forw_subst: "a = b ==> P(b) ==> P(a)"
  by (rule ssubst)

lemma back_subst: "P(a) ==> a = b ==> P(b)"
  by (rule subst)

text {*
  Note that this list of rules is in reverse order of priorities.
*}

lemmas trans_rules [trans] =
  forw_subst
  back_subst
  rev_mp
  mp
  transitive
  trans

lemmas [elim?] = sym

end
