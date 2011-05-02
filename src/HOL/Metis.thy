(*  Title:      HOL/Metis.thy
    Author:     Lawrence C. Paulson, Cambridge University Computer Laboratory
    Author:     Jia Meng, Cambridge University Computer Laboratory and NICTA
    Author:     Jasmin Blanchette, TU Muenchen
*)

header {* Metis Proof Method *}

theory Metis
imports Meson
uses "~~/src/Tools/Metis/metis.ML"
     ("Tools/Metis/metis_translate.ML")
     ("Tools/Metis/metis_reconstruct.ML")
     ("Tools/Metis/metis_tactics.ML")
     ("Tools/try.ML")
begin


subsection {* Higher-order reasoning helpers *}

definition fFalse :: bool where [no_atp]:
"fFalse \<longleftrightarrow> False"

definition fTrue :: bool where [no_atp]:
"fTrue \<longleftrightarrow> True"

definition fNot :: "bool \<Rightarrow> bool" where [no_atp]:
"fNot P \<longleftrightarrow> \<not> P"

definition fconj :: "bool \<Rightarrow> bool \<Rightarrow> bool" where [no_atp]:
"fconj P Q \<longleftrightarrow> P \<and> Q"

definition fdisj :: "bool \<Rightarrow> bool \<Rightarrow> bool" where [no_atp]:
"fdisj P Q \<longleftrightarrow> P \<or> Q"

definition fimplies :: "bool \<Rightarrow> bool \<Rightarrow> bool" where [no_atp]:
"fimplies P Q \<longleftrightarrow> (P \<longrightarrow> Q)"

definition fequal :: "'a \<Rightarrow> 'a \<Rightarrow> bool" where [no_atp]:
"fequal x y \<longleftrightarrow> (x = y)"


subsection {* Literal selection helpers *}

definition select :: "'a \<Rightarrow> 'a" where
[no_atp]: "select = (\<lambda>x. x)"

lemma not_atomize: "(\<not> A \<Longrightarrow> False) \<equiv> Trueprop A"
by (cut_tac atomize_not [of "\<not> A"]) simp

lemma atomize_not_select: "(A \<Longrightarrow> select False) \<equiv> Trueprop (\<not> A)"
unfolding select_def by (rule atomize_not)

lemma not_atomize_select: "(\<not> A \<Longrightarrow> select False) \<equiv> Trueprop A"
unfolding select_def by (rule not_atomize)

lemma select_FalseI: "False \<Longrightarrow> select False" by simp


subsection {* Metis package *}

use "Tools/Metis/metis_translate.ML"
use "Tools/Metis/metis_reconstruct.ML"
use "Tools/Metis/metis_tactics.ML"

setup {* Metis_Tactics.setup *}

hide_const (open) fFalse fTrue fNot fconj fdisj fimplies fequal select
hide_fact (open) fFalse_def fTrue_def fNot_def fconj_def fdisj_def fimplies_def
    fequal_def select_def not_atomize atomize_not_select not_atomize_select
    select_FalseI

subsection {* Try *}

use "Tools/try.ML"

setup {* Try.setup *}

end
