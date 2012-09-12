(*  Title:      HOL/Codatatype/BNF_Def.thy
    Author:     Dmitriy Traytel, TU Muenchen
    Copyright   2012

Definition of bounded natural functors.
*)

header {* Definition of Bounded Natural Functors *}

theory BNF_Def
imports BNF_Util
keywords
  "print_bnfs" :: diag and
  "bnf_def" :: thy_goal
begin

ML_file "Tools/bnf_def_tactics.ML"
ML_file"Tools/bnf_def.ML"

end
