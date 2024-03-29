(*  Title:      HOL/Tools/SMT/Verit_Proof.ML
    Author:     Mathias Fleury, ENS Rennes
    Author:     Sascha Boehme, TU Muenchen

VeriT proofs: parsing and abstract syntax tree.
*)

signature VERIT_PROOF =
sig
  (*proofs*)
  datatype veriT_step = VeriT_Step of {
    id: string,
    rule: string,
    prems: string list,
    proof_ctxt: term list,
    concl: term,
    fixes: string list}

  datatype veriT_replay_node = VeriT_Replay_Node of {
    id: string,
    rule: string,
    args: term list,
    prems: string list,
    proof_ctxt: term list,
    concl: term,
    bounds: (string * typ) list,
    declarations: (string * term) list,
    insts: term Symtab.table,
    subproof: (string * typ) list * term list * term list * veriT_replay_node list}

  (*proof parser*)
  val parse: typ Symtab.table -> term Symtab.table -> string list ->
    Proof.context -> veriT_step list * Proof.context
  val parse_replay: typ Symtab.table -> term Symtab.table -> string list ->
    Proof.context -> veriT_replay_node list * Proof.context

  val step_prefix : string
  val input_rule: string
  val keep_app_symbols: string -> bool
  val keep_raw_lifting: string -> bool
  val normalized_input_rule: string
  val la_generic_rule : string
  val rewrite_rule : string
  val simp_arith_rule : string
  val veriT_deep_skolemize_rule : string
  val veriT_def : string
  val subproof_rule : string
  val local_input_rule : string
  val not_not_rule : string
  val contract_rule : string
  val ite_intro_rule : string
  val eq_congruent_rule : string
  val eq_congruent_pred_rule : string
  val skolemization_steps : string list
  val theory_resolution2_rule: string
  val equiv_pos2_rule: string
  val th_resolution_rule: string
  val and_pos_rule: string

  val is_skolemization: string -> bool
  val is_skolemization_step: veriT_replay_node -> bool

  val number_of_steps: veriT_replay_node list -> int

  (*Strategy related*)
  val veriT_strategy : string Config.T
  val veriT_current_strategy : Context.generic -> string list
  val all_veriT_stgies: Context.generic -> string list;

  val select_veriT_stgy: string -> Context.generic -> Context.generic;
  val valid_veriT_stgy: string -> Context.generic -> bool;
  val verit_add_stgy: string * string list -> Context.generic -> Context.generic
  val verit_rm_stgy: string -> Context.generic -> Context.generic

  (*Global tactic*)
  val verit_tac: Proof.context -> thm list -> int -> tactic
  val verit_tac_stgy: string -> Proof.context -> thm list -> int -> tactic
end;

structure Verit_Proof: VERIT_PROOF =
struct

open SMTLIB_Proof

val veriT_strategy_default_name = "default"; (*FUDGE*)
val veriT_strategy_del_insts_name = "del_insts"; (*FUDGE*)
val veriT_strategy_rm_insts_name = "ccfv_SIG"; (*FUDGE*)
val veriT_strategy_ccfv_insts_name = "ccfv_threshold"; (*FUDGE*)
val veriT_strategy_best_name = "best"; (*FUDGE*)

val veriT_strategy_best = ["--index-sorts", "--index-fresh-sorts", "--triggers-new",
  "--triggers-sel-rm-specific"];
val veriT_strategy_del_insts = ["--index-sorts", "--index-fresh-sorts", "--ccfv-breadth",
  "--inst-deletion", "--index-SAT-triggers", "--inst-deletion-loops", "--inst-deletion-track-vars",
  "--inst-deletion", "--index-SAT-triggers"];
val veriT_strategy_rm_insts = ["--index-SIG", "--triggers-new", "--triggers-sel-rm-specific"];
val veriT_strategy_ccfv_insts = ["--index-sorts", "--index-fresh-sorts", "--triggers-new",
  "--triggers-sel-rm-specific", "--triggers-restrict-combine", "--inst-deletion",
  "--index-SAT-triggers", "--inst-deletion-loops", "--inst-deletion-track-vars", "--inst-deletion",
  "--index-SAT-triggers", "--inst-sorts-threshold=100000", "--ematch-exp=10000000",
  "--ccfv-index=100000", "--ccfv-index-full=1000"]

val veriT_strategy_default = [];

type verit_strategy = {default_strategy: string, strategies: (string * string list) list}
fun mk_verit_strategy default_strategy strategies : verit_strategy = {default_strategy=default_strategy,strategies=strategies}

val empty_data = mk_verit_strategy veriT_strategy_best_name
  [(veriT_strategy_default_name, veriT_strategy_default),
   (veriT_strategy_del_insts_name, veriT_strategy_del_insts),
   (veriT_strategy_rm_insts_name, veriT_strategy_rm_insts),
   (veriT_strategy_ccfv_insts_name, veriT_strategy_ccfv_insts),
   (veriT_strategy_best_name, veriT_strategy_best)]

fun merge_data ({strategies=strategies1,...}:verit_strategy,
    {default_strategy,strategies=strategies2}:verit_strategy) : verit_strategy =
  mk_verit_strategy default_strategy (AList.merge (op =) (op =) (strategies1, strategies2))

structure Data = Generic_Data
(
  type T = verit_strategy
  val empty = empty_data
  val merge = merge_data
)

fun veriT_current_strategy ctxt =
  let
    val {default_strategy,strategies} = (Data.get ctxt)
  in
    AList.lookup (op=) strategies default_strategy
   |> the
  end

val veriT_strategy = Attrib.setup_config_string \<^binding>\<open>smt_verit_strategy\<close> (K veriT_strategy_best_name);

fun valid_veriT_stgy stgy context =
  let
    val {strategies,...} = Data.get context
  in
    AList.defined (op =) strategies stgy
  end

fun select_veriT_stgy stgy context =
  let
    val {strategies,...} = Data.get context
    val upd = Data.map (K (mk_verit_strategy stgy strategies))
  in
    if not (AList.defined (op =) strategies stgy) then
      error ("Trying to select unknown veriT strategy: " ^ quote stgy)
    else upd context
  end

fun verit_add_stgy stgy context =
  let
    val {default_strategy,strategies} = Data.get context
  in
    Data.map
      (K (mk_verit_strategy default_strategy (AList.update (op =) stgy strategies)))
      context
  end

fun verit_rm_stgy stgy context =
  let
    val {default_strategy,strategies} = Data.get context
  in
    Data.map
      (K (mk_verit_strategy default_strategy (AList.delete (op =) stgy strategies)))
      context
  end

fun all_veriT_stgies context =
  let
    val {strategies,...} = Data.get context
   in
    map fst strategies
  end

val select_verit = SMT_Config.select_solver "verit"
fun verit_tac ctxt = SMT_Solver.smt_tac (Config.put SMT_Config.native_bv false ((Context.proof_map select_verit ctxt)))
fun verit_tac_stgy stgy ctxt = verit_tac (Context.proof_of (select_veriT_stgy stgy (Context.Proof ctxt)))

datatype raw_veriT_node = Raw_VeriT_Node of {
  id: string,
  rule: string,
  args: SMTLIB.tree,
  prems: string list,
  concl: SMTLIB.tree,
  declarations: (string * SMTLIB.tree) list,
  subproof: raw_veriT_node list}

fun mk_raw_node id rule args prems declarations concl subproof =
  Raw_VeriT_Node {id = id, rule = rule, args = args, prems = prems, declarations = declarations,
    concl = concl, subproof = subproof}

datatype veriT_node = VeriT_Node of {
  id: string,
  rule: string,
  prems: string list,
  proof_ctxt: term list,
  concl: term}

fun mk_node id rule prems proof_ctxt concl =
  VeriT_Node {id = id, rule = rule, prems = prems, proof_ctxt = proof_ctxt, concl = concl}

datatype veriT_replay_node = VeriT_Replay_Node of {
  id: string,
  rule: string,
  args: term list,
  prems: string list,
  proof_ctxt: term list,
  concl: term,
  bounds: (string * typ) list,
  insts: term Symtab.table,
  declarations: (string * term) list,
  subproof: (string * typ) list * term list * term list * veriT_replay_node list}

fun mk_replay_node id rule args prems proof_ctxt concl bounds insts declarations subproof =
  VeriT_Replay_Node {id = id, rule = rule, args = args, prems = prems, proof_ctxt = proof_ctxt,
    concl = concl, bounds = bounds, insts = insts, declarations = declarations,
    subproof = subproof}

datatype veriT_step = VeriT_Step of {
  id: string,
  rule: string,
  prems: string list,
  proof_ctxt: term list,
  concl: term,
  fixes: string list}

fun mk_step id rule prems proof_ctxt concl fixes =
  VeriT_Step {id = id, rule = rule, prems = prems, proof_ctxt = proof_ctxt, concl = concl,
    fixes = fixes}

val step_prefix = ".c"
val input_rule = "input"
val la_generic_rule = "la_generic"
val normalized_input_rule = "__normalized_input" (*arbitrary*)
val rewrite_rule = "__rewrite" (*arbitrary*)
val subproof_rule = "subproof"
val local_input_rule = "__local_input" (*arbitrary*)
val simp_arith_rule = "simp_arith"
val veriT_def = "__skolem_definition" (*arbitrary*)
val not_not_rule = "not_not"
val contract_rule = "contraction"
val eq_congruent_pred_rule = "eq_congruent_pred"
val eq_congruent_rule = "eq_congruent"
val ite_intro_rule = "ite_intro"
val default_skolem_rule = "sko_forall" (*arbitrary, but must be one of the skolems*)
val theory_resolution2_rule = "__theory_resolution2" (*arbitrary*)
val equiv_pos2_rule = "equiv_pos2"
val th_resolution_rule = "th_resolution"
val and_pos_rule = "and_pos"

val skolemization_steps = ["sko_forall", "sko_ex"]
val is_skolemization = member (op =) skolemization_steps
val keep_app_symbols = member (op =) [eq_congruent_pred_rule, eq_congruent_rule, ite_intro_rule, and_pos_rule]
val keep_raw_lifting = member (op =) [eq_congruent_pred_rule, eq_congruent_rule, ite_intro_rule, and_pos_rule]
val is_SH_trivial = member (op =) [not_not_rule, contract_rule]

fun is_skolemization_step (VeriT_Replay_Node {id, ...}) = is_skolemization id

(* Even the veriT developers do not know if the following rule can still appear in proofs: *)
val veriT_deep_skolemize_rule = "deep_skolemize"

fun number_of_steps [] = 0
  | number_of_steps ((VeriT_Replay_Node {subproof = (_, _, _, subproof), ...}) :: pf) =
      1 + number_of_steps subproof + number_of_steps pf

(* proof parser *)

fun node_of p cx =
  ([], cx)
  ||>> `(with_fresh_names (term_of p))
  |>> snd

fun find_type_in_formula (Abs (v, T, u)) var_name =
    if String.isPrefix var_name v then SOME T else find_type_in_formula u var_name
  | find_type_in_formula (u $ v) var_name =
    (case find_type_in_formula u var_name of
      NONE => find_type_in_formula v var_name
    | some_T => some_T)
  | find_type_in_formula (Free(v, T)) var_name =
    if String.isPrefix var_name v then SOME T else NONE
  | find_type_in_formula _ _ = NONE

fun synctactic_var_subst old_name new_name (u $ v) =
    (synctactic_var_subst old_name new_name u $ synctactic_var_subst old_name new_name v)
  | synctactic_var_subst old_name new_name (Abs (v, T, u)) =
    Abs (if String.isPrefix old_name v then new_name else v, T,
      synctactic_var_subst old_name new_name u)
  | synctactic_var_subst old_name new_name (Free (v, T)) =
     if String.isPrefix old_name v then Free (new_name, T) else Free (v, T)
  | synctactic_var_subst _ _ t = t

fun synctatic_rew_in_lhs_subst old_name new_name (Const(\<^const_name>\<open>HOL.eq\<close>, T) $ t1 $ t2) =
     Const(\<^const_name>\<open>HOL.eq\<close>, T) $ synctactic_var_subst old_name new_name t1 $ t2
  | synctatic_rew_in_lhs_subst old_name new_name (Const(\<^const_name>\<open>Trueprop\<close>, T) $ t1) =
     Const(\<^const_name>\<open>Trueprop\<close>, T) $ (synctatic_rew_in_lhs_subst old_name new_name t1)
  | synctatic_rew_in_lhs_subst _ _ t = t

fun add_bound_variables_to_ctxt cx =
  fold (update_binding o
    (fn (s, SOME typ) => (s, Term (Free (s, type_of cx typ)))))

local

  fun extract_symbols bds =
    bds
    |> map (fn (SMTLIB.S [SMTLIB.Sym "=", SMTLIB.Sym x, SMTLIB.Sym y], typ) => [([x, y], typ)]
             | t => raise (Fail ("match error " ^ @{make_string} t)))
    |> flat

  (* onepoint can bind a variable to another variable or to a constant *)
  fun extract_qnt_symbols cx bds =
    bds
    |> map (fn (SMTLIB.S [SMTLIB.Sym "=", SMTLIB.Sym x, SMTLIB.Sym y], typ) =>
                (case node_of (SMTLIB.Sym y) cx of
                  ((_, []), _) => [([x], typ)]
                | _ => [([x, y], typ)])
             | (SMTLIB.S (SMTLIB.Sym "=" :: SMTLIB.Sym x :: _), typ) => [([x], typ)]
             | t => raise (Fail ("match error " ^ @{make_string} t)))
    |> flat

  fun extract_symbols_map bds =
    bds
    |> map (fn (SMTLIB.S [SMTLIB.Sym "=", SMTLIB.Sym x, _], typ) => [([x], typ)])
    |> flat
in

fun declared_csts _ "__skolem_definition" [(SMTLIB.S [SMTLIB.Sym x, typ, _], _)] = [(x, typ)]
  | declared_csts _ "__skolem_definition" t = raise (Fail ("unrecognized skolem_definition " ^ @{make_string} t))
  | declared_csts _ _ _ = []

fun skolems_introduced_by_rule (SMTLIB.S bds) =
   fold (fn (SMTLIB.S [SMTLIB.Sym "=", _, SMTLIB.Sym y]) => curry (op ::) y) bds []

(*FIXME there is probably a way to use the information given by onepoint*)
fun bound_vars_by_rule _ "bind" (bds) = extract_symbols bds
  | bound_vars_by_rule cx "onepoint" bds = extract_qnt_symbols cx bds
  | bound_vars_by_rule _ "sko_forall" bds = extract_symbols_map bds
  | bound_vars_by_rule _ "sko_ex" bds = extract_symbols_map bds
  | bound_vars_by_rule _ "__skolem_definition" [(SMTLIB.S [SMTLIB.Sym x, typ, _], _)] = [([x], SOME typ)]
  | bound_vars_by_rule _ "__skolem_definition" [(SMTLIB.S [_, SMTLIB.Sym x, _], _)] = [([x], NONE)]
  | bound_vars_by_rule _ _ _ = []

(* VeriT adds "?" before some variables. *)
fun remove_all_qm (SMTLIB.Sym v :: l) =
    SMTLIB.Sym (perhaps (try (unprefix "?")) v) :: remove_all_qm l
  | remove_all_qm (SMTLIB.S l :: l') = SMTLIB.S (remove_all_qm l) :: remove_all_qm l'
  | remove_all_qm (SMTLIB.Key v :: l) = SMTLIB.Key v :: remove_all_qm l
  | remove_all_qm (v :: l) = v :: remove_all_qm l
  | remove_all_qm [] = []

fun remove_all_qm2 (SMTLIB.Sym v) = SMTLIB.Sym (perhaps (try (unprefix "?")) v)
  | remove_all_qm2 (SMTLIB.S l) = SMTLIB.S (remove_all_qm l)
  | remove_all_qm2 (SMTLIB.Key v) = SMTLIB.Key v
  | remove_all_qm2 v = v

end

datatype step_kind = ASSUME | ANCHOR | NO_STEP | NORMAL_STEP | SKOLEM

fun parse_raw_proof_steps (limit : string option) (ls : SMTLIB.tree list) (cx : name_bindings) :
     (raw_veriT_node list * SMTLIB.tree list * name_bindings) =
  let
    fun rotate_pair (a, (b, c)) = ((a, b), c)
    fun step_kind [] = (NO_STEP, SMTLIB.S [], [])
      | step_kind ((p as SMTLIB.S (SMTLIB.Sym "anchor" :: _)) :: l) = (ANCHOR, p, l)
      | step_kind ((p as SMTLIB.S (SMTLIB.Sym "assume" :: _)) :: l) = (ASSUME, p, l)
      | step_kind ((p as SMTLIB.S (SMTLIB.Sym "step" :: _)) :: l) = (NORMAL_STEP, p, l)
      | step_kind ((p as SMTLIB.S (SMTLIB.Sym "define-fun" :: _)) :: l) = (SKOLEM, p, l)
    fun parse_skolem (SMTLIB.S [SMTLIB.Sym "define-fun", SMTLIB.Sym id,  _, typ,
           SMTLIB.S (SMTLIB.Sym "!" :: t :: [SMTLIB.Key _, SMTLIB.Sym name])]) cx =
         (*replace the name binding by the constant instead of the full term in order to reduce
           the size of the generated terms and therefore the reconstruction time*)
         let val (l, cx) = (fst oo SMTLIB_Proof.extract_and_update_name_bindings) t cx
            |> apsnd (SMTLIB_Proof.update_name_binding (name, SMTLIB.Sym id))
         in
           (mk_raw_node (id ^ veriT_def) veriT_def (SMTLIB.S [SMTLIB.Sym id, typ, l]) [] []
              (SMTLIB.S [SMTLIB.Sym "=", SMTLIB.Sym id, l]) [], cx)
         end
      | parse_skolem (SMTLIB.S [SMTLIB.Sym "define-fun", SMTLIB.Sym id,  _, typ, SMTLIB.S l]) cx =
         let val (l, cx) = (fst oo SMTLIB_Proof.extract_and_update_name_bindings) (SMTLIB.S l ) cx
         in
           (mk_raw_node (id ^ veriT_def) veriT_def (SMTLIB.S [SMTLIB.Sym id, typ, l]) [] []
              (SMTLIB.S [SMTLIB.Sym "=", SMTLIB.Sym id, l]) [], cx)
         end
      | parse_skolem t _ = raise Fail ("unrecognized VeriT proof " ^ \<^make_string> t)
    fun get_id_cx (SMTLIB.S ((SMTLIB.Sym _) :: (SMTLIB.Sym id) :: l), cx) = (id, (l, cx))
      | get_id_cx t = raise Fail ("unrecognized VeriT proof " ^ \<^make_string> t)
    fun get_id (SMTLIB.S ((SMTLIB.Sym _) :: (SMTLIB.Sym id) :: l)) = (id, l)
      | get_id t = raise Fail ("unrecognized VeriT proof " ^ \<^make_string> t)
    fun parse_source (SMTLIB.Key "premises" :: SMTLIB.S source ::l, cx) =
        (SOME (map (fn (SMTLIB.Sym id) => id) source), (l, cx))
      | parse_source (l, cx) = (NONE, (l, cx))
    fun parse_rule (SMTLIB.Key "rule" :: SMTLIB.Sym r :: l, cx) = (r, (l, cx))
      | parse_rule t = raise Fail ("unrecognized VeriT proof " ^ \<^make_string> t)
    fun parse_anchor_step (SMTLIB.S (SMTLIB.Sym "anchor" :: SMTLIB.Key "step" :: SMTLIB.Sym r :: l), cx) = (r, (l, cx))
      | parse_anchor_step t = raise Fail ("unrecognized VeriT proof " ^ \<^make_string> t)
    fun parse_args (SMTLIB.Key "args" :: args :: l, cx) =
          let val ((args, cx), _) = SMTLIB_Proof.extract_and_update_name_bindings args cx
          in (args, (l, cx)) end
      | parse_args (l, cx) = (SMTLIB.S [], (l, cx))
    fun parse_and_clausify_conclusion (SMTLIB.S (SMTLIB.Sym "cl" :: []) :: l, cx) =
          (SMTLIB.Sym "false", (l, cx))
      | parse_and_clausify_conclusion (SMTLIB.S (SMTLIB.Sym "cl" :: concl) :: l, cx) =
          let val (concl, cx) = fold_map (fst oo SMTLIB_Proof.extract_and_update_name_bindings) concl cx
          in (SMTLIB.S (SMTLIB.Sym "or" :: concl), (l, cx)) end
      | parse_and_clausify_conclusion t = raise Fail ("unrecognized VeriT proof " ^ \<^make_string> t)
    val parse_normal_step =
        get_id_cx
        ##> parse_and_clausify_conclusion
        #> rotate_pair
        ##> parse_rule
        #> rotate_pair
        ##> parse_source
        #> rotate_pair
        ##> parse_args
        #> rotate_pair

    fun to_raw_node subproof ((((id, concl), rule), prems), args) =
        mk_raw_node id rule args (the_default [] prems) [] concl subproof
    fun at_discharge NONE _ = false
      | at_discharge (SOME id) p = p |> get_id |> fst |> (fn id2 => id = id2)
  in
    case step_kind ls of
        (NO_STEP, _, _) => ([],[], cx)
      | (NORMAL_STEP, p, l) =>
          if at_discharge limit p then ([], ls, cx) else
            let
              val (s, (_, cx)) =  (p, cx)
                |> parse_normal_step
                ||> (fn i => i)
                |>>  (to_raw_node [])
              val (rp, rl, cx) = parse_raw_proof_steps limit l cx
          in (s :: rp, rl, cx) end
      | (ASSUME, p, l) =>
          let
            val (id, t :: []) = p
              |> get_id
            val ((t, cx), _) = SMTLIB_Proof.extract_and_update_name_bindings t cx
            val s = mk_raw_node id input_rule (SMTLIB.S []) [] [] t []
            val (rp, rl, cx) = parse_raw_proof_steps limit l cx
          in (s :: rp, rl, cx) end
      | (ANCHOR, p, l) =>
          let
            val (anchor_id, (anchor_args, (_, cx))) = (p, cx) |> (parse_anchor_step ##> parse_args)
            val (subproof, discharge_step :: remaining_proof, cx) = parse_raw_proof_steps (SOME anchor_id) l cx
            val (curss, (_, cx)) = parse_normal_step (discharge_step, cx)
            val s = to_raw_node subproof (fst curss, anchor_args)
            val (rp, rl, cx) = parse_raw_proof_steps limit remaining_proof cx
          in (s :: rp, rl, cx) end
      | (SKOLEM, p, l) =>
          let
            val (s, cx) = parse_skolem p cx
            val (rp, rl, cx) = parse_raw_proof_steps limit l cx
          in (s :: rp, rl, cx) end
  end

fun proof_ctxt_of_rule "bind" t = t
  | proof_ctxt_of_rule "sko_forall" t = t
  | proof_ctxt_of_rule "sko_ex" t = t
  | proof_ctxt_of_rule "let" t = t
  | proof_ctxt_of_rule "onepoint" t = t
  | proof_ctxt_of_rule _ _ = []

fun args_of_rule "bind" t = t
  | args_of_rule "la_generic" t = t
  | args_of_rule "lia_generic" t = t
  | args_of_rule _ _ = []

fun insts_of_forall_inst "forall_inst" t = map (fn SMTLIB.S [_, SMTLIB.Sym x, a] => (x, a)) t
  | insts_of_forall_inst _ _ = []

fun id_of_last_step prems =
  if null prems then []
  else
    let val VeriT_Replay_Node {id, ...} = List.last prems in [id] end

fun extract_assumptions_from_subproof subproof =
  let fun extract_assumptions_from_subproof (VeriT_Replay_Node {rule, concl, ...}) assms =
    if rule = local_input_rule then concl :: assms else assms
  in
    fold extract_assumptions_from_subproof subproof []
  end

fun normalized_rule_name id rule =
  (case (rule = input_rule, can SMTLIB_Interface.role_and_index_of_assert_name id) of
    (true, true) => normalized_input_rule
  | (true, _) => local_input_rule
  | _ => rule)

fun is_assm_repetition id rule =
  rule = input_rule andalso can SMTLIB_Interface.role_and_index_of_assert_name id

fun extract_skolem ([SMTLIB.Sym var, typ, choice]) = (var, typ, choice)
  | extract_skolem t = raise Fail ("fail to parse type" ^ @{make_string} t)

(* The preprocessing takes care of:
     1. unfolding the shared terms
     2. extract the declarations of skolems to make sure that there are not unfolded
*)
fun preprocess compress step =
  let
    fun expand_assms cs =
      map (fn t => case AList.lookup (op =) cs t of NONE => t | SOME a => a)
    fun expand_lonely_arguments (args as SMTLIB.S [SMTLIB.Sym "=", _, _]) = [args]
      | expand_lonely_arguments (x as SMTLIB.S [SMTLIB.Sym var, _]) = [SMTLIB.S [SMTLIB.Sym "=", x, SMTLIB.Sym var]]

    fun preprocess (Raw_VeriT_Node {id, rule, args, prems, concl, subproof, ...}) (cx, remap_assms)  =
      let
        val (skolem_names, stripped_args) = args
          |> (fn SMTLIB.S args => args)
          |> map
              (fn SMTLIB.S [SMTLIB.Key "=", x, y] => SMTLIB.S [SMTLIB.Sym "=", x, y]
                | x => x)
          |> (rule = "bind" orelse rule = "onepoint") ? flat o (map expand_lonely_arguments)
          |> `(if rule = veriT_def then single o extract_skolem else K [])
          ||> SMTLIB.S
        val (subproof, (cx, _)) = fold_map preprocess subproof (cx, remap_assms) |> apfst flat
        val remap_assms = (if rule = "or" then (id, hd prems) :: remap_assms else remap_assms)
        (* declare variables in the context *)
        val declarations =
           if rule = veriT_def
           then skolem_names |> map (fn (name, _, choice) => (name, choice))
           else []
      in
        if compress andalso rule = "or"
        then ([], (cx, remap_assms))
        else ([Raw_VeriT_Node {id = id, rule = rule, args = stripped_args,
           prems = expand_assms remap_assms prems, declarations = declarations, concl = concl, subproof = subproof}],
          (cx, remap_assms))
      end
  in preprocess step end

fun filter_split _ [] = ([], [])
  | filter_split f (a :: xs) =
     (if f a then apfst (curry op :: a) else apsnd (curry op :: a)) (filter_split f xs)

fun collect_skolem_defs (Raw_VeriT_Node {rule, subproof = subproof, args, ...}) =
  (if is_skolemization rule then map (fn id => id ^ veriT_def) (skolems_introduced_by_rule args) else []) @
  flat (map collect_skolem_defs subproof)

fun extract_types_of_args (SMTLIB.S [var, typ, t as SMTLIB.S [SMTLIB.Sym "choice", _, _]]) =
    (SMTLIB.S [var, typ, t], SOME typ)
    |> single
 | extract_types_of_args (SMTLIB.S t) =
  let
    fun extract_types_of_arg (SMTLIB.S [eq, SMTLIB.S [var, typ], t]) = (SMTLIB.S [eq, var, t], SOME typ)
      | extract_types_of_arg t = (t, NONE)
  in
    t
    |> map extract_types_of_arg
  end

(*The postprocessing does:
  1. translate the terms to Isabelle syntax, taking care of free variables
  2. remove the ambiguity in the proof terms:
       x \<leadsto> y |- x = x
    means y = x. To remove ambiguity, we use the fact that y is a free variable and replace the term
    by:
      xy \<leadsto> y |- xy = x.
    This is now does not have an ambiguity and we can safely move the "xy \<leadsto> y" to the proof
    assumptions.
*)
fun postprocess_proof compress ctxt step cx =
  let
    fun postprocess (Raw_VeriT_Node {id, rule, args, prems, declarations, concl, subproof}) (cx, rew) =
    let
      val _ = (SMT_Config.verit_msg ctxt) (fn () => @{print} ("id =", id, "concl =", concl))

      val (args) = extract_types_of_args args
      val globally_bound_vars = declared_csts cx rule args
      val cx = fold (update_binding o (fn (s, typ) => (s, Term (Free (s, type_of cx typ)))))
           globally_bound_vars cx

      (*find rebound variables specific to the LHS of the equivalence symbol*)
      val bound_vars = bound_vars_by_rule cx rule args
      val bound_vars_no_typ = map fst bound_vars
      val rhs_vars =
        fold (fn [t', t] => t <> t' ? (curry (op ::) t) | _ => fn x => x) bound_vars_no_typ []
      fun not_already_bound cx t = SMTLIB_Proof.lookup_binding cx t = None andalso
          not (member (op =) rhs_vars t)
      val (shadowing_vars, rebound_lhs_vars) = bound_vars
        |> filter_split (fn ([t, _], typ) => not_already_bound cx t | _ => true)
        |>> map (apfst (hd))
        |>> (fn vars => vars @ flat (map (fn ([_, t], typ) => [(t, typ)] | _ => []) bound_vars))
      val subproof_rew = fold (fn [t, t'] => curry (op ::) (t, t ^ t'))
        (map fst rebound_lhs_vars) rew
      val subproof_rewriter = fold (fn (t, t') => synctatic_rew_in_lhs_subst t t')
         subproof_rew

      val ((concl, bounds), cx') = node_of concl cx

      val extra_lhs_vars = map (fn ([a,b], typ) => (a, a^b, typ)) rebound_lhs_vars
      val old_lhs_vars = map (fn (a, _, typ) => (a, typ)) extra_lhs_vars
      val new_lhs_vars = map (fn (_, newvar, typ) => (newvar, typ)) extra_lhs_vars

      (* postprocess conclusion *)
      val concl = SMTLIB_Isar.unskolemize_names ctxt (subproof_rewriter concl)

      val _ = (SMT_Config.verit_msg ctxt) (fn () => \<^print> ("id =", id, "concl =", concl))
      val _ = (SMT_Config.verit_msg ctxt) (fn () => \<^print> ("id =", id, "cx' =", cx',
        "bound_vars =", bound_vars))

      val bound_tvars =
        map (fn (s, SOME typ) => (s, type_of cx typ))
         (shadowing_vars @ new_lhs_vars)
      val subproof_cx =
         add_bound_variables_to_ctxt cx (shadowing_vars @ new_lhs_vars) cx

      fun could_unify (Bound i, Bound j) = i = j
        | could_unify (Var v, Var v') = v = v'
        | could_unify (Free v, Free v') = v = v'
        | could_unify (Const (v, ty), Const (v', ty')) = v = v' andalso ty = ty'
        | could_unify (Abs (_, ty, bdy), Abs (_, ty', bdy')) = ty = ty' andalso could_unify (bdy, bdy')
        | could_unify (u $ v, u' $ v') = could_unify (u, u') andalso could_unify (v, v')
        | could_unify _ = false
      fun is_alpha_renaming t =
          t
          |> HOLogic.dest_Trueprop
          |> HOLogic.dest_eq
          |> could_unify
        handle TERM _ => false
      val alpha_conversion = rule = "bind" andalso is_alpha_renaming concl

      val can_remove_subproof =
        compress andalso (is_skolemization rule orelse alpha_conversion)
      val (fixed_subproof : veriT_replay_node list, _) =
         fold_map postprocess (if can_remove_subproof then [] else subproof)
           (subproof_cx, subproof_rew)

      val unsk_and_rewrite = SMTLIB_Isar.unskolemize_names ctxt o subproof_rewriter

      (* postprocess assms *)
      val stripped_args = map fst args
      val sanitized_args = proof_ctxt_of_rule rule stripped_args

      val arg_cx = add_bound_variables_to_ctxt cx (shadowing_vars @ old_lhs_vars) subproof_cx
      val (termified_args, _) = fold_map node_of sanitized_args arg_cx |> apfst (map fst)
      val normalized_args = map unsk_and_rewrite termified_args

      val subproof_assms = proof_ctxt_of_rule rule normalized_args

      (* postprocess arguments *)
      val rule_args = args_of_rule rule stripped_args
      val (termified_args, _) = fold_map term_of rule_args subproof_cx
      val normalized_args = map unsk_and_rewrite termified_args
      val rule_args = map subproof_rewriter normalized_args

      val raw_insts = insts_of_forall_inst rule stripped_args
      fun termify_term (x, t) cx = let val (t, cx) = term_of t cx in ((x, t), cx) end
      val (termified_args, _) = fold_map termify_term raw_insts subproof_cx
      val insts = Symtab.empty
        |> fold (fn (x, t) => fn insts => Symtab.update_new (x, t) insts) termified_args
        |> Symtab.map (K unsk_and_rewrite)

      (* declarations *)
      val (declarations, _) = fold_map termify_term declarations cx
        |> apfst (map (apsnd unsk_and_rewrite))

      (* fix step *)
      val _ = if bounds <> [] then raise (Fail "found dangling variable in concl") else ()
      val skolem_defs = (if is_skolemization rule
         then map (fn id => id ^ veriT_def) (skolems_introduced_by_rule (SMTLIB.S (map fst args))) else [])
      val skolems_of_subproof = (if is_skolemization rule
         then flat (map collect_skolem_defs subproof) else [])
      val fixed_prems =
        prems @ (if is_assm_repetition id rule then [id] else []) @
        skolem_defs @ skolems_of_subproof @ (id_of_last_step fixed_subproof)

      (* fix subproof *)
      val normalized_rule = normalized_rule_name id rule
        |> (if compress andalso alpha_conversion then K "refl" else I)

      val extra_assms2 =
        (if rule = subproof_rule then extract_assumptions_from_subproof fixed_subproof else [])

      val step = mk_replay_node id normalized_rule rule_args fixed_prems subproof_assms concl
        [] insts declarations (bound_tvars, subproof_assms, extra_assms2, fixed_subproof)

    in
       (step, (cx', rew))
    end
  in
    postprocess step (cx, [])
    |> (fn (step, (cx, _)) => (step, cx))
  end

fun combine_proof_steps ((step1 : veriT_replay_node) :: step2 :: steps) =
      let
        val (VeriT_Replay_Node {id = id1, rule = rule1, args = args1, prems = prems1,
            proof_ctxt = proof_ctxt1, concl = concl1, bounds = bounds1, insts = insts1,
            declarations = declarations1,
            subproof = (bound_sub1, assms_sub1, assms_extra1, subproof1)}) = step1
        val (VeriT_Replay_Node {id = id2, rule = rule2, args = args2, prems = prems2,
            proof_ctxt = proof_ctxt2, concl = concl2, bounds = bounds2, insts = insts2,
            declarations = declarations2,
            subproof = (bound_sub2, assms_sub2, assms_extra2, subproof2)}) = step2
        val goals1 =
          (case concl1 of
            _ $ (Const (\<^const_name>\<open>HOL.disj\<close>, _) $ _ $
                  (Const (\<^const_name>\<open>HOL.disj\<close>, _) $ (Const (\<^const_name>\<open>HOL.Not\<close>, _) $a) $ b)) => [a,b]
          | _ => [])
        val goal2 = (case concl2 of _ $ a => a)
      in
        if rule1 = equiv_pos2_rule andalso rule2 = th_resolution_rule andalso member (op =) prems2 id1
          andalso member (op =) goals1 goal2
        then
          mk_replay_node id2 theory_resolution2_rule args2 (filter_out (curry (op =) id1) prems2)
            proof_ctxt2 concl2 bounds2 insts2 declarations2
            (bound_sub2, assms_sub2, assms_extra2, combine_proof_steps subproof2) ::
          combine_proof_steps steps
        else
          mk_replay_node id1 rule1 args1 prems1
            proof_ctxt1 concl1 bounds1 insts1 declarations1
            (bound_sub1, assms_sub1, assms_extra1, combine_proof_steps subproof1) ::
          combine_proof_steps (step2 :: steps)
      end
  | combine_proof_steps steps = steps


val linearize_proof =
  let
    fun map_node_concl f (VeriT_Node {id, rule, prems, proof_ctxt, concl}) =
       mk_node id rule prems proof_ctxt (f concl)
    fun linearize (VeriT_Replay_Node {id = id, rule = rule, args = _, prems = prems,
        proof_ctxt = proof_ctxt, concl = concl, bounds = bounds, insts = _, declarations = _,
        subproof = (bounds', assms, inputs, subproof)}) =
      let
        val bounds = distinct (op =) bounds
        val bounds' = distinct (op =) bounds'
        fun mk_prop_of_term concl =
          concl |> fastype_of concl = \<^typ>\<open>bool\<close> ? curry (op $) \<^term>\<open>Trueprop\<close>
        fun remove_assumption_id assumption_id prems =
          filter_out (curry (op =) assumption_id) prems
        fun add_assumption assumption concl =
          \<^Const>\<open>Pure.imp for \<open>mk_prop_of_term assumption\<close> \<open>mk_prop_of_term concl\<close>\<close>
        fun inline_assumption assumption assumption_id
            (VeriT_Node {id, rule, prems, proof_ctxt, concl}) =
          mk_node id rule (remove_assumption_id assumption_id prems) proof_ctxt
            (add_assumption assumption concl)
        fun find_input_steps_and_inline [] = []
          | find_input_steps_and_inline
              (VeriT_Node {id = id', rule, prems, concl, ...} :: steps) =
            if rule = input_rule then
              find_input_steps_and_inline (map (inline_assumption concl id') steps)
            else
              mk_node (id') rule prems [] concl :: find_input_steps_and_inline steps

        fun free_bounds bounds (concl) =
          fold (fn (var, typ) => fn t => Logic.all (Free (var, typ)) t) bounds concl
        val subproof = subproof
          |> flat o map linearize
          |> map (map_node_concl (fold add_assumption (assms @ inputs)))
          |> map (map_node_concl (free_bounds (bounds @ bounds')))
          |> find_input_steps_and_inline
        val concl = free_bounds bounds concl
      in
        subproof @ [mk_node id rule prems proof_ctxt concl]
      end
  in linearize end

fun rule_of (VeriT_Replay_Node {rule,...}) = rule
fun subproof_of (VeriT_Replay_Node {subproof = (_, _, _, subproof),...}) = subproof


(* Massage Skolems for Sledgehammer.

We have to make sure that there is an "arrow" in the graph for skolemization steps.


A. The normal easy case

This function detects the steps of the form
  P \<longleftrightarrow> Q :skolemization
  Q       :resolution with P
and replace them by
  Q       :skolemization
Throwing away the step "P \<longleftrightarrow> Q" completely. This throws away a lot of information, but it does not
matter too much for Sledgehammer.


B. Skolems in subproofs
Supporting this is more or less hopeless as long as the Isar reconstruction of Sledgehammer
does not support more features like definitions. veriT is able to generate proofs with skolemization
happening in subproofs inside the formula.
  (assume "A \<or> P"
   ...
   P \<longleftrightarrow> Q :skolemization in the subproof
   ...)
  hence A \<or> P \<longrightarrow> A \<or> Q :lemma
  ...
  R :something with some rule
and replace them by
  R :skolemization with some rule
Without any subproof
*)
fun remove_skolem_definitions_proof steps =
  let
    fun replace_equivalent_by_imp (judgement $ ((Const(\<^const_name>\<open>HOL.eq\<close>, typ) $ arg1) $ arg2)) =
       judgement $ ((Const(\<^const_name>\<open>HOL.implies\<close>, typ) $ arg1) $ arg2)
     | replace_equivalent_by_imp a = a (*This case is probably wrong*)
    fun remove_skolem_definitions (VeriT_Replay_Node {id = id, rule = rule, args = args,
         prems = prems,
        proof_ctxt = proof_ctxt, concl = concl, bounds = bounds, insts = insts,
        declarations = declarations,
        subproof = (vars, assms', extra_assms', subproof)}) (prems_to_remove, skolems) =
    let
      val prems = prems
        |> filter_out (member (op =) prems_to_remove)
      val trivial_step = is_SH_trivial rule
      fun has_skolem_substep st NONE = if is_skolemization (rule_of st) then SOME (rule_of st)
             else fold has_skolem_substep (subproof_of st) NONE
        | has_skolem_substep _ a = a
      val promote_to_skolem = exists (fn t => member (op =) skolems t) prems
      val promote_from_assms = fold has_skolem_substep subproof NONE <> NONE
      val promote_step = promote_to_skolem orelse promote_from_assms
      val skolem_step_to_skip = is_skolemization rule orelse
        (promote_from_assms andalso length prems > 1)
      val is_skolem = is_skolemization rule orelse promote_step
      val prems = prems
        |> filter_out (fn t => member (op =) skolems t)
        |> is_skolem ? filter_out (String.isPrefix id)
      val rule = (if promote_step then default_skolem_rule else rule)
      val subproof = subproof
        |> (is_skolem ? K []) (*subproofs of skolemization steps are useless for SH*)
        |> map (fst o (fn st => remove_skolem_definitions st (prems_to_remove, skolems)))
             (*no new definitions in subproofs*)
        |> flat
      val concl = concl
        |> is_skolem ? replace_equivalent_by_imp
      val step = (if skolem_step_to_skip orelse rule = veriT_def orelse trivial_step then []
        else mk_replay_node id rule args prems proof_ctxt concl bounds insts declarations
            (vars, assms', extra_assms', subproof)
          |> single)
      val defs = (if rule = veriT_def orelse trivial_step then id :: prems_to_remove
         else prems_to_remove)
      val skolems = (if skolem_step_to_skip then id :: skolems else skolems)
    in
      (step, (defs, skolems))
    end
  in
    fold_map remove_skolem_definitions steps ([], [])
    |> fst
    |> flat
  end

local
  fun import_proof_and_post_process typs funs lines ctxt =
    let
      val compress = SMT_Config.compress_verit_proofs ctxt
      val smtlib_lines_without_qm =
        lines
        |> map single
        |> map SMTLIB.parse
        |> map remove_all_qm2
      val (raw_steps, _, _) =
        parse_raw_proof_steps NONE smtlib_lines_without_qm SMTLIB_Proof.empty_name_binding

      fun process step (cx, cx') =
        let fun postprocess step (cx, cx') =
          let val (step, cx) = postprocess_proof compress ctxt step cx
          in (step, (cx, cx')) end
        in uncurry (fold_map postprocess) (preprocess compress step (cx, cx')) end
      val step =
        (empty_context ctxt typs funs, [])
        |> fold_map process raw_steps
        |> (fn (steps, (cx, _)) => (flat steps, cx))
        |> compress? apfst combine_proof_steps
    in step end
in

fun parse typs funs lines ctxt =
  let
    val (u, env) = import_proof_and_post_process typs funs lines ctxt
    val t = u
       |> remove_skolem_definitions_proof
       |> flat o (map linearize_proof)
    fun node_to_step (VeriT_Node {id, rule, prems, concl, ...}) =
      mk_step id rule prems [] concl []
  in
    (map node_to_step t, ctxt_of env)
  end

fun parse_replay typs funs lines ctxt =
  let
    val (u, env) = import_proof_and_post_process typs funs lines ctxt
    val _ = (SMT_Config.verit_msg ctxt) (fn () => \<^print> u)
  in
    (u, ctxt_of env)
  end
end

end;
