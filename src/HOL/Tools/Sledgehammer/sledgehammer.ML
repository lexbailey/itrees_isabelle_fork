(*  Title:      HOL/Tools/Sledgehammer/sledgehammer.ML
    Author:     Fabian Immler, TU Muenchen
    Author:     Makarius
    Author:     Jasmin Blanchette, TU Muenchen

Sledgehammer's heart.
*)

signature SLEDGEHAMMER =
sig
  type stature = ATP_Problem_Generate.stature
  type fact = Sledgehammer_Fact.fact
  type fact_override = Sledgehammer_Fact.fact_override
  type proof_method = Sledgehammer_Proof_Methods.proof_method
  type play_outcome = Sledgehammer_Proof_Methods.play_outcome
  type mode = Sledgehammer_Prover.mode
  type params = Sledgehammer_Prover.params
  type induction_rules = Sledgehammer_Prover.induction_rules
  type prover_problem = Sledgehammer_Prover.prover_problem
  type prover_result = Sledgehammer_Prover.prover_result

  type preplay_result = proof_method * play_outcome * (string * stature) list

  datatype sledgehammer_outcome =
    SH_Some of prover_result * preplay_result list
  | SH_Unknown
  | SH_Timeout
  | SH_None

  val short_string_of_sledgehammer_outcome : sledgehammer_outcome -> string
  val string_of_factss : (string * fact list) list -> string
  val run_sledgehammer : params -> mode -> (string -> unit) option -> int -> fact_override ->
    Proof.state -> bool * (sledgehammer_outcome * string)
end;

structure Sledgehammer : SLEDGEHAMMER =
struct

open ATP_Util
open ATP_Problem
open ATP_Proof
open ATP_Problem_Generate
open Sledgehammer_Util
open Sledgehammer_Fact
open Sledgehammer_Proof_Methods
open Sledgehammer_Isar_Proof
open Sledgehammer_Isar_Preplay
open Sledgehammer_Isar_Minimize
open Sledgehammer_ATP_Systems
open Sledgehammer_Prover
open Sledgehammer_Prover_ATP
open Sledgehammer_Prover_Minimize
open Sledgehammer_MaSh

type preplay_result = proof_method * play_outcome * (string * stature) list

datatype sledgehammer_outcome =
  SH_Some of prover_result * preplay_result list
| SH_Unknown
| SH_Timeout
| SH_None

fun short_string_of_sledgehammer_outcome (SH_Some _) = "some"
  | short_string_of_sledgehammer_outcome SH_Unknown = "unknown"
  | short_string_of_sledgehammer_outcome SH_Timeout = "timeout"
  | short_string_of_sledgehammer_outcome SH_None = "none"

fun alternative f (SOME x) (SOME y) = SOME (f (x, y))
  | alternative _ (x as SOME _) NONE = x
  | alternative _ NONE (y as SOME _) = y
  | alternative _ NONE NONE = NONE

fun max_outcome outcomes =
  let
    val some = find_first (fn (SH_Some _, _) => true | _ => false) outcomes
    val unknown = find_first (fn (SH_Unknown, _) => true | _ => false) outcomes
    val timeout = find_first (fn (SH_Timeout, _) => true | _ => false) outcomes
    val none = find_first (fn (SH_None, _) => true | _ => false) outcomes
  in
    some
    |> alternative snd unknown
    |> alternative snd timeout
    |> alternative snd none
    |> the_default (SH_Unknown, "")
  end

fun play_one_line_proofs minimize timeout used_facts state i methss =
  (if timeout = Time.zeroTime then
     []
   else
     let
       val ctxt = Proof.context_of state
       val used_facts = filter_out (fn (_, (sc, _)) => sc = Chained) used_facts
       val fact_names = map fst used_facts
       val {facts = chained, goal, ...} = Proof.goal state
       val goal_t = Logic.get_goal (Thm.prop_of goal) i

       fun try_methss ress [] = ress
         | try_methss ress (meths :: methss) =
           let
             fun mk_step meths =
               Prove {
                 qualifiers = [],
                 obtains = [],
                 label = ("", 0),
                 goal = goal_t,
                 subproofs = [],
                 facts = ([], fact_names),
                 proof_methods = meths,
                 comment = ""}
             val ress' =
               preplay_isar_step ctxt chained timeout [] (mk_step meths)
               |> map (fn (meth, play_outcome) =>
                  (case (minimize, play_outcome) of
                    (true, Played time) =>
                    let
                      val (time', used_names') =
                        minimized_isar_step ctxt chained time (mk_step [meth])
                        ||> (facts_of_isar_step #> snd)
                      val used_facts' = filter (member (op =) used_names' o fst) used_facts
                    in
                      (meth, Played time', used_facts')
                    end
                  | _ => (meth, play_outcome, used_facts)))
             val any_succeeded = exists (fn (_, Played _, _) => true | _ => false) ress'
           in
             try_methss (ress' @ ress) (if any_succeeded then [] else methss)
           end
     in
       try_methss [] methss
     end)
  |> map (fn (meth, play_outcome, used_facts) => (meth, play_outcome, filter_out (fn (_, (sc, _)) => sc = Chained) used_facts))
  |> sort (play_outcome_ord o apply2 (fn (_, play_outcome, _) => play_outcome))

fun select_one_line_proof used_facts preferred_meth preplay_results =
  (case preplay_results of
    (* Select best method if preplay succeeded *)
    (best_meth, best_outcome as Played _, best_used_facts) :: _ =>
    (best_used_facts, (best_meth, best_outcome))
    (* Otherwise select preferred method *)
  | (fst_meth, fst_outcome, _) :: _ =>
    (used_facts, (preferred_meth,
       if fst_meth = preferred_meth then fst_outcome else Play_Timed_Out Time.zeroTime))
  | [] => (used_facts, (preferred_meth, Play_Timed_Out Time.zeroTime)))
  |> apfst (filter_out (fn (_, (sc, _)) => sc = Chained))

fun launch_prover (params as {verbose, spy, slices, timeout, ...}) mode learn
    (problem as {state, subgoal, factss, ...} : prover_problem)
    (slice as ((slice_size, num_facts, fact_filter), _)) name =
  let
    val ctxt = Proof.context_of state

    val _ = spying spy (fn () => (state, subgoal, name, "Launched"))

    val _ =
      if verbose then
        writeln (name ^ " with " ^ string_of_int num_facts ^ " " ^ fact_filter ^ " fact" ^
          plural_s num_facts ^ " for " ^ string_of_time (slice_timeout slice_size slices timeout) ^
          "...")
      else
        ()

    fun print_used_facts used_facts used_from =
      tag_list 1 used_from
      |> map (fn (j, fact) => fact |> apsnd (K j))
      |> filter_used_facts false used_facts
      |> map (fn ((name, _), j) => name ^ "@" ^ string_of_int j)
      |> commas
      |> prefix ("Facts in " ^ name ^ " proof: ")
      |> writeln

    fun spying_str_of_res ({outcome = NONE, used_facts, used_from, ...} : prover_result) =
        let
          val num_used_facts = length used_facts

          fun find_indices facts =
            tag_list 1 facts
            |> map (fn (j, fact) => fact |> apsnd (K j))
            |> filter_used_facts false used_facts
            |> distinct (eq_fst (op =))
            |> map (prefix "@" o string_of_int o snd)

          fun filter_info (fact_filter, facts) =
            let
              val indices = find_indices facts
              (* "Int.max" is there for robustness *)
              val unknowns = replicate (Int.max (0, num_used_facts - length indices)) "?"
            in
              (commas (indices @ unknowns), fact_filter)
            end

          val filter_infos =
            map filter_info (("actual", used_from) :: factss)
            |> AList.group (op =)
            |> map (fn (indices, fact_filters) => commas fact_filters ^ ": " ^ indices)
        in
          "Success: Found proof with " ^ string_of_int num_used_facts ^ " fact" ^
          plural_s num_used_facts ^
          (if num_used_facts = 0 then "" else ": " ^ commas filter_infos)
        end
      | spying_str_of_res {outcome = SOME failure, ...} =
        "Failure: " ^ string_of_atp_failure failure
 in
   get_minimizing_prover ctxt mode learn name params problem slice
   |> verbose ? tap (fn {outcome = NONE, used_facts as _ :: _, used_from, ...} =>
       print_used_facts used_facts used_from
     | _ => ())
   |> spy ? tap (fn res => spying spy (fn () => (state, subgoal, name, spying_str_of_res res)))
 end

fun preplay_prover_result ({ minimize, preplay_timeout, ...} : params) state subgoal
    (result as {outcome, used_facts, preferred_methss, message, ...} : prover_result) =
  let
    val (output, chosen_preplay_outcome) =
      if outcome = SOME ATP_Proof.TimedOut then
        (SH_Timeout, select_one_line_proof used_facts (fst preferred_methss) [])
      else if is_some outcome then
        (SH_None, select_one_line_proof used_facts (fst preferred_methss) [])
      else
        let
          val preplay_results =
            play_one_line_proofs minimize preplay_timeout used_facts state subgoal
              (snd preferred_methss)
          val chosen_preplay_outcome =
            select_one_line_proof used_facts (fst preferred_methss) preplay_results
        in
          (SH_Some (result, preplay_results), chosen_preplay_outcome)
        end
    fun output_message () = message (fn () => chosen_preplay_outcome)
  in
    (output, output_message)
  end

fun check_expected_outcome ctxt prover_name expect outcome =
  let
    val outcome_code = short_string_of_sledgehammer_outcome outcome
  in
    (* The "expect" argument is deliberately ignored if the prover is missing so that
       "Metis_Examples" can be processed on any machine. *)
    if expect = "" orelse not (is_prover_installed ctxt prover_name) then
      ()
    else
      (case (expect, outcome) of
        ("some", SH_Some _) => ()
      | ("some_preplayed", SH_Some (_, preplay_results)) =>
          if exists (fn (_, Played _, _) => true | _ => false) preplay_results then
            ()
          else
            error ("Unexpected outcome: the external prover found a some proof but preplay failed")
      | ("unknown", SH_Unknown) => ()
      | ("timeout", SH_Timeout) => ()
      | ("none", SH_None) => ()
      | _ => error ("Unexpected outcome: " ^ quote outcome_code))
  end

fun launch_prover_and_preplay (params as {debug, timeout, expect, ...}) mode writeln_result learn
    (problem as {state, subgoal, ...}) slice prover_name =
  let
    val ctxt = Proof.context_of state
    val hard_timeout = Time.scale 5.0 timeout

    fun really_go () =
      launch_prover params mode learn problem slice prover_name
      |> preplay_prover_result params state subgoal

    fun go () =
      if debug then
        really_go ()
      else
        (really_go ()
         handle
           ERROR msg => (SH_Unknown, fn () => "Warning: " ^ msg ^ "\n")
         | exn =>
           if Exn.is_interrupt exn then Exn.reraise exn
           else (SH_Unknown, fn () => "Internal error:\n" ^ Runtime.exn_message exn ^ "\n"))

    val (outcome, message) = Timeout.apply hard_timeout go ()
    val () = check_expected_outcome ctxt prover_name expect outcome

    val message = message ()
    val () =
      if mode = Auto_Try then
        ()
      else
        (case outcome of
          SH_Some _ => the_default writeln writeln_result (prover_name ^ ": " ^ message)
        | _ => ())
  in
    (outcome, message)
  end

fun string_of_facts filter facts =
  "Selected " ^ string_of_int (length facts) ^ " " ^ (if filter = "" then "" else filter ^ " ") ^
  "fact" ^ plural_s (length facts) ^ ": " ^ (space_implode " " (map (fst o fst) facts))

fun string_of_factss factss =
  if forall (null o snd) factss then
    "Found no relevant facts"
  else
    cat_lines (map (fn (filter, facts) => string_of_facts filter facts) factss)

val default_slice_schedule =
  (* FUDGE (inspired by Seventeen evaluation) *)
  [cvc4N, zipperpositionN, vampireN, veritN, eN, cvc4N, zipperpositionN, cvc4N, vampireN, cvc4N,
   cvc4N, vampireN, cvc4N, iproverN, zipperpositionN, vampireN, vampireN, zipperpositionN, z3N,
   zipperpositionN, vampireN, iproverN, vampireN, cvc4N, z3N, z3N, cvc4N, cvc4N]

fun schedule_of_provers provers num_slices =
  let
    val (known_provers, unknown_provers) =
      List.partition (member (op =) default_slice_schedule) provers

    val default_slice_schedule = filter (member (op =) known_provers) default_slice_schedule
    val num_default_slices = length default_slice_schedule

    fun round_robin _ [] = []
      | round_robin 0 _ = []
      | round_robin n (prover :: provers) = prover :: round_robin (n - 1) (provers @ [prover])
  in
    if num_slices <= num_default_slices then
      take num_slices default_slice_schedule
    else
      default_slice_schedule
      @ round_robin (num_slices - num_default_slices) (unknown_provers @ known_provers)
  end

fun prover_slices_of_schedule ctxt factss
    ({max_facts, fact_filter, type_enc, lam_trans, uncurried_aliases, ...} : params) schedule =
  let
    fun triplicate_slices original =
      let
        val shift =
          map (apfst (fn (slice_size, num_facts, fact_filter) =>
            (slice_size, num_facts,
             if fact_filter = mashN then mepoN
             else if fact_filter = mepoN then meshN
             else mashN)))

        val shifted_once = shift original
        val shifted_twice = shift shifted_once
      in
        original @ shifted_once @ shifted_twice
      end

    fun adjust_extra (ATP_Slice (format0, type_enc0, lam_trans0, uncurried_aliases0,
        extra_extra0)) =
        ATP_Slice (format0, the_default type_enc0 type_enc, the_default lam_trans0 lam_trans,
          the_default uncurried_aliases0 uncurried_aliases, extra_extra0)
      | adjust_extra (extra as SMT_Slice _) = extra

    fun adjust_slice max_slice_size ((slice_size0, num_facts0, fact_filter0), extra) =
      let
        val slice_size = Int.min (max_slice_size, slice_size0)
        val fact_filter = fact_filter |> the_default fact_filter0
        val max_facts = max_facts |> the_default num_facts0
        val num_facts = Int.min (max_facts, length (facts_of_filter fact_filter factss))
      in
        ((slice_size, num_facts, fact_filter), adjust_extra extra)
      end

    val provers = distinct (op =) schedule
    val prover_slices =
      map (fn prover => (prover,
          (is_none fact_filter ? triplicate_slices) (get_slices ctxt prover)))
        provers

    val max_threads = Multithreading.max_threads ()

    fun translate_schedule _ 0 _ = []
      | translate_schedule _ _ [] = []
      | translate_schedule prover_slices slices_left (prover :: schedule) =
        (case AList.lookup (op =) prover_slices prover of
          SOME (slice0 :: slices) =>
          let
            val prover_slices' = AList.update (op =) (prover, slices) prover_slices
            val slice as ((slice_size, _, _), _) =
              adjust_slice ((slices_left + max_threads - 1) div max_threads) slice0
          in
            (prover, slice) :: translate_schedule prover_slices' (slices_left - slice_size) schedule
          end
        | _ => translate_schedule prover_slices slices_left schedule)
  in
    translate_schedule prover_slices (length schedule) schedule
    |> distinct (op =)
  end

fun run_sledgehammer (params as {verbose, spy, provers, induction_rules, max_facts, max_proofs,
      slices, ...})
    mode writeln_result i (fact_override as {only, ...}) state =
  if null provers then
    error "No prover is set"
  else
    (case subgoal_count state of
      0 => (error "No subgoal!"; (false, (SH_None, "")))
    | n =>
      let
        val _ = Proof.assert_backward state
        val print = if mode = Normal andalso is_none writeln_result then writeln else K ()

        val found_proofs = Synchronized.var "found_proofs" 0

        fun found_proof prover_name =
          if mode = Normal then
            (Synchronized.change found_proofs (fn n => n + 1);
             (the_default writeln writeln_result) (prover_name ^ " found a proof..."))
          else
            ()

        val ctxt = Proof.context_of state
        val inst_inducts = induction_rules = SOME Instantiate
        val {facts = chained_thms, goal, ...} = Proof.goal state
        val (_, hyp_ts, concl_t) = strip_subgoal goal i ctxt
        val _ =
          (case find_first (not o is_prover_supported ctxt) provers of
            SOME name => error ("No such prover: " ^ name)
          | NONE => ())
        val _ = print "Sledgehammering..."
        val _ = spying spy (fn () => (state, i, "***", "Starting " ^ str_of_mode mode ^ " mode"))
        val ({elapsed, ...}, all_facts) = Timing.timing
          (nearly_all_facts_of_context ctxt inst_inducts fact_override chained_thms hyp_ts) concl_t
        val _ = spying spy (fn () => (state, i, "All",
          "Extracting " ^ string_of_int (length all_facts) ^ " facts from background theory in " ^
          string_of_int (Time.toMilliseconds elapsed) ^ " ms"))

        val spying_str_of_factss =
          commas o map (fn (filter, facts) => filter ^ ": " ^ string_of_int (length facts))

        fun get_factss provers =
          let
            val max_max_facts =
              (case max_facts of
                SOME n => n
              | NONE =>
                fold (fn prover =>
                    fold (fn ((_, n, _), _) => Integer.max n) (get_slices ctxt prover))
                  provers 0)
              * 51 div 50  (* some slack to account for filtering of induction facts below *)

            val ({elapsed, ...}, factss) = Timing.timing
              (relevant_facts ctxt params (hd provers) max_max_facts fact_override hyp_ts concl_t)
              all_facts

            val induction_rules = the_default (if only then Include else Exclude) induction_rules
            val factss = map (apsnd (maybe_filter_out_induction_rules induction_rules)) factss

            val () = spying spy (fn () => (state, i, "All",
              "Filtering facts in " ^ string_of_int (Time.toMilliseconds elapsed) ^
              " ms (MaSh algorithm: " ^ str_of_mash_algorithm (the_mash_algorithm ()) ^ ")"));
            val () = if verbose then print (string_of_factss factss) else ()
            val () = spying spy (fn () =>
              (state, i, "All", "Selected facts: " ^ spying_str_of_factss factss))
          in
            factss
          end

        fun launch_provers () =
          let
            val factss = get_factss provers
            val problem =
              {comment = "", state = state, goal = goal, subgoal = i, subgoal_count = n,
               factss = factss, found_proof = found_proof}
            val learn = mash_learn_proof ctxt params (Thm.prop_of goal)
            val launch = launch_prover_and_preplay params mode writeln_result learn

            val schedule =
              if mode = Auto_Try then provers
              else schedule_of_provers provers slices
            val prover_slices = prover_slices_of_schedule ctxt factss params schedule

            val _ =
              if verbose then
                writeln ("Running " ^ commas (map fst prover_slices) ^ "...")
              else
                ()
          in
            if mode = Auto_Try then
              (SH_Unknown, "")
              |> fold (fn (prover, slice) =>
                  fn accum as (SH_Some _, _) => accum
                    | _ => launch problem slice prover)
                prover_slices
            else
              (learn chained_thms;
               Par_List.map (fn (prover, slice) =>
                   if Synchronized.value found_proofs < max_proofs then
                     launch problem slice prover
                   else
                     (SH_None, ""))
                 prover_slices
               |> max_outcome)
          end
      in
        (launch_provers ()
         handle Timeout.TIMEOUT _ => (SH_Timeout, ""))
        |> `(fn (outcome, message) =>
          (case outcome of
            SH_Some _ => (the_default writeln writeln_result "QED"; true)
          | SH_Unknown => (the_default writeln writeln_result message; false)
          | SH_Timeout => (the_default writeln writeln_result "No proof found"; false)
          | SH_None => (the_default writeln writeln_result
                (if message = "" then "No proof found" else "Warning: " ^ message);
              false)))
      end)

end;
