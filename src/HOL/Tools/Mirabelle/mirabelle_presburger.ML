(*  Title:      HOL/Mirabelle/Tools/mirabelle_presburger.ML
    Author:     Martin Desharnais, MPI-INF Saarbrücken

Mirabelle action: "presburger".
*)

structure Mirabelle_Presburger: MIRABELLE_ACTION =
struct

fun make_action ({timeout, ...} : Mirabelle.action_context) =
  let
    fun run ({pre, ...} : Mirabelle.command) =
      (case Timing.timing (Mirabelle.can_apply timeout (Cooper.tac true [] [])) pre of
        ({cpu, ...}, true) => "succeeded (" ^ Time.toString cpu ^ " s)"
      | (_, false) => "failed")
  in ("", {run = run, finalize = K ""}) end

val () = Mirabelle.register_action "presburger" make_action

end
