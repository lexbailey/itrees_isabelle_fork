/*  Title:      Pure/ML/ml_process.scala
    Author:     Makarius

The raw ML process.
*/

package isabelle


import java.util.{Map => JMap, HashMap}
import java.io.{File => JFile}


object ML_Process {
  def apply(options: Options,
    sessions_structure: Sessions.Structure,
    store: Sessions.Store,
    logic: String = "",
    raw_ml_system: Boolean = false,
    use_prelude: List[String] = Nil,
    eval_main: String = "",
    args: List[String] = Nil,
    modes: List[String] = Nil,
    cwd: JFile = null,
    env: JMap[String, String] = Isabelle_System.settings(),
    redirect: Boolean = false,
    cleanup: () => Unit = () => (),
    session_base: Option[Sessions.Base] = None
  ): Bash.Process = {
    val logic_name = Isabelle_System.default_logic(logic)

    val heaps: List[String] =
      if (raw_ml_system) Nil
      else {
        sessions_structure.selection(logic_name).
          build_requirements(List(logic_name)).
          map(a => File.platform_path(store.the_heap(a)))
      }

    val eval_init =
      if (heaps.isEmpty) {
        List(
          """
          fun chapter (_: string) = ();
          fun section (_: string) = ();
          fun subsection (_: string) = ();
          fun subsubsection (_: string) = ();
          fun paragraph (_: string) = ();
          fun subparagraph (_: string) = ();
          val ML_file = PolyML.use;
          """,
          if (Platform.is_windows)
            "fun exit 0 = OS.Process.exit OS.Process.success" +
            " | exit 1 = OS.Process.exit OS.Process.failure" +
            " | exit rc = OS.Process.exit (RunCall.unsafeCast (Word8.fromInt rc))"
          else
            "fun exit rc = Posix.Process.exit (Word8.fromInt rc)",
          "PolyML.Compiler.prompt1 := \"Poly/ML> \"",
          "PolyML.Compiler.prompt2 := \"Poly/ML# \"")
      }
      else
        List(
          "(PolyML.SaveState.loadHierarchy " +
            ML_Syntax.print_list(ML_Syntax.print_string_bytes)(heaps) +
          "; PolyML.print_depth 0) handle exn => (TextIO.output (TextIO.stdErr, General.exnMessage exn ^ " +
          ML_Syntax.print_string_bytes(": " + logic_name + "\n") +
          "); OS.Process.exit OS.Process.failure)")

    val eval_modes =
      if (modes.isEmpty) Nil
      else List("Print_Mode.add_modes " + ML_Syntax.print_list(ML_Syntax.print_string_bytes)(modes))


    // options
    val eval_options = if (heaps.isEmpty) Nil else List("Options.load_default ()")
    val isabelle_process_options = Isabelle_System.tmp_file("options")
    Isabelle_System.chmod("600", File.path(isabelle_process_options))
    File.write(isabelle_process_options, YXML.string_of_body(options.encode))

    // session base
    val (init_session_base, eval_init_session) =
      session_base match {
        case None => (sessions_structure.bootstrap, Nil)
        case Some(base) => (base, List("Resources.init_session_env ()"))
      }
    val init_session = Isabelle_System.tmp_file("init_session")
    Isabelle_System.chmod("600", File.path(init_session))
    File.write(init_session, new Resources(sessions_structure, init_session_base).init_session_yxml)

    // process
    val eval_process =
      proper_string(eval_main).getOrElse(
        if (heaps.isEmpty) {
          "PolyML.print_depth " + ML_Syntax.print_int(options.int("ML_print_depth"))
        }
        else "Isabelle_Process.init ()")

    // ISABELLE_TMP
    val isabelle_tmp = Isabelle_System.tmp_dir("process")

    val ml_runtime_options = {
      val ml_options = Word.explode(Isabelle_System.getenv("ML_OPTIONS"))
      val ml_options1 =
        if (ml_options.exists(_.containsSlice("gcthreads"))) ml_options
        else ml_options ::: List("--gcthreads", options.int("threads").toString)
      val ml_options2 =
        if (!Platform.is_windows || ml_options.exists(_.containsSlice("codepage"))) ml_options1
        else ml_options1 ::: List("--codepage", "utf8")
      ml_options2 ::: List("--exportstats")
    }

    // bash
    val bash_args =
      ml_runtime_options :::
      (eval_init ::: eval_modes ::: eval_options ::: eval_init_session).flatMap(List("--eval", _)) :::
      use_prelude.flatMap(List("--use", _)) ::: List("--eval", eval_process) ::: args

    val bash_env = new HashMap(env)
    bash_env.put("ISABELLE_PROCESS_OPTIONS", File.standard_path(isabelle_process_options))
    bash_env.put("ISABELLE_INIT_SESSION", File.standard_path(init_session))
    bash_env.put("ISABELLE_TMP", File.standard_path(isabelle_tmp))
    bash_env.put("POLYSTATSDIR", isabelle_tmp.getAbsolutePath)

    Bash.process(
      options.string("ML_process_policy") + """ "$ML_HOME/poly" -q """ +
        Bash.strings(bash_args),
      cwd = cwd,
      env = bash_env,
      redirect = redirect,
      cleanup = { () =>
        isabelle_process_options.delete
        init_session.delete
        Isabelle_System.rm_tree(isabelle_tmp)
        cleanup()
      })
  }


  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("process", "raw ML process (batch mode)",
    Scala_Project.here,
    { args =>
      var dirs: List[Path] = Nil
      var eval_args: List[String] = Nil
      var logic = Isabelle_System.getenv("ISABELLE_LOGIC")
      var modes: List[String] = Nil
      var options = Options.init()

      val getopts = Getopts("""
Usage: isabelle process [OPTIONS]

  Options are:
    -T THEORY    load theory
    -d DIR       include session directory
    -e ML_EXPR   evaluate ML expression on startup
    -f ML_FILE   evaluate ML file on startup
    -l NAME      logic session name (default ISABELLE_LOGIC=""" + quote(logic) + """)
    -m MODE      add print mode for output
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

  Run the raw Isabelle ML process in batch mode.
""",
        "T:" -> (arg =>
          eval_args = eval_args ::: List("--eval", "use_thy " + ML_Syntax.print_string_bytes(arg))),
        "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
        "e:" -> (arg => eval_args = eval_args ::: List("--eval", arg)),
        "f:" -> (arg => eval_args = eval_args ::: List("--use", arg)),
        "l:" -> (arg => logic = arg),
        "m:" -> (arg => modes = arg :: modes),
        "o:" -> (arg => options = options + arg))

      val more_args = getopts(args)
      if (args.isEmpty || more_args.nonEmpty) getopts.usage()

      val base_info = Sessions.base_info(options, logic, dirs = dirs).check
      val store = Sessions.store(options)
      val result =
        ML_Process(options, base_info.sessions_structure, store, logic = logic, args = eval_args,
          modes = modes, session_base = Some(base_info.base)).result(
            progress_stdout = Output.writeln(_, stdout = true),
            progress_stderr = Output.writeln(_))

      sys.exit(result.rc)
    })
}
