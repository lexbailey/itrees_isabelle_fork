/*  Title:      Pure/Tools/profiling_report.scala
    Author:     Makarius

Report Poly/ML profiling information from session build database.
*/

package isabelle


object Profiling_Report
{
  def profiling_report(
    options: Options,
    session: String,
    theories: List[String] = Nil,
    clean_name: Boolean = false,
    progress: Progress = new Progress): Unit =
  {
    val store = Sessions.store(options)

    using(store.open_database_context())(db_context =>
    {
      val result =
        db_context.input_database(session)((db, name) => Some(store.read_theories(db, name)))
      result match {
        case None => error("Missing build database for session " + quote(session))
        case Some(used_theories) =>
          theories.filterNot(used_theories.toSet) match {
            case Nil =>
            case bad => error("Unknown theories " + commas_quote(bad))
          }
          val reports =
            (for {
              thy <- used_theories.iterator
              if theories.isEmpty || theories.contains(thy)
              command <- Build_Job.read_theory(db_context, List(session), thy).iterator
              snapshot = Document.State.init.snippet(command)
              (Protocol.ML_Profiling(report), _) <- snapshot.messages.iterator
            } yield if (clean_name) report.clean_name else report).toList

          for (report <- ML_Profiling.account(reports)) progress.echo(report.print)
      }
    })
  }


  /* Isabelle tool wrapper */

  val isabelle_tool =
    Isabelle_Tool("profiling_report", "report Poly/ML profiling information from log files",
      Scala_Project.here, args =>
    {
      var theories: List[String] = Nil
      var clean_name = false
      var options = Options.init()

      val getopts =
        Getopts("""
Usage: isabelle profiling_report [OPTIONS] SESSION

  Options are:
    -T NAME      restrict to given theories (multiple options possible)
    -c           clean function names
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

  Report Poly/ML profiling from the build database of the given session
  (without up-to-date check of sources).
""",
          "T:" -> (arg => theories = theories ::: List(arg)),
          "c" -> (_ => clean_name = true),
          "o:" -> (arg => options = options + arg))

      val more_args = getopts(args)
      val session_name =
        more_args match {
          case List(session_name) => session_name
          case _ => getopts.usage()
        }

      val progress = new Console_Progress()

      profiling_report(options, session_name, theories = theories,
        clean_name = clean_name, progress = progress)
    })
}
