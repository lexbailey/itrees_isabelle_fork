/*  Title:      Pure/Admin/ci_profile.scala
    Author:     Lars Hupel

Build profile for continuous integration services.
*/

package isabelle


import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.{Properties => JProperties, Map => JMap}


abstract class CI_Profile extends Isabelle_Tool.Body
{
  case class Result(rc: Int)
  case object Result
  {
    def ok: Result = Result(Process_Result.RC.ok)
    def error: Result = Result(Process_Result.RC.error)
  }

  private def build(options: Options): (Build.Results, Time) =
  {
    val progress = new Console_Progress(verbose = true)
    val start_time = Time.now()
    val results = progress.interrupt_handler {
      Build.build(
        options + "system_heaps",
        selection = selection,
        progress = progress,
        clean_build = clean,
        verbose = true,
        numa_shuffling = numa,
        max_jobs = jobs,
        dirs = include,
        select_dirs = select)
    }
    val end_time = Time.now()
    (results, end_time - start_time)
  }

  private def load_properties(): JProperties =
  {
    val props = new JProperties()
    val file_name = Isabelle_System.getenv("ISABELLE_CI_PROPERTIES")

    if (file_name != "")
    {
      val file = Path.explode(file_name).file
      if (file.exists())
        props.load(new java.io.FileReader(file))
      props
    }
    else
      props
  }

  private def compute_timing(results: Build.Results, group: Option[String]): Timing =
  {
    val timings = results.sessions.collect {
      case session if group.forall(results.info(session).groups.contains(_)) =>
        results(session).timing
    }
    timings.foldLeft(Timing.zero)(_ + _)
  }

  private def with_documents(options: Options): Options =
  {
    if (documents)
      options
        .bool.update("browser_info", true)
        .string.update("document", "pdf")
        .string.update("document_variants", "document:outline=/proof,/ML")
    else
      options
  }


  final def hg_id(path: Path): String =
    Mercurial.repository(path).id()

  final def print_section(title: String): Unit =
    println(s"\n=== $title ===\n")


  final val isabelle_home = Path.explode(Isabelle_System.getenv_strict("ISABELLE_HOME"))
  final val isabelle_id = hg_id(isabelle_home)
  final val start_time = Instant.now().atZone(ZoneId.systemDefault).format(DateTimeFormatter.RFC_1123_DATE_TIME)


  override final def apply(args: List[String]): Unit =
  {
    print_section("CONFIGURATION")
    println(Build_Log.Settings.show())
    val props = load_properties()
    System.getProperties().asInstanceOf[JMap[AnyRef, AnyRef]].putAll(props)

    val options =
      with_documents(Options.init())
        .int.update("parallel_proofs", 1)
        .int.update("threads", threads)

    println(s"jobs = $jobs, threads = $threads, numa = $numa")

    print_section("BUILD")
    println(s"Build started at $start_time")
    println(s"Isabelle id $isabelle_id")
    val pre_result = pre_hook(args)

    print_section("LOG")
    val (results, elapsed_time) = build(options)

    print_section("TIMING")

    val groups = results.sessions.map(results.info).flatMap(_.groups)
    for (group <- groups)
      println(s"Group $group: " + compute_timing(results, Some(group)).message_resources)

    val total_timing = compute_timing(results, None).copy(elapsed = elapsed_time)
    println("Overall: " + total_timing.message_resources)

    if (!results.ok) {
      print_section("FAILED SESSIONS")

      for (name <- results.sessions) {
        if (results.cancelled(name)) {
          println(s"Session $name: CANCELLED")
        }
        else {
          val result = results(name)
          if (!result.ok)
            println(s"Session $name: FAILED ${result.rc}")
        }
      }
    }

    val post_result = post_hook(results)

    System.exit(List(pre_result.rc, results.rc, post_result.rc).max)
  }

  /* profile */

  def threads: Int = Isabelle_System.hostname() match {
    case "hpcisabelle" => 8
    case "lxcisa1" => 4
    case _ => 2
  }

  def jobs: Int = Isabelle_System.hostname() match {
    case "hpcisabelle" => 8
    case "lxcisa1" => 10
    case _ => 2
  }

  def numa: Boolean = Isabelle_System.hostname() == "hpcisabelle"

  def documents: Boolean = true
  def clean: Boolean = true

  def include: List[Path]
  def select: List[Path]

  def pre_hook(args: List[String]): Result
  def post_hook(results: Build.Results): Result

  def selection: Sessions.Selection
}
