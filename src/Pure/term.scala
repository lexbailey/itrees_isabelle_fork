/*  Title:      Pure/term.scala
    Author:     Makarius

Lambda terms, types, sorts.

Note: Isabelle/ML is the primary environment for logical operations.
*/

package isabelle


object Term
{
  /* types and terms */

  type Indexname = (String, Int)

  type Sort = List[String]
  val dummyS: Sort = List("")

  sealed abstract class Typ
  case class Type(name: String, args: List[Typ] = Nil) extends Typ
  case class TFree(name: String, sort: Sort = dummyS) extends Typ
  case class TVar(name: Indexname, sort: Sort = dummyS) extends Typ
  val dummyT = Type("dummy")

  sealed abstract class Term
  case class Const(name: String, typ: Typ = dummyT) extends Term
  case class Free(name: String, typ: Typ = dummyT) extends Term
  case class Var(name: Indexname, typ: Typ = dummyT) extends Term
  case class Bound(index: Int) extends Term
  case class Abs(name: String, typ: Typ = dummyT, body: Term) extends Term
  case class App(fun: Term, arg: Term) extends Term


  /* Pure logic */

  def itselfT(ty: Typ): Typ = Type(Pure_Thy.ITSELF, List(ty))
  val propT: Typ = Type(Pure_Thy.PROP, Nil)
  def funT(ty1: Typ, ty2: Typ): Typ = Type(Pure_Thy.FUN, List(ty1, ty2))

  def mk_type(ty: Typ): Term = Const(Pure_Thy.TYPE, itselfT(ty))

  def const_of_class(c: String): String = c + "_class"

  def mk_of_sort(ty: Typ, s: Sort): List[Term] =
  {
    val class_type = funT(itselfT(ty), propT)
    val t = mk_type(ty)
    s.map(c => App(Const(const_of_class(c), class_type), t))
  }


  /* type arguments of consts */

  def const_typargs(name: String, typ: Typ, typargs: List[String], decl: Typ): List[Typ] =
  {
    var subst = Map.empty[String, Typ]

    def bad_match(): Nothing = error("Malformed type instance for " + name + ": " + typ)
    def raw_match(arg: (Typ, Typ))
    {
      arg match {
        case (TFree(a, _), ty) =>
          subst.get(a) match {
            case None => subst += (a -> ty)
            case Some(ty1) => if (ty != ty1) bad_match()
          }
        case (Type(c1, args1), Type(c2, args2)) if c1 == c2 =>
          (args1 zip args2).foreach(raw_match)
        case _ => bad_match()
      }
    }
    raw_match(decl, typ)

    typargs.map(subst)
  }



  /** cache **/

  def make_cache(initial_size: Int = 131071, max_string: Int = Integer.MAX_VALUE): Cache =
    new Cache(initial_size, max_string)

  class Cache private[Term](initial_size: Int, max_string: Int)
    extends isabelle.Cache(initial_size, max_string)
  {
    protected def cache_indexname(x: Indexname): Indexname =
      lookup(x) getOrElse store(cache_string(x._1), cache_int(x._2))

    protected def cache_sort(x: Sort): Sort =
      if (x == dummyS) dummyS
      else lookup(x) getOrElse store(x.map(cache_string(_)))

    protected def cache_typ(x: Typ): Typ =
    {
      if (x == dummyT) dummyT
      else
        lookup(x) match {
          case Some(y) => y
          case None =>
            x match {
              case Type(name, args) => store(Type(cache_string(name), args.map(cache_typ(_))))
              case TFree(name, sort) => store(TFree(cache_string(name), cache_sort(sort)))
              case TVar(name, sort) => store(TVar(cache_indexname(name), cache_sort(sort)))
            }
        }
    }

    protected def cache_term(x: Term): Term =
    {
      lookup(x) match {
        case Some(y) => y
        case None =>
          x match {
            case Const(name, typ) => store(Const(cache_string(name), cache_typ(typ)))
            case Free(name, typ) => store(Free(cache_string(name), cache_typ(typ)))
            case Var(name, typ) => store(Var(cache_indexname(name), cache_typ(typ)))
            case Bound(index) => store(Bound(cache_int(index)))
            case Abs(name, typ, body) =>
              store(Abs(cache_string(name), cache_typ(typ), cache_term(body)))
            case App(fun, arg) => store(App(cache_term(fun), cache_term(arg)))
          }
      }
    }

    // main methods
    def indexname(x: Indexname): Indexname = synchronized { cache_indexname(x) }
    def sort(x: Sort): Sort = synchronized { cache_sort(x) }
    def typ(x: Typ): Typ = synchronized { cache_typ(x) }
    def term(x: Term): Term = synchronized { cache_term(x) }

    def position(x: Position.T): Position.T =
      synchronized { x.map({ case (a, b) => (cache_string(a), cache_string(b)) }) }
  }
}
