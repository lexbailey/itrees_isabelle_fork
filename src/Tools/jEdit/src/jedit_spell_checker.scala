/*  Title:      Tools/jEdit/src/jedit_spell_checker.scala
    Author:     Makarius

Specific spell-checker support for Isabelle/jEdit.
*/

package isabelle.jedit


import isabelle._

import javax.swing.JMenuItem
import scala.swing.ComboBox

import org.gjt.sp.jedit.menu.EnhancedMenuItem
import org.gjt.sp.jedit.jEdit
import org.gjt.sp.jedit.textarea.{JEditTextArea, TextArea}


object JEdit_Spell_Checker
{
  /* words within text */

  def current_word(text_area: TextArea, rendering: JEdit_Rendering, range: Text.Range)
    : Option[Text.Info[String]] =
  {
    for {
      spell_range <- rendering.spell_checker_point(range)
      text <- JEdit_Lib.try_get_text(text_area.getBuffer, spell_range)
      info <- Spell_Checker.marked_words(
        spell_range.start, text, info => info.range.overlaps(range)).headOption
    } yield info
  }


  /* completion */

  def completion(text_area: TextArea, explicit: Boolean, rendering: JEdit_Rendering)
      : Option[Completion.Result] =
  {
    for {
      spell_checker <- PIDE.spell_checker.get
      if explicit
      range = JEdit_Lib.before_caret_range(text_area, rendering)
      word <- current_word(text_area, rendering, range)
      words = spell_checker.complete(word.info)
      if words.nonEmpty
      descr = "(from dictionary " + quote(spell_checker.toString) + ")"
      items =
        words.map(w => Completion.Item(word.range, word.info, "", List(w, descr), w, 0, false))
    } yield Completion.Result(word.range, word.info, false, items)
  }


  /* context menu */

  def context_menu(text_area: JEditTextArea, offset: Text.Offset): List[JMenuItem] =
  {
    val result =
      for {
        spell_checker <- PIDE.spell_checker.get
        doc_view <- Document_View.get(text_area)
        rendering = doc_view.get_rendering()
        range = JEdit_Lib.point_range(text_area.getBuffer, offset)
        Text.Info(_, word) <- current_word(text_area, rendering, range)
      } yield (spell_checker, word)

    result match {
      case Some((spell_checker, word)) =>

        val context = jEdit.getActionContext()
        def item(name: String): JMenuItem =
          new EnhancedMenuItem(context.getAction(name).getLabel, name, context)

        val complete_items =
          if (spell_checker.complete_enabled(word)) List(item("isabelle.complete-word"))
          else Nil

        val update_items =
          if (spell_checker.check(word))
            List(item("isabelle.exclude-word"), item("isabelle.exclude-word-permanently"))
          else
            List(item("isabelle.include-word"), item("isabelle.include-word-permanently"))

        val reset_items =
          spell_checker.reset_enabled() match {
            case 0 => Nil
            case n =>
              val name = "isabelle.reset-words"
              val label = context.getAction(name).getLabel
              List(new EnhancedMenuItem(label + " (" + n + ")", name, context))
          }

        complete_items ::: update_items ::: reset_items

      case None => Nil
    }
  }


  /* dictionaries */

  def dictionaries_selector(): Option_Component =
  {
    GUI_Thread.require {}

    val option_name = "spell_checker_dictionary"
    val opt = PIDE.options.value.check_name(option_name)

    val entries = Spell_Checker.dictionaries()
    val component = new ComboBox(entries) with Option_Component {
      name = option_name
      val title = opt.title()
      def load: Unit =
      {
        val lang = PIDE.options.string(option_name)
        entries.find(_.lang == lang) match {
          case Some(entry) => selection.item = entry
          case None =>
        }
      }
      def save: Unit = PIDE.options.string(option_name) = selection.item.lang
    }

    component.load()
    component.tooltip = GUI.tooltip_lines(opt.print_default)
    component
  }
}