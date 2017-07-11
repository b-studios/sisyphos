import sbt.IO.append
import scala.io.AnsiColor._

/**
 * Run with filename (e.g. `exam.scores`) as first and only argument.
 *
 * Once you are in answer entering mode, there is a set of commands:
 *
 *  :edit student-id|exam-id
 *  :exit
 *  :help
 *  :reenter [idx] (:r)
 **/
object Enter extends App {

  import controlParsers._

  // A string representing crossed answers for one MC question
  // Examples: "", "ac", "acd"
  type Answer = String
  type Answers = List[String]

  def prompt[T](f: => T): T = {
    print("> ")
    f
  }

  val logo =
     """   _______ _________ _______                 _______           _______  _______
       ^  (  ____ \\__   __/(  ____ \   |\     /|   (  ____ )|\     /|(  ___  )(  ____ \
       ^  | (    \/   ) (   | (    \/   ( \   / )   | (    )|| )   ( || (   ) || (    \/
       ^  | (_____    | |   | (_____     \ (_) /    | (____)|| (___) || |   | || (_____
       ^  (_____  )   | |   (_____  )     \   /     |  _____)|  ___  || |   | |(_____  )
       ^        ) |   | |         ) |      ) (      | (      | (   ) || |   | |      ) |
       ^  /\____) |___) (___/\____) |      | |      | )      | )   ( || (___) |/\____) |
       ^  \_______)\_______/\_______)      \_/      |/       |/     \|(_______)\_______)
       ^
       ^      _______  _______  _        _______  _______  ______   _______  ______
       ^     (  ____ )(  ____ \( \      (  ___  )(  ___  )(  __  \ (  ____ \(  __  \
       ^     | (    )|| (    \/| (      | (   ) || (   ) || (  \  )| (    \/| (  \  )
       ^     | (____)|| (__    | |      | |   | || (___) || |   ) || (__    | |   ) |
       ^     |     __)|  __)   | |      | |   | ||  ___  || |   | ||  __)   | |   | |
       ^     | (\ (   | (      | |      | |   | || (   ) || |   ) || (      | |   ) |
       ^     | ) \ \__| (____/\| (____/\| (___) || )   ( || (__/  )| (____/\| (__/  )
       ^     |/   \__/(_______/(_______/(_______)|/     \|(______/ (_______/(______/
       ^""".stripMargin('^')

  val banner =
     s"""$RED $BOLD
        ^$logo
        ^$RESET
        ^This tool allows you to enter answers to multiple-choice-exams.
        ^
        ^It will always only append to the given file, so don't worry.
        ^""".stripMargin('^')

  val sep = "────────────────────────────────────────────────────────────────\n"


  if (args.size < 1) {
    println("""Please invoke tool with name of the score file as first argument (e.g.):
              |   score my-score-file.scores
              |""".stripMargin('|'))
    System.exit(1)
  }

  val scoreFile = new java.io.File(args(0))

  println(banner)

  println(sep)

  println("Let me briefly ask you a few things about the exam:\n")

  println("How many questions does the exam have?")
  val questionCount = prompt(readInt)

  println("What is the highest possible answer in the exam (e.g. 'a' - 'z')?")
  val maxAnswer = prompt(readChar)

  println("You can now start entering data, for a list of available commands, type :help")

  println(sep)

  enterExams

  class Exam {

    var studentid: String = ""
    var examid: String = ""

    editStudentId
    editExamId

    var answers: Answers = Nil

    def input(k: String => Unit): Unit = prompt(readLine) match {
      case ReEnterN(n) =>
        dropLast(n)
        enterAnswers
      case ReEnter() =>
        dropLast()
        enterAnswers
      case Edit(StudentId) =>
        editStudentId
        enterAnswers
      case Edit(ExamId) =>
        editExamId
        enterAnswers
      case Exit() => System.exit(0)
      case Help() =>
        println(Cmd.toString + "\n")
        input(k)
      case UnkownCommand(cmd) =>
        println(s"I don't understand command '${cmd}', enter ':help' for available commands.")
        input(k)
      case in =>
        if (! in.toSet.forall { _ <= maxAnswer }) {
          println("Not a valid answer")
          enterAnswers
        } else {
          k(in.toSet.mkString(""))
        }
    }

    def enterAnswers: Unit = {
      if (done) {
        return;
      }

      println(s"\n${BOLD}${RED}Question $currQuestion ${RESET}($examid):")
      input { answer =>
        val answerOrDash = if (answer == "") "-" else answer
        answers = answers :+ answerOrDash
        enterAnswers
      }
    }

    def editStudentId: Unit = {
      println("Please enter student id:")
      studentid = prompt(readLine)
    }

    def editExamId: Unit = {
      println("Please enter the four-digit id of the exam (e.g. 4335):")
      examid = prompt(readLine)
    }

    def dropLast(n: Int = 1): Unit = {
      answers = answers.slice(0, answers.size - n)
    }

    def writeToFile: Unit = {
      append(scoreFile, s"$studentid $examid ${formatAnswers(answers)}\n")
    }

    def formatAnswers(ans: Answers): String =
      ans.zipWithIndex map { case (el, idx) => (idx + 1).toString + el } mkString ""

    def currQuestion = answers.size + 1
    def done: Boolean = currQuestion > questionCount
  }

  def enterExams: Unit = {
    println(s"\n${BOLD}${YELLOW}Next Student${RESET}")

    val exam = new Exam
    exam.enterAnswers
    exam.writeToFile

    enterExams
  }

}

object controlParsers {

  sealed trait Field
  object StudentId extends Field
  object ExamId extends Field

  class Cmd(val signature: String, val description: String) {
    def printPadded(col: Int): String =
      signature + (" " * (col - signature.size)) + description
  }
  object Cmd {
    val cmds = Edit :: Exit :: Help :: ReEnter :: Nil
    val margin = 5

    override def toString: String = {
      val maxLength = cmds.map{ _.signature.size }.max + margin
      ("Some commands can be abbreviated; Abbreviation given in parenthesis." ::
       cmds.map(_.printPadded(maxLength))) mkString "\n"
    }
  }

  object Edit extends Cmd (
    ":edit student-id|exam-id",
    "edit the meta data of the current exam"
  ) {
    def unapply(s: String): Option[Field] = splitToList(s) match {
      case ":edit" :: "student-id" :: Nil |
           ":edit" :: "matr" :: Nil |
           ":edit" :: "matrnr" :: Nil => Some(StudentId)

      case ":edit" :: "id" :: Nil |
           ":edit" :: "exam-id" :: Nil => Some(ExamId)

      case _ => None
    }
  }

  object Exit extends Cmd (
    ":exit",
    "exit the exam-entering, do not save current exam"
  ) {
    def unapply(s: String): Boolean = splitToList(s) match {
      case ":exit" :: Nil => true
      case _ => false
    }
  }

  object Help extends Cmd (
    ":help (:h)",
    "shows this summary of available commands"
  ) {
    def unapply(s: String): Boolean = splitToList(s) match {
      case ":help" :: Nil => true
      case ":h" :: Nil => true
      case _ => false
    }
  }


  object ReEnter extends Cmd (
    ":reenter [n] (:r)",
    "reenter the answers to the n-last questions (n=1, if omitted)"
  ) {
    def unapply(s: String): Boolean = splitToList(s) match {
      case ":reenter" :: Nil => true
      case ":r" :: Nil => true
      case _ => false
    }
  }

  object ReEnterN {
    def unapply(s: String): Option[Int] = splitToList(s) match {
      case ":reenter" :: n :: Nil => Some(n.toInt)
      case ":r" :: n :: Nil => Some(n.toInt)
      case _ => None
    }
  }

  object UnkownCommand {
    def unapply(s: String): Option[String] =
      if (s startsWith ":") Some(s) else None
  }

  def splitToList : String => List[String] = s => {
    s.trim.split(" ").toList
  }

}
