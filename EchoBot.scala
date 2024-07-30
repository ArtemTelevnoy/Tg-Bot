import cats.Parallel
import cats.effect.Async
import com.lightbend.emoji._
import slick.jdbc.H2Profile.api._
import telegramium.bots.high.{Api, LongPollBot}
import telegramium.bots.high.implicits._

case class Movie(id: Long, wins: Int, loses: Int, draws: Int)

class Movies(tag: Tag) extends Table[Movie](tag, "UserStats") {
  def id = column[Long]("id", O.PrimaryKey)
  def wins = column[Int]("wins")
  def loses = column[Int]("loses")
  def draws = column[Int]("draws")
  def * = (id, wins, loses, draws) <> (Movie.tupled, Movie.unapply)
}

class EchoBot[F[_]]()(implicit
  bot: Api[F],
  asyncF: Async[F],
  parallel: Parallel[F]
) extends LongPollBot[F](bot) {
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import telegramium.bots._

  override def onMessage(msg: Message): F[Unit] = msg.text match {
    case Some("/start") => sendMessage(ChatIntId(msg.chat.id),
      text = "Hello, nice to meet you. Send /game dor playing").exec.void

    case Some(/*/"game"*/"g") => sendMessage(ChatIntId(msg.chat.id), text = "Menu",
        replyMarkup = Some(
          InlineKeyboardMarkup(
            inlineKeyboard = List(
              List(InlineKeyboardButton("State", callbackData = Some("state")),
                   InlineKeyboardButton("R-P-S Game", callbackData = Some("rps"))),
              List(
                InlineKeyboardButton(EmojiDice.toString(), callbackData = Some("dice")),
                InlineKeyboardButton(EmojiDarts.toString(), callbackData = Some("darts"))),
              List(
                InlineKeyboardButton("Gimme a quiz", callbackData = Some("quiz")))
            )))).exec.void

    case _ => sendMessage(ChatIntId(msg.chat.id),
      text = "Sorry, I can't understand you").exec.void
  }

  override def onCallbackQuery(query: CallbackQuery): F[Unit] = {
    def rollTheDice(chatId: Long, emoji: Emoji = EmojiDice): F[Unit] = {
      sendDice(ChatIntId(chatId), emoji = Some(emoji.toString)).exec.void >>
        answerCallbackQuery(query.id).exec.void
    }

    def getState(chatId: Long): String = {
      import Connection._

      import scala.concurrent.Await
      import scala.concurrent.duration.Duration

      val mov = TableQuery[Movies]
      val query = mov.filter(_.id === chatId)
      val resultFuture = db.run(query.result.headOption)

      Await.result(resultFuture, Duration.Inf) match {
        case Some(Movie(_, wins, loses, draws)) =>
          s"Stat for id=$chatId: wins-$wins, loses-$loses, draws-$draws, total match-${wins + loses + draws}"
        case None =>
          db.run(TableQuery[Movies] += Movie(chatId, 0, 0, 0))
          "You are a new user, your score is 0"
      }
    }

    def updateStat(chatId: Long, res: Int): Unit = {
      import Connection._

      import scala.concurrent.Await
      import scala.concurrent.duration.Duration

      val mov = TableQuery[Movies]
      val query = mov.filter(_.id === chatId)
      val resultFuture = db.run(query.result.headOption)

      Await.result(resultFuture, Duration.Inf) match {
        case Some(Movie(_, wins, loses, draws)) =>
          val updatedMovie = Movie(chatId, wins + (if (res == 1) 1 else 0),
            loses + (if (res == 2) 1 else 0), draws + (if (res == 0) 1 else 0))
          val updateAction = mov.filter(_.id === chatId).update(updatedMovie)
          db.run(updateAction)
        case None =>
          db.run(TableQuery[Movies] += Movie(chatId, 0, 0, 0))
      }

    }

    def quiz(chatId: Long): F[Unit] = {
      sendPoll(ChatIntId(chatId),
        question = "What is Tesseract(Figure)?",
        `type` = Some("quiz"),
        options = List("Volumetric oval", "Four dimensional cube", "Triangle with curved sides", "I don't know!"),
        correctOptionId = Some(1),
        isAnonymous = Some(false),
        explanation = Some("https://en.wikipedia.org/wiki/Tesseract")
      ).exec.void >>
        answerCallbackQuery(callbackQueryId = query.id).exec.void
    }

    def sendMsg(chatId: Long, text: String, parseMode: ParseMode): F[Unit] = {
      sendMessage(ChatIntId(chatId), text,
        parseMode = Some(parseMode)
      ).exec.void >> answerCallbackQuery(callbackQueryId = query.id).exec.void
    }

    def rpcProcess(m: Message): F[Unit] = {
      import scala.util.Random
      val botAns = Random.nextInt(3)
      def winnerCMN(ans: Int): String = (botAns, ans) match {
          case (x, y) if x == y => "draw"
          case (x, y) if x - y == 1 || x == 0 && y == 2 => "win"
          case _ => "lose"
        }

      sendMessage(
        chatId = ChatIntId(m.chat.id),
        text = "Choose: ",
        replyMarkup = Some(
          InlineKeyboardMarkup(
            inlineKeyboard = List(
              List(
                InlineKeyboardButton(Emoji(0x270A).toString(), callbackData = Some(winnerCMN(0))),
                InlineKeyboardButton(Emoji(0x270B).toString(), callbackData = Some(winnerCMN(1))),
                InlineKeyboardButton(Emoji(0x270C).toString(), callbackData = Some(winnerCMN(2)))
              ))))).exec.void >> answerCallbackQuery(callbackQueryId = query.id).exec.void

    }

    query.data
      .map {
        case "state"      => query.message.fold(asyncF.unit)(m => sendMsg(m.chat.id, getState(m.chat.id), Html))
        case "dice"       => query.message.fold(asyncF.unit)(m => rollTheDice(m.chat.id))
        case "darts"      => query.message.fold(asyncF.unit)(m => rollTheDice(m.chat.id, EmojiDarts))
        case "quiz"       => query.message.fold(asyncF.unit)(m => quiz(m.chat.id))
        case "win"        => query.message.fold(asyncF.unit)(m => {
            updateStat(m.chat.id, 1)
            sendMessage(ChatIntId(m.chat.id),
              "Congratulations! You win").exec.void >> answerCallbackQuery(query.id).exec.void
          })
        case "lose"       => query.message.fold(asyncF.unit)(m => {
          updateStat(m.chat.id, 2)
          sendMessage(ChatIntId(m.chat.id),
            "Sorry, you lose.. ):").exec.void >> answerCallbackQuery(query.id).exec.void
        })
        case "draw"       => query.message.fold(asyncF.unit)(m => {
          updateStat(m.chat.id, 0)
          sendMessage(ChatIntId(m.chat.id),
            "Oh, draw").exec.void >> answerCallbackQuery(query.id).exec.void
        })
        case "rps"        => query.message.fold(asyncF.unit)(m => rpcProcess(m))
      }
      .getOrElse(asyncF.unit)
  }

  override def onInlineQuery(query: InlineQuery): F[Unit] = {
    answerInlineQuery(query.id,
      results = query.query
        .split(" ")
        .zipWithIndex
        .map { case (word, idx) =>
          InlineQueryResultArticle(
            id = idx.toString,
            word,
            InputTextMessageContent(word)
          )
        }
        .toList
    ).exec.void
  }

  override def onChosenInlineResult(inlineResult: ChosenInlineResult): F[Unit] = {
    import io.circe.syntax._
    import telegramium.bots.CirceImplicits._
    asyncF.delay {
      println(inlineResult.asJson.spaces4)
    }
  }
}
