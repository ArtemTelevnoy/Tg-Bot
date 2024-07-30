import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.client.middleware.Logger
import telegramium.bots.high.{Api, BotApi}

object Application extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO].resource
      .use { httpClient =>
        val http = Logger(logBody = false, logHeaders = false)(httpClient)
        implicit val api: Api[IO] = BotApi(http, baseUrl = s"https://api.telegram.org/bot${ConfValues.token}")
        val echoBot = new EchoBot()

        echoBot.start().as(ExitCode.Success)
      }.guarantee(IO {
        Connection.db.close()
    })
}
/*
CREATE TABLE public."UserStats"
(
    id bigint NOT NULL,
    wins integer,
    loses integer,
    draws integer,
    PRIMARY KEY (id)
);

ALTER TABLE IF EXISTS public."UserStats"
    OWNER to postgres;
 */