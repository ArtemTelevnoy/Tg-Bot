
object Connection {
  import ConfValues._
  import slick.jdbc.PostgresProfile.api._

  import scala.concurrent.ExecutionContext

  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global
  val db = Database.forURL(s"jdbc:postgresql://$host:$port/$baseName",
    s"$user", s"$password", driver = "org.postgresql.Driver")
}