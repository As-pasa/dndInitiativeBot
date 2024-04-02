import cats.implicits.toFunctorOps
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.{Args, Commands}
import com.bot4s.telegram.clients.FutureSttpClient
import com.bot4s.telegram.future.{Polling, TelegramBot}
import com.bot4s.telegram.methods.SendMessage
import domain.input.AdditionCommandParser.{parseDamage, parseEntityDeclaration, parseInt, parseIntList, parseNameDamage, parseStr}
import domain.lobbies.{GameRepository, Player}
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration


class InitiativeBot(val token: String, repo: GameRepository) extends TelegramBot
  with Polling
  with Commands[Future] {

  LoggerConfig.factory = PrintLoggerFactory()
  // set log level, e.g. to TRACE
  LoggerConfig.level = LogLevel.WARN

  // Use sttp-based backend
  implicit val backend = CustomSttpBackend()
  override val client: RequestHandler[Future] = new FutureSttpClient(token)
  onCommand("greet") { implicit msg =>
    println(msg.source)
    reply(msg.source.toString).void

  }

  onCommand("/create") { implicit msg =>
    (for {
      user <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
      status <- repo.createLobby(Player(user, msg.source, Nil))
    } yield status) match {
      case Left(value) => reply(value).void
      case Right(value) => reply(s"lobby created, your id: ${value.toString}").void
    }
  }
  onCommand("/join") { implicit msg =>
    withArgs {

      args =>
        (for {
          name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
          id <- parseInt.run(args.toList)
          lobby <- repo.getLobby(id._2)
          status <- repo.addPlayer(Player(name, msg.source, Nil), lobby)
        } yield (status, name)) match {
          case Left(value) => reply(value).void
          case Right(_) =>
            reply("Successfully joined to lobby").void

        }

    }
  }
  onCommand("/quit") {
    implicit msg => {
      (for {
        name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))

        status <- repo.rmPlayer(name)
      } yield status) match {
        case Left(value) => reply(value).void
        case Right(_) => reply("Successfully leaving lobby").void
      }
    }
  }
  onCommand("/removeLobby") {
    implicit msg => {
      (for {
        name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
        lobby <- repo.getLobbyFromPlayer(name)
        status <- repo.rmLobby(lobby.id)
      } yield status) match {
        case Left(value) => reply(value).void
        case Right(_) => reply("Lobby closed").void
      }
    }
  }
  onCommand("/add") {
    implicit msg =>
      withArgs {
        args =>
          (for {
            entity <- parseEntityDeclaration.run(args.toList)
            name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
            lobby <- repo.getLobbyFromPlayer(name)
            status <- lobby.addEntity(name, entity._2)
          } yield status) match {
            case Left(value) => reply(value).void
            case Right(_) => reply("Entity added").void
          }
      }
  }
  onCommand("/rm") {
    implicit msg =>
      withArgs {
        args =>
          (for {
            entityName <- parseStr.run(args.toList)
            name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
            lobby <- repo.getLobbyFromPlayer(name)
            status <- lobby.damageEntity(entityName._2, 25000)
          } yield status) match {
            case Left(value) => reply(value).void
            case Right(_) => reply("Entity removed").void
          }
      }
  }
  onCommand("/dmg") {
    implicit msg =>
      withArgs {
        args =>
          (for {
            entityDamage <- parseNameDamage.run(args.toList)

            name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
            lobby <- repo.getLobbyFromPlayer(name)
            status <- lobby.damageEntity(entityDamage._2._1, entityDamage._2._2)
          } yield status) match {
            case Left(value) => reply(value).void
            case Right(_) => reply("Entity Damaged").void
          }
      }
  }
  onCommand("/heal") {
    implicit msg =>
      withArgs {
        args =>
          (for {
            entityDamage <- parseDamage.run(args.toList)
            name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
            lobby <- repo.getLobbyFromPlayer(name)
            status <- lobby.healEntity(entityDamage._2._1, entityDamage._2._2)
          } yield status) match {
            case Left(value) => reply(value).void
            case Right(_) => reply("Entity Healed").void
          }
      }
  }
  onCommand("/cur") {
    implicit msg => {
      (for {
        name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
        lobby <- repo.getLobbyFromPlayer(name)
        status <- lobby.getCurrent
      } yield status) match {
        case Left(value) => reply(value).void
        case Right(value) => reply(value.name).void
      }
    }
  }

  private def sendTo(id: Seq[Long], text: String): Future[Unit] = Future.sequence(
    id.map(a => request(SendMessage(
      a, text, None, None, None, None, None, None, None, None)))
  ).void

  onCommand("/nxt") {
    implicit msg => {
      (for {
        name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
        lobby <- repo.getLobbyFromPlayer(name)
        _ <- lobby.nextEntity()
        status <- lobby.getCurrent
      } yield (status, lobby)) match {
        case Left(value) => reply(value).void
        case Right(value) => sendTo(value._2.getListOfEntityHolders(value._1)
          .map(_.chatId), s"your turn! character name: ${value._1.name}")
      }
    }
  }
  onCommand("/prv") {
    implicit msg => {
      (for {
        name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
        lobby <- repo.getLobbyFromPlayer(name)
        _ <- lobby.prevEntity()
        status <- lobby.getCurrent
      } yield (status, lobby)) match {
        case Left(value) => reply(value).void
        case Right(value) => sendTo(value._2.getListOfEntityHolders(value._1)
          .map(_.chatId), s"your turn! character name: ${value._1.name}")
      }
    }
  }
  onCommand("/lobbyInfo") {
    implicit msg => {
      (for {
        name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
        lobby <- repo.getLobbyFromPlayer(name)

      } yield (lobby.id, lobby.linearize(), lobby.getPlayers)) match {
        case Left(value) => reply(value).void
        case Right(value) => reply(s"id ${value._1} players:${value._3.mkString("\n")}").void
      }
    }
  }
  onCommand("/all") {
    implicit msg => {
      (for {
        name <- msg.from.toRight("Username unspecified").flatMap(_.username.toRight("No Nickname"))
        lobby <- repo.getLobbyFromPlayer(name)
        info <- Right(lobby.linearize())
      } yield {
        info
      }) match {
        case Left(value) => reply(value).void
        case Right(value) => if (value.isEmpty) reply("Entities not found").void else reply(value.map(a => a.toString).mkString("\n")).void
      }
    }
  }
}


object main {
  def main(args: Array[String]): Unit = {
    val lobbyRepo = GameRepository()
    val bot = new InitiativeBot(sys.env("dndBotToken"), lobbyRepo)
    val eol = bot.run()
    scala.Predef.println("Press [ENTER] to shutdown the bot, it may take a few seconds...")
    scala.io.StdIn.readLine()
    bot.shutdown() // initiate shutdown
    // Wait for the bot end-of-life
    Await.result(eol, Duration.Inf)
  }
}