package domain.input
import cats.data.{OptionT, State, StateT}
import cats.implicits._
import com.bot4s.telegram.models.Message
import domain.characterSheet.initiative.{Encounter, InitEntity}
import domain.lobbies.GameLobby
object AdditionCommandParser{
  type StringError[A]=Either[String,A]
  type EitherState[S,A]=StateT[StringError,S,A]
  val parseStr:EitherState[List[String],String]=StateT[StringError, List[String], String] {
    l=>
      l match {
        case ::(head, next) => Right(next,head)
        case Nil => Left("Parse Error")
      }
  }
  val parseInt=StateT[StringError,List[String],Int] {
    case ::(head, next) => head.toIntOption match {
      case Some(value) => Right(next, value)
      case None => Left("Parse Error")
    }
    case Nil => Left("Parse Error")
  }
  val parseEntityDeclaration=for{
    name<-parseStr
    hp<-parseInt
    ac<-parseInt
    init<-parseInt
  }yield InitEntity(name,hp,init,ac)
  val parseDamage=for{
    name<-parseStr
    value<-parseInt
  }yield(name,value)
  val parseIntList:EitherState[List[String],List[Int]]=StateT[StringError,List[String],List[Int]]{
  l=>
    l.map(a => a.toIntOption).traverse(identity).toRight("Error while parsing sequence") match {
      case Left(value) => Left(value)
      case Right(value) => Right(Nil,value)
    }
  }
  val parseNameDamage:EitherState[List[String],(String,Int)]=for{
    name<-parseStr
    dmg<-parseIntList
  }yield (name,dmg.sum)
}