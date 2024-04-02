package domain.lobbies

import cats.data.Reader
import domain.characterSheet.initiative.{Encounter, InitEntity, InitiativeHolder}
import shapeless.PolyDefns.->
import cats.data.ReaderT
import domain.lobbies.Types.{EitherString, LobbyIdEitherReader}


object Types {
  type EitherString[T] = Either[String, T]
  type LobbyIdEitherReader[T] = ReaderT[EitherString, Int, T]

}
case class Player(name:String,chatId:Long,entities:List[InitEntity]){
  def entityAdded(entity:InitEntity):Player={
    Player(name,chatId,entity::entities)
  }
}

class GameLobby(val id:Int,private var players: Map[String, Player], private var encounter: Encounter, private val initiative: InitiativeHolder) {

  import Types._
  def getPlayers:Seq[String]=players.keySet.toSeq
  private def getPlayer(name: String): EitherString[(String, Player)] = {
    players.get(name) match {
      case Some(value) => Right(name -> value)
      case None => Left("No such player")
    }
  }
  def linearize(): Seq[InitEntity] = encounter.linearize()

  def addEntity(playerHolder: String, n: InitEntity): EitherString[String] = for{
    player<-getPlayer(playerHolder)
  }yield{
    players=players.updated(player._1,player._2.entityAdded(n))
    encounter=encounter.add(Seq(n))
    initiative.encounter=encounter
    "Success"
  }

  def getEntityFromName(name: String): EitherString[(InitEntity, Seq[String])] = for {
    entity <- encounter.fromName(name)
  } yield {
    val hosts = players.filter(a=>a._2.entities.contains(entity)).keySet.toSeq
    (entity, hosts)
  }
  def isEmptyLobby():Boolean= players.isEmpty
  def getCurrent: EitherString[InitEntity] = {
    Right(initiative.getCurrent)
  }

  def nextEntity(): EitherString[Unit] = Right(initiative.next())

  def prevEntity(): EitherString[Unit] = Right(initiative.prev())

  def addPlayer(player: Player): EitherString[String] = players.get(player.name) match {
    case Some(value) => Left("Player already in lobby")
    case None => {
      players = players + (player.name -> player)
      Right("Success")
    }
  }
  def getListOfEntityHolders(entity:InitEntity):Seq[Player]={
    (for { player<-players; if player._2.entities.contains(entity)}yield player._2).toSeq
  }

  def rmPlayer(name: String): EitherString[String] = players.get(name) match {
    case Some(value) => {
      players = players.removed(name); Right("Success")
    }
    case None => Left("No Such Player")
  }

  def damageEntity(n: String, value: Int): EitherString[String] = for {
    entity <- getEntityFromName(n)
  } yield {
    entity._1.hp -= value
    "Success"
  }

  def healEntity(n: String, value: Int): EitherString[String] = for {
    entity <- getEntityFromName(n)
  } yield {
    entity._1.hp += value
    "Success"
  }
}
object GameLobby{
  def apply(creator:Player,id:Int):GameLobby={
      val nc=new Encounter(Seq())
      val in=new InitiativeHolder(nc)
      new GameLobby(id,Map(creator.name->creator),nc,in)
  }
}
class GameRepository(var players:Map[String,GameLobby], var lobbies: Map[Int, GameLobby], var freeLobbyId: Int) {
  def getLobby(id:Int):EitherString[GameLobby]=lobbies.get(id).toRight("No Such Lobby")
  def getLobbyFromPlayer(player:String):EitherString[GameLobby]= players.get(player) match {
    case Some(value) => Right(value)
    case None =>Left("No repository Found")
  }
  def checkPlayerFree(player:String):EitherString[String]= players.get(player) match {
    case Some(_) => Left("Already in lobby")
    case None => Right(player)
  }
  def addPlayer(player:Player,lobby:GameLobby):EitherString[String]=for{
    name<-checkPlayerFree(player.name)
    status<-lobby.addPlayer(player)
  }yield {
    players=players+(name->lobby)
    status
  }
  def rmPlayer(player:String):EitherString[String]= for{
    repo<-getLobbyFromPlayer(player)
    status<-repo.rmPlayer(player)
  }yield{
    players=players.removed(player)
    if(repo.isEmptyLobby()){
      lobbies=lobbies.removed(repo.id)
    }
    status
  }
  def createLobby(creator:Player):EitherString[Int]=for{
    name<-checkPlayerFree(creator.name)
  } yield{
    freeLobbyId=freeLobbyId+1
    val lobby=GameLobby(creator,freeLobbyId-1)
    lobbies=lobbies+((freeLobbyId-1)->lobby)
    players=players+(name->lobby)
    freeLobbyId-1
  }
  def rmLobby(id:Int):EitherString[String]=for{
    lobby<-getLobby(id)
  }yield{
    lobby.getPlayers.map(a=>rmPlayer(a).getOrElse(""))
    "Success"
  }




}
object GameRepository{
  def apply():GameRepository=new GameRepository(Map(),Map(),0)

}