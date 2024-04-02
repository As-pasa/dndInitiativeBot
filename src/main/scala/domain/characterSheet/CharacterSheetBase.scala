package domain.characterSheet

import cats.data.Reader

case class DndCharacteristics(
                               stat: Map[Score, Int]
                             ) {

  def get(a: Score): Int = {
    stat.getOrElse(a, 10)
  }
}


case class CharacterSheetBase(
                               name: String,
                               proficiency: Int,
                               maxHitPoints: Int,
                               speed: Int,
                               AC: Int,
                               Initiative: Int,
                               scores: DndCharacteristics,
                               savingThrowProficiencies: Map[Score, ProficiencyLevel],
                               skillProficiencies: Map[CharacterSkill, ProficiencyLevel],
                               abilities:Seq[CharacterAbility],
                               inventory:Seq[InventoryItem],
                               otherProficiencies: Seq[OtherProficiencies]
                             )

sealed trait Score {
  def str: String
}

case object STR extends Score {
  override def str = "strength"
}

case object DEX extends Score {
  override def str = "dexterity"
}

case object CON extends Score {
  override def str = "constitution"
}

case object INT extends Score {
  override def str = "intelligence"
}

case object WIS extends Score {
  override def str = "wisdom"
}

case object CHA extends Score {
  override def str = "charisma"
}

trait ProficiencyLevel

case object NoProf extends ProficiencyLevel

case object HalfProf extends ProficiencyLevel

case object NormalProf extends ProficiencyLevel

case object DoubleProf extends ProficiencyLevel

trait CharacterSkill

case object Acrobatics extends CharacterSkill

case object SleightOfHand extends CharacterSkill

case object Stealth extends CharacterSkill

case object Arcana extends CharacterSkill

case object History extends CharacterSkill

case object Investigation extends CharacterSkill

case object Nature extends CharacterSkill

case object Religion extends CharacterSkill

case object AnimalHandling extends CharacterSkill

case object Insight extends CharacterSkill

case object Medicine extends CharacterSkill

case object Perception extends CharacterSkill

case object Survival extends CharacterSkill

case object Deception extends CharacterSkill

case object Intimidation extends CharacterSkill

case object Performance extends CharacterSkill

case object Persuasion extends CharacterSkill

case class CharacterAbility(name:String, description:String)
case class InventoryItem(name:String,description:String)
case class OtherProficiencies(name:String,description:String)
object Util {
  def getScoreFromSkill(characterSkill: CharacterSkill): Score = {
    characterSkill match {
      case Acrobatics => DEX
      case AnimalHandling => WIS
      case Arcana => INT
      case Deception => CHA
      case History => INT
      case Insight => WIS
      case Intimidation => CHA
      case Investigation => INT
      case Medicine => WIS
      case Nature => INT
      case Perception => WIS
      case Performance => CHA
      case Persuasion => CHA
      case Religion => INT
      case SleightOfHand => DEX
      case Stealth => DEX
      case Survival => WIS
      case _ => CON
    }
  }

  def calcProfCoefficient(profLevel: ProficiencyLevel): Double = profLevel match {
    case DoubleProf => 2
    case HalfProf => 0.5
    case NoProf => 0
    case NormalProf => 1
    case _ => 0
  }

}

object ModifierReaders{
  def scoreValueReader(score: Score): Reader[CharacterSheetBase, Int] = {
    Reader[CharacterSheetBase, Int](ch => ch.scores.get(score))
  }

  def savingThrowProficiencyLevelReader(score: Score): Reader[CharacterSheetBase, ProficiencyLevel] = {
    Reader[CharacterSheetBase, ProficiencyLevel](ch => {
      ch.savingThrowProficiencies.getOrElse(score, NoProf)
    })
  }

  def scoreModifierReader(score: Score): Reader[CharacterSheetBase, Int] = for {
    v <- scoreValueReader(score)
  } yield (v - 10) / 2

  def savingThrowProficiencyCoefficientReader(score: Score): Reader[CharacterSheetBase, Double] =
    for {
      d <- savingThrowProficiencyLevelReader(score)
    } yield Util.calcProfCoefficient(d)

  def savingThrowModifierReader(score: Score): Reader[CharacterSheetBase, Int] =
    for {
      mastery <- proficiencyReader
      scoreValue <- scoreModifierReader(score)
      coefficient <- savingThrowProficiencyCoefficientReader(score)
    } yield scoreValue + (coefficient * mastery).toInt

  def skillProficiencyLevelReader(characterSkill: CharacterSkill): Reader[CharacterSheetBase, ProficiencyLevel] =
    Reader[CharacterSheetBase, ProficiencyLevel](ch => ch.skillProficiencies.getOrElse(characterSkill, NoProf))


  val proficiencyReader: Reader[CharacterSheetBase, Int] =
    Reader[CharacterSheetBase, Int](_.proficiency)
  val maxHealthReader: Reader[CharacterSheetBase, Int] =
    Reader[CharacterSheetBase, Int](_.maxHitPoints)
  val speedReader: Reader[CharacterSheetBase, Int] =
    Reader[CharacterSheetBase, Int](_.speed)
  val initiativeModReader: Reader[CharacterSheetBase, Int] =
    scoreModifierReader(DEX)
  val acReader: Reader[CharacterSheetBase, Int] =
    Reader[CharacterSheetBase, Int](_.AC)
}

object Readers {

  val characterAbilitiesReader: Reader[CharacterSheetBase, Seq[CharacterAbility]] =
    Reader[CharacterSheetBase,Seq[CharacterAbility]](ch=>ch.abilities)

}

