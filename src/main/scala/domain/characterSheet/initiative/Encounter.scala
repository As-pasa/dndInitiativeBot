package domain.characterSheet.initiative

class Encounter(entities: Seq[InitEntity]) {
  private val map: Map[String, InitEntity] = entities.foldLeft(Map[String, InitEntity]())(
    (acc, v: InitEntity) => {
      acc + (v.name -> v)
    }
  )

  def linearize(): Seq[InitEntity] = map.values.filter(a => a.hp > 0).toSeq.sorted

  def add(other: Seq[InitEntity]): Encounter = {
    new Encounter(other ++ map.values)
  }

  def fromName(n: String): Either[String,InitEntity] = map.get(n) match {
    case Some(value) => Right(value)
    case None => Left("No such Entity")
  }

}
