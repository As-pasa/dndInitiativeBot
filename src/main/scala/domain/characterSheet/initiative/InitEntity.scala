package domain.characterSheet.initiative

class InitEntity(val name: String, var hp: Int, val initiative: Int, val ac: Int){
  override def toString: String =
    s"${name} AC: ${ac} HP: ${hp}"
}


object InitEntity {
  def apply(name: String, Hp: Int, Initiative: Int, AC: Int): InitEntity = {
    new InitEntity(name, Hp, Initiative, AC)
  }

  implicit object initOrdering extends Ordering[InitEntity] {
    override def compare(x: InitEntity, y: InitEntity): Int = {
      val i = (x.initiative compare y.initiative)*(-1)
      if (i != 0) i else 1
    }
  }
}





