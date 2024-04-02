package domain.characterSheet.initiative

class InitiativeHolder(var encounter: Encounter, var current: String = "") {
  def next(): Unit = {
    val alive = encounter.linearize().filter(a => a.hp > 0 || a.name == current).map(_.name)
    val index = alive.indexOf(current)
    if (index >= 0) {
      current = alive((index + 1) % alive.length)
    }
    else {
      current = alive.head
    }
  }

  def prev(): Unit = {
    val alive = encounter.linearize().filter(a => a.hp > 0 || a.name == current).map(_.name)
    val index = alive.indexOf(current)
    if (index >= 0) {
      current = alive((index - 1) % alive.length)
    } else {

      current = alive.last
    }
  }

  def getCurrent: InitEntity = {
    encounter.fromName(current).getOrElse(encounter.linearize().head)
  }
}
