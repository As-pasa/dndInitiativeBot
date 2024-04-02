import domain.characterSheet.{Acrobatics, CharacterSheetBase, DEX, DndCharacteristics, DoubleProf, ModifierReaders, NoProf, NormalProf, Readers, STR, WIS}
import org.scalatest.flatspec.AnyFlatSpec

class DomainAccessorSpec extends AnyFlatSpec {
  val ch: CharacterSheetBase = CharacterSheetBase("Zondo", 3, 10, 10, 10, 10, DndCharacteristics(Map(STR -> 18)), Map(DEX->NormalProf,WIS->NormalProf), Map(Acrobatics->DoubleProf),Seq(),Seq(),Seq())

  "STR reader" should "return character strength" in{
    val strReader=ModifierReaders.scoreValueReader(STR)
    assert(strReader.run(ch) == 18)
  }
  it should "get 10 if characteristic is not specified in definition" in {
    val dexReader=ModifierReaders.scoreValueReader(DEX)
    assert(dexReader.run(ch)==10)
  }
  it should "get 3 if character has 10 dex and proficiency in it" in {
    assert(ModifierReaders.savingThrowModifierReader(DEX).run(ch)==3)
  }
}
