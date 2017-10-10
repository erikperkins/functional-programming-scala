package fpinscala.handlingerrors

import org.scalatest.FunSuite

class PersonTest extends FunSuite {

  test("setPerson") {
    val pearl = Person.create("Pearl", 0)

    assert(pearl.map(p => p.name.value == "Pearl") == Right(true))
    assert(pearl.map(p => p.age.value == 0) == Right(true))
  }

}
