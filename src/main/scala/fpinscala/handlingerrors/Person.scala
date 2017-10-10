package fpinscala.handlingerrors

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {

  def create(name: String, age: Int): Either[String, Person] =
    setName(name).map2(setAge(age))(Person(_,_))

  private def setName(name: String): Either[String, Name] = {
    if (name == null || name == "") Left("Name cannot be blank.")
    else Right(new Name(name))
  }

  private def setAge(age: Int): Either[String, Age] = {
    if (age < 0) Left("Age cannot be negative.")
    else Right(new Age(age))
  }

}
