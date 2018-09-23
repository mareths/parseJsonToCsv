package parserJson

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Location(lat: Double, long: Double)
case class Resident(name: String, age: Int, role: Option[String])
case class Place(name: String, location: Location, residents: Seq[Resident])

object ParserJson {
  def findSeq(sample: JsValue, str: String): String = {
    implicit val locationReads: Reads[Location] = (
      (JsPath \ "lat").read[Double] and
        (JsPath \ "long").read[Double]
      )(Location.apply _)

    implicit val residentReads: Reads[Resident] = (
      (JsPath \ "name").read[String] and
        (JsPath \ "age").read[Int] and
        (JsPath \ "role").readNullable[String]
      )(Resident.apply _)

    implicit val placeReads: Reads[Place] = (
      (JsPath \ "name").read[String] and
        (JsPath \ "location").read[Location] and
        (JsPath \ "residents").read[Seq[Resident]]
      )(Place.apply _)

    sample.validate[Place] match {
      case s: JsSuccess[Place] => {
        return s.get.toString
      }
      case e: JsError => {
        return e.toString
      }
    }
  }

  def find(sample: JsValue, str: String): String = {
    val nameReads: Reads[String] = (JsPath \ str).read[String]

    val nameResult: JsResult[String] = sample.validate[String](nameReads)

    nameResult match {
      case s: JsSuccess[String] => return ("{\"" + str + "\":\"" + s.get + "\"}")
      case e: JsError => return ("Errors: " + JsError.toJson(e).toString())
    }

  }

  def print(emptySample: JsValue): String  = {
    return Json.stringify(emptySample)
  }


}
