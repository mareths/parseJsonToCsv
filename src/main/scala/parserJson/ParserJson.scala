package parserJson

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Location(lat: Double, long: Double)
case class Resident(name: String, age: Int, role: Option[String])
case class Place(name: String, location: Location, residents: List[Resident])

object ParserJson {
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
      (JsPath \ "residents").read[List[Resident]]
    )(Place.apply _)

  case class CsvLine( name: String, age:Int )

  def findResidentsNameAndAge(sample: JsValue): List[CsvLine] =
  {
    sample.validate[Place] match {
      case s: JsSuccess[Place] => {
        def generateListCsvLine(residentList: List[Resident]): List[CsvLine] =
          if (residentList.isEmpty) {
            List()
          }
          else {
            List(CsvLine(residentList.head.name, residentList.head.age)) :::
              (generateListCsvLine(residentList.drop(1)))
          }
        generateListCsvLine(s.get.residents)
      }
      case e: JsError => {
        List()
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
