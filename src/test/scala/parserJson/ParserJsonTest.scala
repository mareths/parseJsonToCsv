package parserJson


import org.scalatest.FlatSpec
import parserJson.ParserJson.CsvLine
import play.api.libs.json.{JsNull, JsString, JsValue, Json}

class ParserJsonTest extends FlatSpec {
  "Quand on passe au ParserJson un json vide" should
  "affiche un resultat vide" in {
    val emptySample: JsValue = Json.parse("""{}""")

    assert(ParserJson.print(emptySample) == "{}")
  }

  "Quand on passe au ParserJson un json comprenant un element" should
    "affiche l'element contenu" in {
    val sample: JsValue = Json.parse("""{ "test": "valeur" }""")

    assert(ParserJson.print(sample) == """{"test":"valeur"}""")
  }

  "Quand on passe au ParserJson un json comprenant plusieurs elements" should
    "affiche l'element dont l'attribut est passé en paramétre" in {
    val sample: JsValue = Json.parse("""{ "test": "valeur", "label": "coucou" }""")

    assert(ParserJson.find(sample, "label") == """{"label":"coucou"}""")
  }

  "Quand on passe au ParserJson un json comprenant un tableau avec au moins 2 éléments" should
    "affiche le nom et l'age des elements du tableau trouvé" in {
    val sample: JsValue = Json.parse("""
      {
        "name" : "Watership Down",
        "location" : {
          "lat" : 51.235685,
          "long" : -1.309197
        },
        "residents" : [ {
          "name" : "Fiver",
          "age" : 4,
          "role" : null
        }, {
          "name" : "Bigwig",
          "age" : 6,
          "role" : "Owsla"
        } ]
      }
      """)

    assert(ParserJson.findResidentsNameAndAge(sample) == List(CsvLine("Fiver",4), CsvLine("Bigwig",6)))
  }

}
