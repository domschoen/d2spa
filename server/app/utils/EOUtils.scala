package utils

import d2spa.shared._
import play.api.Logger
import play.api.libs.ws._
import play.api.libs.json._

object EOUtils {

  def convertEOJsonToEOs(eomodel: EOModel, eo: EO, resultBody: JsObject): List[EO] = {
    val jObj = resultBody.asInstanceOf[JsObject]
    val refreshedEOOpt = eoFromJsonJsObject(eomodel, Some(eo), jObj)
    //println("convertEOJsonToEOs | refreshedEOOpt " + refreshedEOOpt)
    refreshedEOOpt match {
      case Some(refreshedEO) =>
        //println("convertEOJsonToEOs | refreshedEO " + refreshedEO)
        //val eoExtracted = eoExtractor(refreshedEO, Set(refreshedEO))
        //println("convertEOJsonToEOs | eoExtracted " + eoExtracted)
        List(refreshedEO)
      case None => List()
    }
  }

  def eoFromJsonJsObject(eomodel: EOModel, eoOpt: Option[EO], jsObj: JsObject): Option[EO] = {
    //println("eoFromJsonJsObject | eoOpt " + eoOpt)
    //println("eoFromJsonJsObject | jsObj " + jsObj)
    val entityNameOpt: Option[(String, JsValue)] = jsObj.fields.find(x => x._1.equals("type"))
    //println("eoFromJsonJsObject | entityNameOpt " + entityNameOpt)
    entityNameOpt match {
      case Some((_, JsString(entityName))) =>
        val entityOpt = EOModelUtils.entityNamed(eomodel, entityName)
        //println("eoFromJsonJsObject | entityOpt " + entityOpt)
        entityOpt match {
          case Some(entity) =>
            val pkNames = entity.pkAttributeNames
            //println("eoFromJsonJsObject | pkNames " + pkNames)

            // Iterate on fields (Seq[(String, JsValue)]
            val eo = eoOpt match {
              case Some(x) => x
              case None =>
                val fieldsMap = jsObj.fields.toList.toMap

                val pk = fieldsMap("id") match {
                  case JsNumber(bigDecimal) => List(bigDecimal.toInt)
                  case JsArray(array) => {
                    array.map(pkv => pkv match {
                      case JsNumber(bigDecimal) => Some(bigDecimal.toInt)
                      case _ => None
                    }).toList.flatten
                  }
                }
                EOValue.dryEOWithEntity(entity.name, EOPk(pk))
            }
            val jObjValues: Seq[(String, JsValue)] = jsObj.fields.filter(x => {
              //println("x " + x)
              !(pkNames.contains(x._1) || x._1.equals("type") || x._1.equals("id"))
            })
            //println("jObjValues " + jObjValues)
            val valuesMap = valueMapWithJsonFields(eomodel, jObjValues)
            //println("valuesMap " + valuesMap)
            Some(EOValue.takeValuesForKeys(eo, valuesMap))


          case None =>
            println("no entity found for entityName: " + entityName)
            None
        }

      case None =>
        println("no type in json: " + jsObj)
        None
    }
  }

  def valueMapWithJsonFields(eomodel: EOModel, fields: Seq[(String, JsValue)]): Map[String, EOValue] = {
    //println("valueMapWithJsonFields | eomodel " + eomodel)
    //println("valueMapWithJsonFields | fields " + fields)

    fields.map(kvTuple => {
      val key = kvTuple._1
      val value = kvTuple._2
      //Logger.debug("valueMapWithJsonFields : " + value)
      //println("valueMapWithJsonFields : " + value)

      //Logger.debug("Complete EO: value : " + value)
      // JsValue : JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsUndefined
      val newValue = value match {
        case jsArray: play.api.libs.json.JsArray =>
          //println("valueMapWithJsonFields | value | match with JsArray " + jsArray)
          //println("jsArray " + jsArray.getClass.getName)


          val seqJsValues = jsArray.value
          //Logger.debug("seqJsValues : " + seqJsValues)

          if (seqJsValues.size == 0) {
            EmptyValue
          } else {
            seqJsValues.head match {
              case oneElement: JsObject =>
                // {"id":1,"type":"Project"}

                val eoOpts: List[Option[EO]] = seqJsValues.toList.map(x => {
                  eoFromJsonJsObject(eomodel, None, x.asInstanceOf[JsObject])
                })
                /*val hasError = !eos.find(eoOpt => eoOpt.isEmpty).isEmpty
                if (hasError) {
                  handleException(response.body, eo)

                }*/
                //println("related eos : " + eoOpts)


                val eos = eoOpts.flatten
                val eoPks = eos.map(eo => eo.pk)
                ObjectsValue(eoPks)

              case oneElement: JsNumber =>
                Logger.error("Key: " + key + " has a value of JsArray of JsNumber: " + seqJsValues)

                // {"id":1,"type":"Project"}
                /*val ints: List[Option[Int]] = seqJsValues.toList.map(n => {
                  n match {
                    case JsNumber(num) =>
                      num match {
                        case bd: BigDecimal => Some(bd.toInt)
                        case _ => {
                          Logger.error("Found Unsupported JsNumber type " + n)
                          None
                        }
                      }
                    case _ =>
                      Logger.error("Non herogeneous array. Expected JsNumber. Found: " + n)
                      None
                  }

                })
                /*val hasError = !eos.find(eoOpt => eoOpt.isEmpty).isEmpty
                if (hasError) {
                  handleException(response.body, eo)

                }*/
                val pks: List[Int] = ints.flatten
                val eo = EOValue.dryEOWithEntity(entity, pks)
                ObjectValue*/
                EmptyValue
              case _ =>
                Logger.error("Key: " + key + " has a value of JsArray of unsupported element: " + seqJsValues.head)
                EmptyValue
            }

          }


        case jsObj: JsObject =>
          //println("valueMapWithJsonFields | value | match with JsObject " + jsObj)
          val eoOpt = eoFromJsonJsObject(eomodel, None, jsObj)
          eoOpt match {
            case Some(eo) =>
              ObjectValue(eo = eo.pk)
            case None =>
              println("Failed to create EO with json " + jsObj)
              EmptyValue

          }

        case jsString: JsString =>
          //println("valueMapWithJsonFields | value | match with JsString " + jsString)
          val stringV = jsString.value
          EOValue.stringV(stringV)

        case play.api.libs.json.JsNumber(n) =>
          //println("valueMapWithJsonFields | value | match with JsNumber " + n)
          // TBD use a BigDecimal container
          n match {
            case bd: BigDecimal => IntValue(bd.toInt)
            case _ => {
              Logger.error("Found Unsupported JsNumber type " + n)
              EmptyValue
            }
          }

        case n: play.api.libs.json.JsBoolean =>
          //println("valueMapWithJsonFields | value | match with JsBoolean " + n)
          val boolVal = n.value
          BooleanValue(boolVal)

        case play.api.libs.json.JsNull =>
          //println("valueMapWithJsonFields | value | match with JsNull")
          EmptyValue

        case _ =>
          //println("valueMapWithJsonFields | value | match with unsupported " + _)
          EmptyValue
        //val stringV = value.toString()
        //EOValue.stringV(stringV)

      }

      //Logger.debug("JsObj value " + value.getClass.getName + " value: " + value)
      (key, newValue)
    }).toMap

  }


}