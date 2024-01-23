package handlers

import com.fasterxml.jackson.databind.ObjectMapper
import org.apache.commons.lang3.StringUtils
import org.apache.poi.ss.usermodel.{CellType, Row}
import org.apache.poi.xssf.usermodel.{XSSFRow, XSSFWorkbook}
import org.slf4j.{Logger, LoggerFactory}
import org.sunbird.cache.impl.RedisCache
import utils.Constants

import java.io._
import java.net.{HttpURLConnection, URL}
import java.util
import java.util.Collections
import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.JavaConverters._
import scala.util.control.Breaks._

object QuestionExcelParser {

  private val logger: Logger = LoggerFactory.getLogger(RedisCache.getClass.getCanonicalName)

  def getQuestions(fileName: String, file: File): Option[IndexedSeq[Map[String, AnyRef]]] = {
    try {
      val workbook = new XSSFWorkbook(new FileInputStream(file))
      val sheets = (0 until workbook.getNumberOfSheets).map(index => workbook.getSheetAt(index))

      val filteredQuestions = sheets.flatMap(sheet => {
        logger.info("Inside the getQuestions")

        (1 until sheet.getPhysicalNumberOfRows)
          .filter(rowNum => {
            val oRow: Option[Row] = Option(sheet.getRow(rowNum))
            oRow.exists(row => {
              val questionType = row.getCell(11)
              val isMCQ = questionType.toString.trim.equalsIgnoreCase(Constants.MCQ_SINGLE_SELECT)
              val answerCell = row.getCell(10)
              val isAnswerNotBlank = answerCell.getCellType() != CellType.BLANK
              isMCQ && isAnswerNotBlank
            })
          })
          .map(rowNum => parseQuestion(sheet.getRow(rowNum)).toMap).toList
      })

      Some(filteredQuestions.toIndexedSeq)
    } catch {
      case e: Exception =>
        logger.error("Error while processing Excel file", e)
        None
    }
  }
  def validateQuestions(questions: Option[IndexedSeq[Map[String, AnyRef]]]): List[Map[String, Any]] = {
    logger.info("Inside the validateQuestions")
    questions.map { questionSeq =>
      val competencyCodes: Seq[String] = questionSeq.flatMap {
        case q: Map[String, Any] @unchecked =>
          q.get("Competency Code") match {
            case Some(codes: java.util.List[String] @unchecked) => codes.asScala
            case _ => Seq.empty[String]
          }
        case _ => Seq.empty[String]
      }
      val difficultyLevel: Seq[String] = questionSeq.flatMap {
        case q: Map[String, Any] @unchecked =>
          q.get("difficultyLevel").collect {
            case levels: java.util.List[String] @unchecked => levels.asScala
          }.getOrElse(Seq.empty[String])
        case _ => Seq.empty[String]
      }

      val validatedQuestions = questionSeq.map { question =>
        val validCompetencyCodes: Seq[String] = question match {
          case q: Map[String, Any] @unchecked =>
            q.get("competencyCodes") match {
              case Some(codes: java.util.List[String] @unchecked) => codes.asScala.filter(competencyCodes.contains)
              case _ => Seq.empty[String]
            }
          case _ => Seq.empty[String]
        }
        val validDifficultyLevels: Seq[String] = question match {
          case q: Map[String, Any] @unchecked =>
            q.get("difficultyLevel") match {
              case Some(levels: java.util.List[String] @unchecked) => Option(levels.asScala).getOrElse(Seq.empty[String]).filter(difficultyLevel.contains)
              case _ => Seq.empty[String]
            }
          case _ => Seq.empty[String]
        }
        question
      }
      validatedQuestions.toList
    }.getOrElse(List.empty[Map[String, Any]])
  }
  def frameworkRead(frameworkUrl: String): Map[String, Map[String, Object]] = {
    try {
      val url = new URL(frameworkUrl)
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      logger.info("Inside frameworkRead")
      val reader = new BufferedReader(new InputStreamReader(connection.getInputStream))
      val response = new StringBuilder()
      var line: String = null

      while ({line = reader.readLine(); line != null}) {
        response.append(line)
      }

      val responseBody = response.toString()

      val objectMapper = new ObjectMapper()
      val frameworkJson = objectMapper.readTree(responseBody)

      val competencyMap = for {
        category <- frameworkJson.get("result").get("framework").get("categories").elements().asScala
        if (category.get("code").asText() == "subject")
        term <- category.get("terms").elements().asScala
      } yield {
        val competencyName = term.get("name").asText()
        val competencyId = term.get("identifier").asText()

        val levelsMap = term.get("associations") match {
          case null => Map.empty[String, Map[String, String]] // or any default value
          case assocArray if assocArray.isArray =>
            assocArray.elements().asScala.flatMap { association =>
              val levelName = association.get("name").asText()
              val levelId = association.get("identifier").asText()
              val levelDetails = Map("levelName" -> levelName)
              Some(levelId -> levelDetails)
            }.toMap
          case _ => Map.empty[String, Map[String, String]] // or any default value
        }

        val competencyDetails = Map("competencyName" -> competencyName, "levels" -> levelsMap)
        (competencyId, competencyDetails)
      }
      competencyMap.toMap
    } catch {
      case ioException: IOException =>
        ioException.printStackTrace()
        Map.empty[String, Map[String, Object]] // Return an empty map in case of an exception
    }
  }

  def buildDefaultQuestion() = {
    val defaultQuestion = new java.util.HashMap().asInstanceOf[java.util.Map[String, AnyRef]]

    defaultQuestion.put(Constants.CODE, "question")
    defaultQuestion.put(Constants.MIME_TYPE, "application/vnd.sunbird.question")
    defaultQuestion.put(Constants.OBJECT_TYPE, "Question")
    defaultQuestion.put(Constants.PRIMARY_CATEGORY, "Multiple Choice Question")
    defaultQuestion.put(Constants.QTYPE, "MCQ")
    defaultQuestion.put(Constants.NAME, "Question")
    defaultQuestion
  }

  def buildOptionMap(option: String, level: Integer, answer: Boolean) = {
    val mapOptionValue = new java.util.HashMap().asInstanceOf[java.util.Map[String, AnyRef]]
    mapOptionValue.put(Constants.BODY, option)
    mapOptionValue.put(Constants.VALUE, level)
    val mapOption = new java.util.HashMap().asInstanceOf[java.util.Map[String, AnyRef]]
    mapOption.put(Constants.ANSWER, answer.asInstanceOf[AnyRef])
    mapOption.put(Constants.VALUE, mapOptionValue)
    mapOption
  }

  def buildInteractionMap(option: String, level: String) = {
    val mapOptionValue = new java.util.HashMap().asInstanceOf[java.util.Map[String, AnyRef]]
    mapOptionValue.put(Constants.LABEL, option)
    mapOptionValue.put(Constants.VALUE, level)
    mapOptionValue
  }

  // determines whether the Opton is correct
  def isOptionAnswer(optSeq: String, answerText: String): Boolean = {

    val correctOpt = answerText.split("[,\n]").map(_.trim)

    var boolean = false
    breakable {
      for (index <- 0 until correctOpt.size) {
          boolean = correctOpt.apply(index).toLowerCase.startsWith(optSeq.toLowerCase)
          if (boolean.equals(true)) {
            break()
          }
        }
    }
    boolean
  }

  def parseQuestion(xssFRow: XSSFRow): java.util.Map[String, AnyRef] = {
    val question = buildDefaultQuestion()
    val rowContent = (0 until xssFRow.getPhysicalNumberOfCells)
      .map(colId => Option(xssFRow.getCell(colId)).getOrElse("").toString.trim)

    // Extracting values from the row content
    val medium = rowContent(0)
    val subject = rowContent(1)
    val difficultyLevel = rowContent(5)
    val gradeLevel = rowContent(7)
    val questionText = rowContent(8)
    val answer = rowContent(10)
    val board = rowContent(12)
    val channel = rowContent(13)
    val maxScore = rowContent(14).toDouble.toInt
    val assessmentType = rowContent(15)

    // Extracting options from the row content
    val options = rowContent(9).split("\n").filter(StringUtils.isNotBlank).zipWithIndex.map {
      case (option, index) => buildOptionMap(option.split("[)]")(1).trim, index, isOptionAnswer(option.split("[)]")(0).trim, answer))
    }.toList.asJava

    // Building response1 map
    val response1 =  {
      val mapResponse = new util.HashMap[String, AnyRef]()
      mapResponse.put(Constants.MAX_SCORE, maxScore.asInstanceOf[AnyRef])
      mapResponse.put(Constants.CARDINALITY, "single")
      mapResponse.put(Constants.TYPE, "integer")

      val mapCorrectResponse = new util.HashMap[String, AnyRef]()
      mapCorrectResponse.put(Constants.VALUE, answer)

      val mapOutcomes = new util.HashMap[String, AnyRef]()
      mapOutcomes.put(Constants.SCORE, maxScore.asInstanceOf[AnyRef])
      mapCorrectResponse.put(Constants.OUTCOMES, mapOutcomes)

      mapResponse.put(Constants.CORRECT_RESPONSE, mapCorrectResponse)
      mapResponse.put(Constants.MAPPING, new util.ArrayList())

      Collections.singletonMap("response1", mapResponse)
    }

    // Building interactionOptions
    val interactionOptions = rowContent(9).split("\n").filter(StringUtils.isNotBlank).map { option =>
      buildInteractionMap(option.split("[)]")(1).trim, difficultyLevel)
    }.toList.asJava

    // Building interaction map
    val interactionMap = createInteractionMap(interactionOptions)


    // Building editorState map
    val editorState = new util.HashMap[String, AnyRef]()
    val interactionTypes = new util.ArrayList[String]()
    interactionTypes.add("choice")
    editorState.put(Constants.OPTIONS, options)
    editorState.put(Constants.QUESTION, questionText)

    // Assembling the final question map
    question.putAll(response1)
    question.put(Constants.BOARD, board)
    question.put(Constants.INTERACTION_TYPES, interactionTypes)
    question.put(Constants.INTERACTIONS, interactionMap)
    question.put(Constants.EDITOR_STATE, editorState)

    setArrayValue(question, medium, Constants.medium)
    setArrayValue(question, subject, Constants.subject)
    setArrayValue(question, gradeLevel, Constants.gradeLevel)
    setArrayValue(question, difficultyLevel, Constants.difficultyLevel)

    question.put(Constants.BODY, questionText)
    question.put(Constants.TEMPLATE_ID, "mcq-vertical")
    question.put(Constants.ANSWER, answer)
    question.put("channel", channel)
    setArrayValue(question, assessmentType, "assessmentType")

    question
  }


  private def createInteractionMap(interactionOptions: util.List[util.Map[String, AnyRef]]) = {
    val mapOption = new util.HashMap().asInstanceOf[util.Map[String, AnyRef]]
    mapOption.put(Constants.TYPE, "choice".asInstanceOf[AnyRef])
    mapOption.put(Constants.OPTIONS, interactionOptions)
    val mapValidation = new util.HashMap().asInstanceOf[util.Map[String, AnyRef]]
    mapValidation.put("required", "Yes".asInstanceOf[AnyRef])
    val mapInteraction = new util.HashMap().asInstanceOf[util.Map[String, AnyRef]]
    mapInteraction.put("response1", mapOption)
    mapInteraction.put(Constants.VALIDATION, mapValidation)
    mapInteraction
  }

  private def setArrayValue(question: util.Map[String, AnyRef], data: String, questionKey: String) = {
    val dataArray = data.split("[|]")
    val valueList = new util.ArrayList[String]()
    dataArray.toStream.foreach(list => valueList.add(list.trim))
    question.put(questionKey, valueList)
  }
}
