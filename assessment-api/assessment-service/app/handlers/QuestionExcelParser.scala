package handlers

import com.fasterxml.jackson.databind.ObjectMapper
import org.apache.commons.lang3.StringUtils
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.xssf.usermodel.{XSSFRow, XSSFWorkbook}
import org.slf4j.{Logger, LoggerFactory}
import org.sunbird.cache.impl.RedisCache
import utils.Constants

import java.io.{BufferedReader, File, FileInputStream, FileNotFoundException, IOException, InputStreamReader}
import java.net.{HttpURLConnection, URL}
import java.util
import scala.collection.JavaConverters._
import scala.util.control.Breaks._

object QuestionExcelParser {

  private val logger: Logger = LoggerFactory.getLogger(RedisCache.getClass.getCanonicalName)

  def getQuestions(fileName: String, file: File) = {
    try {
      val workbook = new XSSFWorkbook(new FileInputStream(file))
      val sheets = (0 until workbook.getNumberOfSheets).map(index => workbook.getSheetAt(index))  // iterates over the excelsheet
      sheets.flatMap(sheet => {
        logger.info("Inside the getQuestions")
        (1 until sheet.getPhysicalNumberOfRows)  // iterates over each row in the sheet
          .filter(rowNum => {
            val oRow = Option(sheet.getRow(rowNum))
            // matching the row value to determine the value of objects
            oRow match {
              case Some(x) => {
                val questionType = sheet.getRow(rowNum).getCell(11)
                val isMCQ = questionType.toString.trim.equalsIgnoreCase(Constants.MCQ_SINGLE_SELECT)// checks questionType is MCQ
                //val isMTF = Constants.MTF.equals(questionType.toString) || Constants.MATCH_THE_FOLLOWING.equals(questionType.toString)
                //val isFITB = Constants.FITB.equals(questionType.toString)
                val answerCell = sheet.getRow(rowNum).getCell(10)
                val isAnswerNotBlank = answerCell.getCellType() != CellType.BLANK
                //isMCQ || isMTF || isFITB && isAnswerNotBlank
                isMCQ && isAnswerNotBlank
              }
              case None => false
            }
          })
          .map(rowNum => parseQuestion(sheet.getRow(rowNum))).toList
      })
    }

    catch {
      case e: Exception => throw new Exception("Invalid File")
    }
  }
  def validateQuestions(questions: List[Map[String, Any]]): List[Map[String, Any]] = {
    val competencyCodes: Seq[String] = questions.flatMap {
      case question: Map[String, Any] @unchecked =>
        question.get("Competency Code") match {
          case Some(codes: java.util.List[String] @unchecked) => codes.asScala
          case _ => Seq.empty[String]
        }
      case _ => Seq.empty[String]
    }
    val competencyLevels: Seq[String] = questions.flatMap {
      case question: Map[String, Any] @unchecked =>
        question.get("Competency Level").collect {
          case levels: java.util.List[String] @unchecked => levels.asScala
        }.getOrElse(Seq.empty[String])
      case _ => Seq.empty[String]
    }
    questions.map { question =>
      val validCompetencyCodes: Seq[String] = question match {
        case q: Map[String, Any] @unchecked =>
          q.get("competencyCodes") match {
            case Some(codes: java.util.List[String] @unchecked) => codes.asScala.filter(competencyCodes.contains)
            case _ => Seq.empty[String]
          }
        case _ => Seq.empty[String]
      }
      val validCompetencyLevels: Seq[String] = question match {
        case q: Map[String, Any] @unchecked =>
          q.get("competencyLevels") match {
            case Some(levels: java.util.List[String] @unchecked) => Option(levels.asScala).getOrElse(Seq.empty[String]).filter(competencyLevels.contains)
            case _ => Seq.empty[String]
          }
        case _ => Seq.empty[String]
      }
      question
    }
  }
  def frameworkRead(frameworkUrl: String): Map[String, Map[String, Object]] = {
    try {
      val url = new URL(frameworkUrl)
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")

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

  def buildInteractionMap(option: String, level: Integer) = {
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

  def parseQuestion(xssFRow: XSSFRow) = {
    val question = buildDefaultQuestion()

    val rowContent = (0 until xssFRow.getPhysicalNumberOfCells)
      .map(colId => Option(xssFRow.getCell(colId)).getOrElse("").toString).toList

    //fetches data from sheet
    // this is the role(medium)
    val medium = rowContent.apply(0)
    // this is competency label
    val subject = rowContent.apply(1)
    // this val is for competency level label
    val difficultyLevel = rowContent.apply(5)
    // this val is for activity(GradeLevel)
    val gradeLevel = rowContent.apply(7)
    val questionText = rowContent.apply(8)
    val answer = rowContent.apply(10).trim
    val board = rowContent.apply(12).trim
    val channel = rowContent.apply(13).trim
    val maxScore:Integer = rowContent.apply(14).trim.toDouble.intValue()
    val assessmentType = rowContent.apply(15)

    var i = -1
    val options = new util.ArrayList[util.Map[String, AnyRef]](rowContent.apply(9).split("\n").filter(StringUtils.isNotBlank).map(o => {
      val option = o.split("[)]").toList
      val optSeq = option.apply(0).trim

      val optText = option.apply(1).trim
      i += 1
      buildOptionMap(optText, i, isOptionAnswer(optSeq, answer))
    }).toList.asJava)

    var j = -1
    val mapRepsonse = new util.HashMap().asInstanceOf[util.Map[String, AnyRef]]
    val repsonse1 = new util.HashMap().asInstanceOf[util.Map[String, AnyRef]]
    val responseDeclarationOption = new util.ArrayList[util.Map[String, AnyRef]](rowContent.apply(9).split("\n").filter(StringUtils.isNotBlank).map(o => {
      val option = o.split("[)]").toList
      val optSeq = option.apply(0).trim

      val optText = option.apply(1).trim
      j += 1
      if(isOptionAnswer(optSeq, answer)){
        mapRepsonse.put(Constants.MAX_SCORE,maxScore.asInstanceOf[AnyRef])
        mapRepsonse.put(Constants.CARDINALITY, "single")
        mapRepsonse.put(Constants.TYPE, "integer")
        val mapCorrectResponse = new util.HashMap().asInstanceOf[util.Map[String, AnyRef]]
        mapCorrectResponse.put(Constants.VALUE, String.valueOf(j))
        val mapOutcomes = new util.HashMap().asInstanceOf[util.Map[String, AnyRef]]
        mapOutcomes.put(Constants.SCORE, maxScore.asInstanceOf[AnyRef])
        mapCorrectResponse.put(Constants.OUTCOMES, mapOutcomes)
        mapRepsonse.put(Constants.CORRECT_RESPONSE, mapCorrectResponse)
        mapRepsonse.put(Constants.MAPPING, new util.ArrayList())
      }
      repsonse1.put("response1", mapRepsonse)
      repsonse1
    }).toList.asJava)

    var k = -1
    val interactionOptions = new util.ArrayList[util.Map[String, AnyRef]](rowContent.apply(9).split("\n").filter(StringUtils.isNotBlank).map(o => {
      val option = o.split("[)]").toList
      val optSeq = option.apply(0).trim

      val optText = option.apply(1).trim
      k += 1
      buildInteractionMap(optText, k)
    }).toList.asJava)

    val mapInteraction: _root_.java.util.Map[_root_.java.lang.String, AnyRef] = createInteractionMap(interactionOptions)
    val editorState = new util.HashMap().asInstanceOf[util.Map[String, AnyRef]]
    val interactionTypes = new util.ArrayList[String]()
    interactionTypes.add("choice")
    question.put(Constants.BOARD, board)
    question.put(Constants.INTERACTION_TYPES,interactionTypes)
    question.put(Constants.INTERACTIONS,mapInteraction)
    question.put(Constants.RESPONSE_DECLARATION,repsonse1)
    setArrayValue(question, medium, Constants.medium)
    setArrayValue(question, subject, Constants.subject)
    setArrayValue(question, gradeLevel, Constants.gradeLevel)
    setArrayValue(question, difficultyLevel, Constants.difficultyLevel)
    editorState.put(Constants.OPTIONS, options)
    editorState.put(Constants.QUESTION, questionText)
    logger.info("Inside the parseQuestion")
    question.put(Constants.BODY, questionText)
    question.put(Constants.EDITOR_STATE, editorState)
    question.put(Constants.TEMPLATE_ID, "mcq-vertical")
    question.put(Constants.ANSWER, answer)
    question.put("channel", channel)
    question.put("assessmentType",assessmentType)
    question
  }


  private def createInteractionMap(interactionOptions: util.ArrayList[util.Map[String, AnyRef]]) = {
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
