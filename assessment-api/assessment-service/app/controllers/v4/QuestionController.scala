package controllers.v4

import akka.actor.{ActorRef, ActorSystem}
import controllers.BaseController
import handlers.{CompetencyExcelParser, QuestionExcelParser}
import org.slf4j.{Logger, LoggerFactory}
import org.sunbird.cache.impl.RedisCache
import org.sunbird.common.dto.Response
import org.sunbird.utils.AssessmentConstants
import play.api.libs.json.Json
import play.api.mvc.{ControllerComponents, Result}
import utils.{ActorNames, ApiId, Constants, JavaJsonUtils, QuestionOperations}

import javax.inject.{Inject, Named}
import scala.collection.JavaConverters._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}


class QuestionController @Inject()(@Named(ActorNames.QUESTION_ACTOR) questionActor: ActorRef, cc: ControllerComponents, actorSystem: ActorSystem)(implicit exec: ExecutionContext) extends BaseController(cc) {

  val objectType = "Question"
  val schemaName: String = "question"
  val version = "1.0"

  private val logger: Logger = LoggerFactory.getLogger(RedisCache.getClass.getCanonicalName)

  def create() = Action.async { implicit request =>
    val headers = commonHeaders()
    val body = requestBody()
    val question = body.getOrDefault("question", new java.util.HashMap()).asInstanceOf[java.util.Map[String, AnyRef]]
    question.putAll(headers)
    val questionRequest = getRequest(question, headers, QuestionOperations.createQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    getResult(ApiId.CREATE_QUESTION, questionActor, questionRequest)
  }

  def privateRead(identifier: String, mode: Option[String], fields: Option[String]) = Action.async { implicit request =>
    val headers = commonHeaders()
    val question = new java.util.HashMap().asInstanceOf[java.util.Map[String, Object]]
    question.putAll(headers)
    question.putAll(Map("identifier" -> identifier, "fields" -> fields.getOrElse(""), "mode" -> mode.getOrElse("read")).asJava)
    val questionRequest = getRequest(question, headers, QuestionOperations.readPrivateQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    getResult(ApiId.READ_PRIVATE_QUESTION, questionActor, questionRequest)
  }

  def update(identifier: String) = Action.async { implicit request =>
    val headers = commonHeaders()
    val body = requestBody()
    val question = body.getOrDefault("question", new java.util.HashMap()).asInstanceOf[java.util.Map[String, Object]];
    question.putAll(headers)
    val questionRequest = getRequest(question, headers, QuestionOperations.updateQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    questionRequest.getContext.put("identifier", identifier)
    getResult(ApiId.UPDATE_QUESTION, questionActor, questionRequest)
  }

  def review(identifier: String) = Action.async { implicit request =>
    val headers = commonHeaders()
    val body = requestBody()
    val question = body.getOrDefault("question", new java.util.HashMap()).asInstanceOf[java.util.Map[String, Object]];
    question.putAll(headers)
    val questionRequest = getRequest(question, headers, QuestionOperations.reviewQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    questionRequest.getContext.put("identifier", identifier)
    getResult(ApiId.REVIEW_QUESTION, questionActor, questionRequest)
  }

  def publish(identifier: String) = Action.async { implicit request =>
    val headers = commonHeaders()
    val body = requestBody()
    val question = body.getOrDefault("question", new java.util.HashMap()).asInstanceOf[java.util.Map[String, Object]];
    question.putAll(headers)
    val questionRequest = getRequest(question, headers, QuestionOperations.publishQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    questionRequest.getContext.put("identifier", identifier)
    getResult(ApiId.PUBLISH_QUESTION, questionActor, questionRequest)
  }

  def retire(identifier: String) = Action.async { implicit request =>
    val headers = commonHeaders()
    val question = new java.util.HashMap().asInstanceOf[java.util.Map[String, Object]]
    question.putAll(headers)
    val questionRequest = getRequest(question, headers, QuestionOperations.retireQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    questionRequest.getContext.put("identifier", identifier)
    getResult(ApiId.RETIRE_QUESTION, questionActor, questionRequest)
  }

  def importQuestion() = Action.async { implicit request =>
    val headers = commonHeaders()
    val body = requestBody()
    body.putAll(headers)
    val questionRequest = getRequest(body, headers, QuestionOperations.importQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    getResult(ApiId.IMPORT_QUESTION, questionActor, questionRequest)
  }

  def systemUpdate(identifier: String) = Action.async { implicit request =>
    val headers = commonHeaders()
    val body = requestBody()
    val content = body.getOrDefault(schemaName, new java.util.HashMap()).asInstanceOf[java.util.Map[String, Object]];
    content.putAll(headers)
    val questionRequest = getRequest(content, headers, QuestionOperations.systemUpdateQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    questionRequest.getContext.put("identifier", identifier);
    getResult(ApiId.SYSTEM_UPDATE_QUESTION, questionActor, questionRequest)
  }


  def reject(identifier: String) = Action.async { implicit request =>
    val headers = commonHeaders()
    val body = requestBody()
    val question = body.getOrDefault(schemaName, new java.util.HashMap()).asInstanceOf[java.util.Map[String, Object]];
    question.putAll(headers)
    val questionRequest = getRequest(question, headers, QuestionOperations.rejectQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    questionRequest.getContext.put("identifier", identifier)
    getResult(ApiId.REJECT_QUESTION, questionActor, questionRequest)
  }

  def copy(identifier: String, mode: Option[String]) = Action.async { implicit request =>
    val headers = commonHeaders()
    val body = requestBody()
    val question = body.getOrDefault("question", new java.util.HashMap()).asInstanceOf[java.util.Map[String, Object]];
    question.putAll(headers)
    question.putAll(Map("identifier" -> identifier, "mode" -> mode.getOrElse(""), "copyType" -> AssessmentConstants.COPY_TYPE_DEEP).asJava)
    val questionRequest = getRequest(question, headers, QuestionOperations.copyQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    getResult(ApiId.COPY_QUESTION, questionActor, questionRequest)
  }

  //Create question by uploading excel file
  def uploadExcel() = Action(parse.multipartFormData) { implicit request =>
    val result: Option[Result] = request.body.file("file").map { filePart =>
      val absolutePath = filePart.ref.path.toAbsolutePath
      val fileName: String = filePart.filename
      val questions: Option[IndexedSeq[Map[String, AnyRef]]] = QuestionExcelParser.getQuestions(fileName, absolutePath.toFile)

      // Step 2: Validate questions based on competency and competency levels
      val validatedQuestions: List[Map[String, Any]] = QuestionExcelParser.validateQuestions(questions)

      // Step 3: Read framework from the API
      val frameworkMap = QuestionExcelParser.frameworkRead(AssessmentConstants.FRAMEWORK_READ_URL)

      // Step 4: Check if questions are valid against the framework
      val isQuestionsValid: Boolean = validatedQuestions.forall { question =>
        val competency: Seq[String] = question.getOrElse("subject", new java.util.ArrayList[String]()).asInstanceOf[java.util.ArrayList[String]].asScala
        val difficultyLevel: Seq[String] = question.getOrElse("difficultyLevel", new java.util.ArrayList[String]()).asInstanceOf[java.util.ArrayList[String]].asScala

        // Check if competency and competencyLevels are present in the framework
        competency.forall(frameworkMap.contains) && difficultyLevel.forall(level => frameworkMap.getOrElse(level, Map.empty).nonEmpty)
      }

      if (isQuestionsValid) {
        // Step 5: Process questions if valid
        val futures = validatedQuestions.map { question =>
          val headers = commonHeaders(request.headers)
          val javaQuestion: java.util.Map[String, AnyRef] = question.mapValues(_.asInstanceOf[AnyRef]).asJava
          val questionRequest = getRequest(javaQuestion, headers, QuestionOperations.createQuestionByBulkUpload.toString)
          setRequestContext(questionRequest, version, objectType, schemaName)
          getResponse(ApiId.CREATE_QUESTION, questionActor, questionRequest)
        }

        // Step 6: Await and handle the result
        val f = Future.sequence(futures).map(results => results.map(_.asInstanceOf[Response]).groupBy(_.getResponseCode.toString).mapValues(listResult => {
          listResult.map(result => {
            setResponseEnvelope(result)
            JavaJsonUtils.serialize(result.getResult)
          })
        })).map(f => Ok(Json.stringify(Json.toJson(f))).as("application/json"))

        Await.result(f, Duration.apply("300s"))
      } else {
        // If questions are not valid, respond with an error
        BadRequest("Invalid questions: competency or competencyLevels not present in the framework")
      }
    }

    // Return the result or a default BadRequest if the file is not present
    result.getOrElse(BadRequest("File not provided"))
  }

  def createFrameworkMappingData() = Action(parse.multipartFormData) { implicit request =>
    val competency = request.body
      .file("file")
      .map { filePart =>
        val absolutePath = filePart.ref.path.toAbsolutePath
        println("createFrameworkMappingData:= " + absolutePath)
        CompetencyExcelParser.getCompetency(absolutePath.toFile)
      }
    val futures = competency.get.map(competncy => {
      val headers = commonHeaders(request.headers)
      logger.info("put headers  " + headers)
      val questionRequest = getRequest(competncy, headers, QuestionOperations.createQuestion.toString)
      logger.info("After the questionRequest")
      setRequestContext(questionRequest, version, objectType, schemaName)
      logger.info("After the setRequestContext")
      getResponse(ApiId.CREATE_QUESTION, questionActor, questionRequest)
    }
    )
    val futures1 = competency.get.map(competncy => {
      val headers = commonHeaders(request.headers)
      System.out.println("Headers is " + headers)
      val body = competencyRequestBody()
      System.out.println("body is " + body)
      val question = body.getOrDefault("competency", new java.util.HashMap()).asInstanceOf[java.util.Map[String, AnyRef]]
      competncy.putAll(headers)
      val questionRequest = getRequest(competncy, headers, QuestionOperations.bulkUploadFrameworkMapping.toString)
      setRequestContext(questionRequest, version, "competency", "competency")
      getResult(ApiId.FRAMEWORK_COMPETENCY_QUESTION, questionActor, questionRequest)
    })

    logger.info("After the getResponse")
    val f = Future.sequence(futures1).map(results => results.map(_.asInstanceOf[Response]).groupBy(_.getResponseCode.toString).mapValues(listResult => {
      listResult.map(result => {
        setResponseEnvelope(result)
        JavaJsonUtils.serialize(result.getResult)
      })
    })).map(f => Ok(Json.stringify(Json.toJson(f))).as("application/json"))
    logger.info("in Future sequence")
    Await.result(f, Duration.apply("30s"))
  }

  def editorList(fields: Option[String]) = {
    fetchQuestions(fields, true)
  }

  def fetchQuestions(fields: Option[String], exclusive: Boolean) = Action.async { implicit request =>
    val headers = commonHeaders()
    val body = requestBody()
    val question = body.getOrDefault("search", new java.util.HashMap()).asInstanceOf[java.util.Map[String, Object]];
    question.putAll(headers)
    question.put("fields", fields.getOrElse(""))
    if (exclusive) question.put("isEditor", "true")
    val questionRequest = getRequest(question, headers, QuestionOperations.listQuestions.toString)
    questionRequest.put("identifiers", questionRequest.get("identifier"))
    setRequestContext(questionRequest, version, objectType, schemaName)
    getResult(ApiId.LIST_QUESTIONS, questionActor, questionRequest)
  }

  def editorRead(identifier: String, mode: Option[String], fields: Option[String]) = {
    readQuestion(identifier, mode, fields, true)
  }

  def list(fields: Option[String]) = {
    fetchQuestions(fields, false)
  }

  def read(identifier: String, mode: Option[String], fields: Option[String]) = {
    readQuestion(identifier, mode, fields, false)
  }

  def readQuestion(identifier: String, mode: Option[String], fields: Option[String], exclusive: Boolean) = Action.async { implicit request =>
    val headers = commonHeaders()
    val question = new java.util.HashMap().asInstanceOf[java.util.Map[String, Object]]
    question.putAll(headers)
    question.putAll(Map("identifier" -> identifier, "fields" -> fields.getOrElse(""), "mode" -> mode.getOrElse("read")).asJava)
    if (exclusive) question.put("isEditor", "true")
    val questionRequest = getRequest(question, headers, QuestionOperations.readQuestion.toString)
    setRequestContext(questionRequest, version, objectType, schemaName)
    getResult(ApiId.READ_QUESTION, questionActor, questionRequest)
  }
}
