package controllers

import java.io.File

import org.codehaus.jackson.JsonNode
import play.api.data._
import play.api.data.Form._
import play.api.libs.json
import play.api.mvc._
import play.api.mvc.Controller
import models.Product
import models.UserData
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.Logger
import java.util.Date

import play.mvc.Http

import play.api._
import play.api.mvc._
import play.api.libs.json._
// you need this import to have combinators
import play.api.libs.functional.syntax._

import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
/**
 * Controller for products HTTP interface.
 */
object Products extends Controller {


  import org.joda.time.DateTime

//  val userForm = Form(
//    mapping(
//      "name" -> text,
//      "age" -> number
//    )(UserData.apply)(UserData.unapply)
//  )

//  val userForm = Form(
//    mapping(
//      "ean" -> text,
//      "date" -> DATE,
//      "sold" -> Boolean,
//      "productname" -> String,
//      "userid" -> String,
//      "price" -> Double,
//      "stock" -> Int,
//      "brand" -> String,
//      "category" -> String,
//      "condition" -> String)(Product.apply)(Product.unapply)
//  )
case class UserData(name: String, age: Int)

  val userForm = Form(
    mapping(
      "name" -> text,
      "age" -> number
    )(UserData.apply)(UserData.unapply)
  )

  /**
   * Returns an array of products’ EAN codes.
   */
  def list = Action {
    val productCodes = Product.findAll.map(_.ean)
    Ok(Json.toJson(productCodes))
  }

  /**
   * Formats a Product instance as JSON.
   */
  implicit object ProductWrites extends Writes[Product] {
    def writes(p: Product) = Json.obj(
      "ean" -> Json.toJson(p.ean),
      "date" -> Json.toJson(p.date),
      "sold" -> Json.toJson(p.sold),
      "productname" -> Json.toJson(p.productname),
      "userid" -> Json.toJson(p.userid),
      "price" -> Json.toJson(p.price),
      "stock" -> Json.toJson(p.stock),
      "brand" -> Json.toJson(p.brand),
      "category" -> Json.toJson(p.category),
      "condition" -> Json.toJson(p.condition)
    )
  }

  /**
   * Returns details of the given product.
   */
  def details(ean: String) = Action {
    Product.findByEan(ean).map { product =>
      Ok(Json.toJson(product))
    }.getOrElse(NotFound)
  }

  /**
   * Parses a JSON object
   */
  implicit val productReads: Reads[Product] = (
    (JsPath \ "ean").read[String] and
      (JsPath \ "date").read[Date] and
      (JsPath \ "sold").read[Boolean] and
      (JsPath \ "productname").read[String] and
      (JsPath \ "userid").read[String] and
      (JsPath \ "price").read[Double] and
      (JsPath \ "stock").read[Int] and
      (JsPath \ "brand").read[String] and
      (JsPath \ "category").read[String] and
      (JsPath \ "condition").read[String]
    )(Product.apply _)


  /**
   * Saves a product
   */

  def save(ean: String) = Action(parse.json) { request =>
    Logger.info("start saving")
    println(request)
    try {
      import play.api.http.ContentTypes
      val productJson = request.body
      val product = productJson.as[Product]
      Product.save(product)
      Ok
    }
    catch {
      case e: IllegalArgumentException => BadRequest("Product not found")
      case e: Exception => {
        Logger.info("exception = %s" format e)
        BadRequest("Invalid EAN")
      }
    }
  }

  //  def upload = Action(parse.multipartFormData) { request =>
  //    request.body.file("picture").map { picture =>
  //      import java.io.File
  //      val filename = picture.filename
  //      val contentType = picture.contentType
  //      picture.ref.moveTo(new File("/tmp/picture"))
  //      Ok("File uploaded")
  //    }.getOrElse {
  //      Redirect(routes.Application.index).flashing(
  //        "error" -> "Missing file"
  //      )
  //    }
  //  }


  def upload = Action(parse.multipartFormData) {
    request =>
      //      if (request.body.files.isEmpty) BadRequest("Invalid file!")
      //      else if (request.body.asFormUrlEncoded.isEmpty) BadRequest("Invalid data!")
      //      else Ok("Everything is okay!")
      request.body.file("file").map { image =>

        val imageName = image.filename
        val contentType = image.contentType.get
        image.ref.moveTo(new File("/Users/crankyfish/Documents/" + image.filename))
//        Ok(" Server Image name: " + imageName)
      }
      Ok("Everything is okay!")
  }

  def multiupload = Action(parse.multipartFormData) {
    request =>
      request.body.files.map { image =>
        val filename = image.filename
        val contentType = image.contentType.get
        image.ref.moveTo(new File("/Users/crankyfish/Documents/" + image.filename))
        //      val contentType = picture.contentType
      }

        //      request.body.file("files").map { image =>
        //
        //     val dataPart: Map[String, Seq[String]] = request.body.asFormUrlEncoded
        //
        //      val it = dataPart.iterator
        //
        //      val imageName = image.filename
        //      val contentType = image.contentType
        //
        //      while (it.hasNext) {
        //        val pair = it.next()
        //        val values = pair._2
        //
        //        // print the request debug information
        //        println(s"*** content-type: ${request.contentType}")
        //        println(s"*** headers: ${request.headers}")
        //        println(s"*** body: ${request.body}")
        //        println(s"*** query string: ${request.rawQueryString}")
        //        println("The Values are: " + pair._1, values)
        //      }

        //      image.ref.moveTo(new File("/Users/crankyfish/Documents/" + image.filename))

//    }
      Ok("Receive MultiImage is okay!")
  }




  def multiuploaddata = Action(parse.multipartFormData) {
    request =>

//      println("my body: " + request.body.dataParts)

//      request.body.files.map { image =>
//        val filename = image.filename
//        val contentType = image.contentType.get
//        image.ref.moveTo(new File("/Users/crankyfish/Documents/" + image.filename))
//        //      val contentType = picture.contentType
//      }

      val raw = request.body.dataParts.map(x => x._2)
      println(raw)
//      println(body)
//      val raw = request.body.dataParts.map(x => x._2)

//      println("myMap:" + raw)
      //      println("The ean: " + ean)

//      println(productJsValue.as[Product])
//      val bind = raw
//      val json = Json.toJson(raw)
//      println(json)
//      println(json.validate[Product])
//      val product = json.as[Product]
//      Product.save(product)
      Ok("Receive MultiImage is okay!")
  }
}
