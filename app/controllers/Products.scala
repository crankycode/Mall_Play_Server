package controllers

import play.api.mvc._
import play.api.mvc.Controller
import models.Product
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.Logger
import java.util.Date


/**
 * Controller for products HTTP interface.
 */
object Products extends Controller {

  import org.joda.time.DateTime

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

}
