package controllers

import play.api.mvc._
import play.api.mvc.Controller
import models.Product

/**
 * Alternative controller that uses the combinator-based Writes.
 */
object ProductsWithCombinators extends Controller {

  import java.util.Date

  import org.joda.time.DateTime
  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  val productWrites: Writes[Product] = (
    (JsPath \ "ean").write[String] and
      (JsPath \ "date").write[Date] and
      (JsPath \ "sold").write[Boolean] and
      (JsPath \ "productname").write[String] and
      (JsPath \ "userid").write[String] and
      (JsPath \ "price").write[Double] and
      (JsPath \ "stock").write[Int] and
      (JsPath \ "brand").write[String] and
      (JsPath \ "category").write[String] and
      (JsPath \ "condition").write[String]
    )(unlift(Product.unapply))

  /**
   * Returns details of the given product.
   */
  def details(ean: String) = Action {
    Product.findByEan(ean).map { product =>
      Ok(Json.toJson(product)(productWrites))
    }.getOrElse(NotFound)
  }

}
