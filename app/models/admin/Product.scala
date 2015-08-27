package models.admin

import java.util.Date


/**
 * Domain model.
 */
case class Product(
                    ean: Long,
                    date: Date,
                    description: String,
                    stock: Int,
                    price: BigDecimal
                    )

object Product {

  /**
   * Alternative JSON formatter that includes the price.
   */

  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  val adminProductWrites: Writes[Product] = (
    (JsPath \ "ean").write[Long] and
      (JsPath \ "date").write[Date] and
      (JsPath \ "description").write[String] and
      (JsPath \ "stock").write[Int] and
      (JsPath \ "price").write[BigDecimal]
    )(unlift(Product.unapply))

}
