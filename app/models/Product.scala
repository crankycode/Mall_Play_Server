package models

import java.util.Date
import java.text.SimpleDateFormat
import org.joda.time.DateTime
import play.api.data.format.Formats


case class UserData(name: String, age: Int)
/**
 * An entry in the product catalogue.
 *
 * @param ean EAN-13 code - a unique product identifier
 * @param name Product name
 * @param description Product description
 */
case class Product(ean: String,
                   date: Date,
                   sold: Boolean,
                   productname: String,
                   userid: String,
                   price: Double,
                   stock: Int,
                   brand: String,
                   category: String,
                   condition: String)

/**
 * Products data access
 */
object Product {


  val format = new SimpleDateFormat("yyyy-MM-dd")
  format.format(new Date());

  var products = Set(
    Product("5010255079763L", format.parse("2015-03-02"), true, "Popcorn", "Crankycode", 340.0, 10, "Nike", "Instrument", "Guitar"),
    Product("5018206244666L", format.parse("2015-03-02"), true, "ChickenPie", "crankycode", 340.0, 10, "Nike", "Instrument", "Guitar"),
    Product("5018306332812L", format.parse("2015-03-02"), true, "FishCake", "crankycode", 340.0, 10, "Nike", "Instrument", "Guitar"),
    Product("5018306312913L", format.parse("2015-03-02"), true, "Fries", "crankycode", 340.0, 10, "Nike", "Instrument", "Guitar"),
    Product("5018206244611L", format.parse("2015-03-02"), true, "Pineapple", "crankycode", 340.0, 10, "Nike", "Instrument", "Guitar")
  )

  /**
   * Products sorted by EAN code.
   */
  def findAll = this.products.toList.sortBy(_.ean)

  /**
   * The product with the given EAN code.
   */
  def findByEan(ean: String) = this.products.find(_.ean == ean)

  /**
   * Saves a product to the catalog.
   */
  def save(product: Product) = {

    findByEan(product.ean).map(oldProduct =>
      this.products = this.products - oldProduct + product
    ).getOrElse(
        this.products += product
        //        throw new IllegalArgumentException("Product not found")
      )
  }
}

case class Product1(ean: String,
                   date: Long,
                   sold: Boolean,
                   productname: String,
                   userid: String,
                   price: Double,
                   stock: Int,
                   brand: String,
                   category: String,
                   condition: String)

/**
 * Products data access
 */
object Product1 {


  val format = new SimpleDateFormat("yyyy-MM-dd")
  format.format(new Date());

  var products1 = Set(
    Product("5010255079763L", format.parse("2015-03-02"), true, "Popcorn", "Crankycode", 340.0, 10, "Nike", "Instrument", "Guitar"),
    Product("5018206244666L", format.parse("2015-03-02"), true, "ChickenPie", "crankycode", 340.0, 10, "Nike", "Instrument", "Guitar"),
    Product("5018306332812L", format.parse("2015-03-02"), true, "FishCake", "crankycode", 340.0, 10, "Nike", "Instrument", "Guitar"),
    Product("5018306312913L", format.parse("2015-03-02"), true, "Fries", "crankycode", 340.0, 10, "Nike", "Instrument", "Guitar"),
    Product("5018206244611L", format.parse("2015-03-02"), true, "Pineapple", "crankycode", 340.0, 10, "Nike", "Instrument", "Guitar")
  )

  /**
   * Products sorted by EAN code.
   */
  def findAll = this.products1.toList.sortBy(_.ean)

  /**
   * The product with the given EAN code.
   */
  def findByEan(ean: String) = this.products1.find(_.ean == ean)

  /**
   * Saves a product to the catalog.
   */
  def save(product: Product) = {

    findByEan(product.ean).map(oldProduct =>
      this.products1 = this.products1 - oldProduct + product
    ).getOrElse(
        this.products1 += product
        //        throw new IllegalArgumentException("Product not found")
      )
  }
}
