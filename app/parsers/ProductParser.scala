package parsers

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Created by crankyfish on 18/9/15.
 */
class ProductParser extends JavaTokenParsers {

  def space = regex("[ \\n]*".r)
  def message = "[A-Z]".r
  def num = "\\p{Digit}".r

}
