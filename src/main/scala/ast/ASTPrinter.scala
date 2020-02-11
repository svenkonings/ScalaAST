package ast

import java.nio.file.{Files, Paths}

import scala.meta.{Source, Tree}
import scala.meta.inputs.Input

object ASTPrinter {
  def main(args: Array[String]): Unit = {
    val path = Paths.get("C:\\Users\\SvenK\\Documents\\slang\\mp-metrics\\src\\test\\resources", "ScalaLambdas.scala")
    val text = Files.readString(path)
    val input = Input.VirtualFile(path.toString, text)
    val tree = input.parse[Source].get
    val json = anyToString(tree)
    print(json)
  }

  def anyToString(any: Any, indent: Int = 0, indentFirst: Boolean = true, newLineLast : Boolean = true): String = {
    val builder = new StringBuilder
    if (indentFirst) builder ++= " " * indent
    builder ++= (any match {
      case tree: Tree => treeToString(tree, indent)
      case list: List[_] => listToString(list, indent)
      case (name: String, value: Any) => pairToString(name, value, indent)
      case string: String => "\"" + string + "\""
      case Some(value) => anyToString(value, indent, indentFirst = false, newLineLast = false)
      case _ => any.toString
    })
    if (newLineLast) builder ++= "\n"
    builder.toString
  }

  def treeToString(tree: Tree, indent: Int = 0): String = {
    val builder = new StringBuilder
    builder ++= tree.productPrefix
    if (tree.productArity > 0) {
      builder ++= " {\n"
      tree.productFields.zip(tree.productIterator).foreach { case (key, value) =>
        builder ++= pairToString(key, value, indent + 2)
      }
      builder ++= " " * indent
      builder ++= "}"
    }
    builder.toString
  }

  def listToString(list: List[_], indent: Int = 0): String = {
    val builder = new StringBuilder
    if (list.nonEmpty) {
      builder ++= "[\n"
      for (any <- list) {
        builder ++= anyToString(any, indent + 2)
      }
      builder ++= " " * indent
      builder ++= "]"
    } else {
      builder ++= "[]"
    }
    builder.toString
  }

  def pairToString(name: String, any: Any, indent: Int = 0): String = {
    " " * indent + name + ": " + anyToString(any, indent, indentFirst = false)
  }

}
