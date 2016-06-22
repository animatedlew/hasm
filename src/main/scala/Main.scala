import java.io._
import com.animatedlew.HackParser
import language.postfixOps

object Main {
  def main(args: Array[String]): Unit = {
    println(s"Hack Assembler 1.0\nLewis Moronta \u00A9 2016")
    if (args.length == 1) {
      val p = new HackParser
      val file = if (args.head.contains(".asm")) args.head else s"${args.head}.asm"
      println(s"Input: $file")
      val ml = p.run(new FileReader(file))
      val out = file.replace(".asm", ".hack")
      val pw = new PrintWriter(out)
      try ml foreach { case (address, op) => pw.println(op) }
      finally pw.close()
      println(s"Output: $out")
    } else println("Please provide a valid file name.")
  }
}

