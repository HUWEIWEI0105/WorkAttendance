import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by Shikieiki Yamaxanadu on 2016/5/4.
 */
object PreDealData {
  /**
   * 主函数
   * @param args
   *             (0) 输入
   *             (1) 输出
   */
  def main(args: Array[String]) {
    val fileSource = Source.fromFile(args(0), "UTF-8")
    val lineIter = fileSource.getLines()

    val data = new mutable.HashMap[String, ArrayBuffer[String]]()

    for(line <- lineIter){
      if(!data.contains(line.split("\t")(0))){
        data(line.split("\t")(0)) = new ArrayBuffer[String]
      }

      data(line.split("\t")(0)) +=
        line.split("\t")(0) + "," +
        line.split("\t")(1).replace('-','/') + "," +
        {
          if(line.split("\t")(3).toInt == 0)"上班签到"
          else "下班签退"
        }
    }
    fileSource.close()

    val out = new PrintWriter(args(1))
    for((k, v) <- data){
      v.foreach(out.println)
    }
    out.close()
  }

  def TrPr(original: Int, hitRatio: Double  = 0.0, defense: Double = 0.0): Double ={
    original * (1.0 + hitRatio) / (1.0 + defense)
  }
}
