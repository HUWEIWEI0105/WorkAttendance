import java.io.PrintWriter

import scala.collection.mutable
import scala.io.Source

/**
 * Created by Won Andwon on 2016/4/10.
 * 通过考勤记录获得某人或全体的有打卡天数
 */
object CountAttendance {
  /**
   * 主函数
   * @param args
   *             (0) 统计谁的信息，ALL则为全体
   *             (1) 统计的起始月份，包含。务必为输入文件中有的且未统计的 例如 2016/1
   *             (2) 统计的终止月份，不包含。此月之前一月务必为输入文件中有的且未统计的
   *                例如参数 2016/4 则终止日期为2016-4-1 即统计到3月份（截止3-31）的数据
   *             (3) 统计的输入文件，请转换成csv格式的txt文件，务必去掉csv的首行！因为我不想为了确定的第一行还要加个判断
   *                 首行在这，都严格遵守格式：
   *                 姓名,出勤时间,出勤状态,更正状态,异常情况,操作
   *             (4) 统计的输出文件
   */
  def main(args: Array[String]) {
    if(args(0).compareTo("ALL") == 0) {
      val s_year: Int = args(1).split("/")(0).toInt
      if(s_year != args(2).split("/")(0).toInt) {
        println("跨年，请分成两次运行")
        System.exit(-1)
      }

      val startMonth: Int = args(1).split("/")(1).toInt //包含
      val endMonth: Int = args(2).split("/")(1).toInt //不包含

      val peopleAttendance = new mutable.HashMap[String, Array[mutable.HashSet[Int]]]()

      val personSource = Source.fromFile(args(3),"UTF-8")
      val personIter = personSource.getLines()
      for(personLine <- personIter) {
        val name = personLine.split(",")(0)
        if(!peopleAttendance.contains(name)) {
          val personAttendance = new Array[mutable.HashSet[Int]](endMonth - startMonth)
          for(i <- 0 to endMonth - startMonth - 1) {
            personAttendance(i) = new mutable.HashSet[Int]()
          }
          peopleAttendance(name) = personAttendance
        }
      }
      personSource.close()

      val fileSource = Source.fromFile(args(3), "UTF-8")
      val lineIter = fileSource.getLines()

      for(line <- lineIter) {
        val name = line.split(",")(0)
        if(peopleAttendance.contains(name)) {
          val year: Int = line.split(",")(1).split(" ")(0).split("/")(0).toInt
          val month: Int = line.split(",")(1).split(" ")(0).split("/")(1).toInt
          val day: Int = line.split(",")(1).split(" ")(0).split("/")(2).toInt

          if(month >= startMonth && month < endMonth && year == s_year) {
            peopleAttendance(name)(month - startMonth) += day
          }
        }
      }
      fileSource.close()

      val out = new PrintWriter(args(4))
      out.println("每人每月打卡天数统计：\t")
      out.println("月份：\t有记录天数\t<具体有记录的日期>")
      for((name, attendance) <- peopleAttendance) {
        out.println(name+":\t")
        for(i <- attendance.indices) {
          out.println(s_year + "/" + (i + startMonth) + ":\t" +
            attendance(i).size + "\t" + attendance(i).mkString("<",",",">"))
        }
      }
      out.close()
    }
    else {
      val name: String = args(0)

      val s_year: Int = args(1).split("/")(0).toInt
      if(s_year != args(2).split("/")(0).toInt) {
        println("跨年，请分成两次运行")
        System.exit(-1)
      }

      val startMonth: Int = args(1).split("/")(1).toInt //包含
      val endMonth: Int = args(2).split("/")(1).toInt //不包含

      val attendance = new Array[mutable.HashSet[Int]](endMonth - startMonth)

      for(i <- 0 to endMonth - startMonth - 1) {
        attendance(i) = new mutable.HashSet[Int]()
      }

      val fileSource = Source.fromFile(args(3), "UTF-8")
      val lineIter = fileSource.getLines()

      for(line <- lineIter) {
        if(line.split(",")(0).compareTo(name) == 0) {
          val year: Int = line.split(",")(1).split(" ")(0).split("/")(0).toInt
          val month: Int = line.split(",")(1).split(" ")(0).split("/")(1).toInt
          val day: Int = line.split(",")(1).split(" ")(0).split("/")(2).toInt

          if(month >= startMonth && month < endMonth && year == s_year) {
            attendance(month - 1) += day
          }
        }
      }
      fileSource.close()

      println(name+":")
      for(i <- attendance.indices) {
        println(s_year + "/" + (i + 1 ) + ":\t" +
          attendance(i).size + "\t" + attendance(i).mkString("<",",",">"))
      }
    }
  }
}
