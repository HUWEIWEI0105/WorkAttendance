import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.io.Source
import java.util.Calendar

/**
 * Created by Won Andwon on 2016/4/6.
 * 处理考勤记录，获得方便看的版本
 */
object DealRecord {
  val MillisInOneDay: Long = 24 * 3600 * 1000 //一天总的毫秒数
  val DurationStatisticalUnit: Long = 30 * 60 * 1000 //统计单位 半小时毫秒数。
  val StartingDate: Long = {  //起始日期
    val cal: Calendar = Calendar.getInstance()
    cal.set(2016, 0, 1, 3, 0, 0) //每个人的统计起始点为2016年1(从0计算)月1日凌晨三点
    cal.getTimeInMillis
  }
  val DaysInEachMonth =
    Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) // 闰年记得自己2月多+1 比如2016年就是闰年

  def howManyDaysBeforeThisMonth(Month: String): Int = {
    var res: Int = 0
    val year = Month.split("/")(0).toInt
    if(year > 2016) //2017年及往后
      for(y <- 2016 to (year - 1)) {
        res += 365 + {if((y % 400 == 0) || (y % 4 == 0 && y % 100 != 0)) 1 else 0}
      }

    val month = Month.split("/")(1).toInt
    if(month == 1) res
    else {
      for(i <- 0 to (month - 2)) {
        res += DaysInEachMonth(i)
      }
      if(month > 2) {

        val leap = if((year % 400 == 0) || (year % 4 == 0 && year % 100 != 0)) 1 else 0
        res += leap
      }
      res
    }
  }

  /**
   * 主函数的咯
   * @param args
   *             (0) 统计谁的信息，ALL则为全体
   *             (1) 统计的起始月份，包含。务必为输入文件中有的且未统计的 例如 2016/1
   *             (2) 统计的终止月份，不包含。此月之前一月务必为输入文件中有的且未统计的
   *                例如参数 2016/4 则终止日期为2016-4-1 即统计到3月份（截止3-31）的数据
   *             (3) 统计的输入文件，请转换成csv格式的txt文件，务必去掉csv的首行！因为我不想为了确定的第一行还要加个判断
   *                 首行在这，都严格遵守格式：
   *                 姓名,出勤时间,出勤状态,更正状态,异常情况,操作
   *             (4) 统计的输出文件
   *             (5) 统计的人员考勤初始化资料，比如有效时长和有效次数，用以更新平均时长
   */
  def main(args: Array[String]) {
    if(args(0).compareTo("ALL") == 0) { //统计全体
      val minDay: Int = howManyDaysBeforeThisMonth(args(1)) //统计起始天数
      val maxDay: Int = howManyDaysBeforeThisMonth(args(2)) //统计终止天数
      val statisticDays: Int = maxDay - minDay //统计时长（天数）

      val personSource = Source.fromFile(args(3),"UTF-8")
      val personIter = personSource.getLines()

      val peopleRecord = new mutable.HashMap[String, PersonRecord]()

      for(personLine <- personIter) {
        val name = personLine.split(",")(0)
        if(!peopleRecord.contains(name)) {
          val personRecord = new PersonRecord(name, statisticDays)
          peopleRecord(name) = personRecord
        }
      }

      personSource.close()

      val dataFile: File = new File(args(5), "UTF-8") //读取过往资料
      if(dataFile.exists()) {
        val dataSource = Source.fromFile(dataFile)
        val dataline = dataSource.getLines()

        for (line <- dataline) {
          val name = line.split(" ")(0)
          if(peopleRecord.contains(name)) {
            if (line.split(" ")(1).split(":")(0).compareTo("有效总时长") == 0) {
              peopleRecord(name).effectiveTotalDuration += line.split(" ")(1).split(":")(1).toLong
            }
            if (line.split(" ")(1).split(":")(0).compareTo("有效总对数") == 0) {
              peopleRecord(name).effectivePairTimes += line.split(" ")(1).split(":")(1).toInt
            }
          }
        }

        dataSource.close()
      }

      val fileSource = Source.fromFile(args(3),"UTF-8")
      val lineIter = fileSource.getLines()

      var lastRationalEntryTime: Long = 0L
      var currentDay: Int = 0 //按天统计，当前天
      for(line <- lineIter) {
        val name = line.split(",")(0)
        if( peopleRecord.contains(name) &&
            line.split(",")(1).split("/")(0).toInt >= 2016  //统计2016年及以后的，2015年记录无效
        ){
          val year: Int = line.split(",")(1).split(" ")(0).split("/")(0).toInt
          val month: Int = line.split(",")(1).split(" ")(0).split("/")(1).toInt - 1
          val day: Int = line.split(",")(1).split(" ")(0).split("/")(2).toInt
          val hour: Int = line.split(",")(1).split(" ")(1).split(":")(0).toInt
          val minute: Int = line.split(",")(1).split(" ")(1).split(":")(1).toInt
          val second: Int = 0
          val cal: Calendar = Calendar.getInstance()
          cal.set(year, month, day, hour, minute, second)

          val whichDayInTheYear: Int = ((cal.getTimeInMillis - StartingDate) / MillisInOneDay + 1).toInt
          if(whichDayInTheYear > minDay && whichDayInTheYear <= maxDay) {
            val inOrOut: Int = { //上班记录标记为-1，下班则为1
              if(line.split(",")(2).compareTo("上班签到") == 0) -1
              else 1
            }
            val timeRecord: Long = inOrOut * cal.getTimeInMillis

            if(whichDayInTheYear == currentDay) { //判断是否变天了
              if(timeRecord < 0) { //遇到签到记录，如果前面的合法值为0（被处理了）或已有签到（前面的不合法） 覆盖
                if(lastRationalEntryTime <= 0) lastRationalEntryTime = timeRecord
                else {
                  println("Something Error!")
                  System.exit(-1)
                }
              }
              else if(timeRecord > 0){
                if(lastRationalEntryTime < 0) { //正常情况
                  peopleRecord(name).effectiveTotalDuration += (timeRecord + lastRationalEntryTime)
                  peopleRecord(name).effectivePairTimes += 1
                  lastRationalEntryTime = 0
                }
                else if(lastRationalEntryTime == 0) lastRationalEntryTime = 0 //先出现了签退记录
                else {
                  println("Something Error!")
                  System.exit(-1)
                }
              }
              else {
                println("Something Error!")
                System.exit(-1)
              }
            }
            else {  //变天了 重置
              currentDay = whichDayInTheYear
              if(timeRecord < 0)lastRationalEntryTime = timeRecord
              else lastRationalEntryTime = 0L
            }
          }
          else{
          }
        }
      }
      fileSource.close()

      for(each <- peopleRecord) each._2.setAverageEffectiveDuration()

      val out = new PrintWriter(args(5))
      out.println("截至" + args(2) + "的统计数据")
      out.println("用以补足不成对打卡时间的平均有效时长最多为6个小时！")
      for(each <- peopleRecord) {
        out.println(each._1 + " " + "有效总时长:" + each._2.effectiveTotalDuration+ " 毫秒（ms） " +
          each._2.effectiveTotalDuration * 1.0 / (3600 * 1000) + " 小时")
        out.println(each._1 + " " + "有效总对数:" + each._2.effectivePairTimes+ " 次（签到+签退）")
        out.println(each._1 + " " + "平均有效时长:" +
          each._2.averageEffectiveDuration * 1.0 / (3600 * 1000) + " 小时")
      }
      out.close()

      val fileSource1 = Source.fromFile(args(3),"UTF-8")
      val lineIter1 = fileSource1.getLines()

      var lastRationalTime: Long = 0L
      currentDay = 0 //按天统计，当前天
      var currentPerson: String = "None" //按人统计，当前人
      for(line <- lineIter1) {
        val name = line.split(",")(0)
        if(name.compareTo(currentPerson) != 0) { //换人了
          //对上一人最后一条记录做处理，如果为0或者正，均不需要处理，因为每个下班打卡都一定处理过了。
          if(lastRationalTime < 0) { //对最后一条记录是上班的处理，为那天添加时间
          val cutoffTime: Long = currentDay * MillisInOneDay + StartingDate - (3 * 3600 * 1000) //上天午夜
          val t_workHour = cutoffTime + lastRationalTime
            peopleRecord(currentPerson).personCounter(currentDay - 1 - minDay) += { //注意下标
              if(t_workHour > peopleRecord(currentPerson).averageEffectiveDuration)
                peopleRecord(currentPerson).averageEffectiveDuration
              else
                t_workHour}
          }
          lastRationalTime = 0L
          currentDay = 0 //按天统计，当前天
          currentPerson = name //按人统计，当前人
        }
        if( peopleRecord.contains(currentPerson) && //统计某人的
            line.split(",")(1).split("/")(0).toInt >= 2016  //统计2016年及以后的，2015年记录无效
        ){
        val year: Int = line.split(",")(1).split(" ")(0).split("/")(0).toInt
        val month: Int = line.split(",")(1).split(" ")(0).split("/")(1).toInt - 1
        val day: Int = line.split(",")(1).split(" ")(0).split("/")(2).toInt
        val hour: Int = line.split(",")(1).split(" ")(1).split(":")(0).toInt
        val minute: Int = line.split(",")(1).split(" ")(1).split(":")(1).toInt
        val second: Int = 0
        val cal: Calendar = Calendar.getInstance()
        cal.set(year, month, day, hour, minute, second)

        val whichDayInTheYear: Int = ((cal.getTimeInMillis - StartingDate) / MillisInOneDay + 1).toInt
        if(whichDayInTheYear > minDay && whichDayInTheYear <= maxDay) {
          val inOrOut: Int = { //上班记录标记为-1，下班则为1
            if(line.split(",")(2).compareTo("上班签到") == 0) -1
            else 1
          }
          val timeRecord: Long = inOrOut * cal.getTimeInMillis

          if(whichDayInTheYear == currentDay) { //同一天的数据的处理
            //如果上一个记录为下班，则一定处理过了，且确实处理了
            if(lastRationalTime < 0) { //上个记录为上班
              if(timeRecord < 0) { //此条记录也为上班
              val t_workHour = lastRationalTime - timeRecord //统计两条记录的差值
                peopleRecord(currentPerson).personCounter(whichDayInTheYear - 1 - minDay) += { //注意下标
                  if(t_workHour > peopleRecord(currentPerson).averageEffectiveDuration)
                    peopleRecord(currentPerson).averageEffectiveDuration
                  else
                    t_workHour}
              }
              else if(timeRecord > 0) { //此条记录为下班
                peopleRecord(currentPerson).personCounter(whichDayInTheYear - 1 - minDay) += (timeRecord + lastRationalTime)
              }
              else { //此条记录为奇怪0
                println("Something Error!")
                System.exit(-1)
              }
            }
            else if(lastRationalTime > 0){ //上条记录为下班
              if(timeRecord < 0) { //此条记录为上班

              }
              else if(timeRecord > 0) { //此条记录也为下班
              val t_workHour = timeRecord - lastRationalTime //统计两条记录的差值
                peopleRecord(currentPerson).personCounter(whichDayInTheYear - 1 - minDay) += { //注意下标
                  if(t_workHour > peopleRecord(currentPerson).averageEffectiveDuration)
                    peopleRecord(currentPerson).averageEffectiveDuration
                  else
                    t_workHour}
              }
              else { //此条记录为奇怪0
                println("Something Error!")
                System.exit(-1)
              }
            }
            else { //上条记录为奇怪0
              println("Something Error!")
              System.exit(-1)
            }
          }
          else {  //变天了 重置
            // 如果上一个记录为下班，则一定处理过了
            if(lastRationalTime > 0) { //上一天的最后一条记录是下班，正常

            }
            else if(lastRationalTime < 0) { //上一天的最后一条记录是上班，为上一天添加时间
            val cutoffTime: Long = currentDay * MillisInOneDay + StartingDate - (3 * 3600 * 1000) //上天午夜
            val t_workHour = cutoffTime + lastRationalTime
              peopleRecord(currentPerson).personCounter(currentDay - 1 - minDay) += { //注意下标
                if(t_workHour > peopleRecord(currentPerson).averageEffectiveDuration)
                  peopleRecord(currentPerson).averageEffectiveDuration
                else
                  t_workHour}
            }
            else { //此条记录为第一条记录

            }
            currentDay = whichDayInTheYear //换天统计
            if(timeRecord > 0){// 本天第一条是下班 非常
            val cutoffTime: Long = (currentDay - 1) * MillisInOneDay + StartingDate + (3 * 3600 * 1000) //早上6点
            val t_workHour = timeRecord - cutoffTime
              peopleRecord(currentPerson).personCounter(currentDay - 1 - minDay) += { //注意下标
                if(t_workHour > peopleRecord(currentPerson).averageEffectiveDuration)
                  peopleRecord(currentPerson).averageEffectiveDuration
                else
                  t_workHour}
            }
            else if(timeRecord < 0) { //本天第一条是上班 正常

            }
            else { //当前记录是奇怪0
              println("Something Error!")
              System.exit(-1)
            }
          }
          lastRationalTime = timeRecord
        }
      }

      }

      if(lastRationalTime < 0) { //对最后一条记录是上班的处理，为那天添加时间
      val cutoffTime: Long = currentDay * MillisInOneDay + StartingDate - (3 * 3600 * 1000) //上天午夜
      val t_workHour = cutoffTime + lastRationalTime
        peopleRecord(currentPerson).personCounter(currentDay - 1 - minDay) += { //注意下标
          if(t_workHour > peopleRecord(currentPerson).averageEffectiveDuration)
            peopleRecord(currentPerson).averageEffectiveDuration
          else
            t_workHour}
      }

      fileSource1.close()

      val outResult = new PrintWriter(args(4))
      outResult.println(args(1) + "/1（包含） ~ " + args(2) + "/1（未包含） 的统计汇总表")
      outResult.println("\t注意：有效总时间与有效总对数的统计为有记录以来截至" + args(2) + "/1（未包含） 的，" +
        "仅对合法打卡记录进行统计的数据，" +
        "详细表格中的时长则是对每条记录（无论合法不合法）进行统计的数据。所以你的每一次打卡都是有用的。")
      outResult.println("每日具体时长（单位: 小时）：")

      outResult.print("每月：\t")
      for(i <- 1 to 31) {
        outResult.print(i + "\t")
      }
      outResult.println()

      for((name, record) <- peopleRecord) {
        outResult.println("姓名:\t" + name + "\t" +
          "有效总时长:" + "\t\t" + record.effectiveTotalDuration+ " 毫秒（ms） " + "\t\t\t" +
          (record.effectiveTotalDuration * 1.0 / (3600 * 1000)).formatted("%6.2f") + " 小时\t\t" +
          "有效总对数:" + "\t\t" + record.effectivePairTimes+ " 次（签到+签退）" + "\t\t")

        var currentDay: Int = 0
        for(month <- args(1).split("/")(1).toInt //此处必须同年，跨年记得修改这里，出bug了不要怪我没提醒
            to (args(2).split("/")(1).toInt - 1)
            if currentDay < statisticDays) {
          val y = args(1).split("/")(0).toInt
          if(y != args(2).split("/")(0).toInt) {
            println("跨年失败，麻烦你拆成两次运行，不要跨年好伐。")
            System.exit(-1)
          }
          outResult.print(y + "年" + month + "月:\t")
          for(day <- 1 to {
            val leap = if((y % 400 == 0) || (y % 4 == 0 && y % 100 != 0)) 1 else 0
            if(month == 2) DaysInEachMonth(month - 1) + leap
            else DaysInEachMonth(month - 1)
          }) {
            outResult.print((record.personCounter(currentDay) * 1.0 / (3600 * 1000)).formatted("%4.2f") + "\t")
            currentDay += 1
          }
          outResult.println()
        }
      }
      outResult.close()
    }
    else { //统计某个人
      val minDay: Int = howManyDaysBeforeThisMonth(args(1)) //统计起始天数 不包含
      val maxDay: Int = howManyDaysBeforeThisMonth(args(2)) //统计终止天数 包含

      val personRecord = new PersonRecord(args(0), maxDay - minDay)

      val fileSource = Source.fromFile(args(3),"UTF-8")
      val lineIter = fileSource.getLines()

      val dataFile: File = new File(args(5)) //读取过往资料
      if(dataFile.exists()) {
        val dataSource = Source.fromFile(dataFile)
        val dataline = dataSource.getLines()

        for (line <- dataline) {
          if (line.split(" ")(0).compareTo(personRecord.name) == 0) {
            if (line.split(" ")(1).split(":")(0).compareTo("有效总时长") == 0) {
              personRecord.effectiveTotalDuration += line.split(" ")(1).split(":")(1).toLong
            }
            if (line.split(" ")(1).split(":")(0).compareTo("有效总对数") == 0) {
              personRecord.effectivePairTimes += line.split(" ")(1).split(":")(1).toInt
            }
          }
        }

        dataSource.close()
      }

      var lastRationalEntryTime: Long = 0L
      var currentDay: Int = 0 //按天统计，当前天
      for(line <- lineIter) {
        if( line.split(",")(0).compareTo(personRecord.name) == 0 && //统计某人的
          line.split(",")(1).split("/")(0).toInt >= 2016  //统计2016年及以后的，2015年记录无效
        ){
          val year: Int = line.split(",")(1).split(" ")(0).split("/")(0).toInt
          val month: Int = line.split(",")(1).split(" ")(0).split("/")(1).toInt - 1
          val day: Int = line.split(",")(1).split(" ")(0).split("/")(2).toInt
          val hour: Int = line.split(",")(1).split(" ")(1).split(":")(0).toInt
          val minute: Int = line.split(",")(1).split(" ")(1).split(":")(1).toInt
          val second: Int = 0
          val cal: Calendar = Calendar.getInstance()
          cal.set(year, month, day, hour, minute, second)

          val whichDayInTheYear: Int = ((cal.getTimeInMillis - StartingDate) / MillisInOneDay + 1).toInt
          if(whichDayInTheYear > minDay && whichDayInTheYear <= maxDay) {
            val inOrOut: Int = { //上班记录标记为-1，下班则为1
              if(line.split(",")(2).compareTo("上班签到") == 0) -1
              else 1
            }
            val timeRecord: Long = inOrOut * cal.getTimeInMillis

//            println(line + "\t" + whichDayInTheYear + "\t" + timeRecord)

            if(whichDayInTheYear == currentDay) { //判断是否变天了
              if(timeRecord < 0) { //遇到签到记录，如果前面的合法值为0（被处理了）或已有签到（前面的不合法） 覆盖
                if(lastRationalEntryTime <= 0) lastRationalEntryTime = timeRecord
                else {
                  println("Something Error!")
                  System.exit(-1)
                }
              }
              else if(timeRecord > 0){
                if(lastRationalEntryTime < 0) { //正常情况
                  personRecord.effectiveTotalDuration += (timeRecord + lastRationalEntryTime)
                  personRecord.effectivePairTimes += 1
                  lastRationalEntryTime = 0
                }
                else if(lastRationalEntryTime == 0) lastRationalEntryTime = 0 //先出现了签退记录
                else {
                  println("Something Error!")
                  System.exit(-1)
                }
              }
              else {
                println("Something Error!")
                System.exit(-1)
              }
            }
            else {  //变天了 重置
              currentDay = whichDayInTheYear
              if(timeRecord < 0)lastRationalEntryTime = timeRecord
              else lastRationalEntryTime = 0L
            }

//            println(
//              "已有" + personRecord.effectivePairTimes + "次合法记录对。" +
//                "总有效时间为：" + personRecord.effectiveTotalDuration * 1.0 / DurationStatisticalUnit + "个半小时")
          }
        }
      }

      personRecord.setAverageEffectiveDuration()

      val out = new PrintWriter(args(5))
      out.println(args(0) + " " + "有效总时长:" + personRecord.effectiveTotalDuration)
      out.println(args(0) + " " + "有效总对数:" + personRecord.effectivePairTimes)
      out.close()

//      println(personRecord.averageEffectiveDuration * 1.0 / (3600 * 1000))

      val personCounter = new Array[Long](maxDay - minDay)
//      for(i <- 0 to (maxDay - minDay - 1)) { //初始化计数器 比如 1到91天的 存储在对应 0到90里
//        personCounter += 0L
//      }

      fileSource.close()

      val fileSource1 = Source.fromFile(args(3),"UTF-8")
      val lineIter1 = fileSource1.getLines()

      var lastRationalTime: Long = 0L
      currentDay = 0 //按天统计，当前天
      for(line <- lineIter1) {
        if( line.split(",")(0).compareTo(personRecord.name) == 0 && //统计某人的
          line.split(",")(1).split("/")(0).toInt >= 2016  //统计2016年及以后的，2015年记录无效
        ){
          val year: Int = line.split(",")(1).split(" ")(0).split("/")(0).toInt
          val month: Int = line.split(",")(1).split(" ")(0).split("/")(1).toInt - 1
          val day: Int = line.split(",")(1).split(" ")(0).split("/")(2).toInt
          val hour: Int = line.split(",")(1).split(" ")(1).split(":")(0).toInt
          val minute: Int = line.split(",")(1).split(" ")(1).split(":")(1).toInt
          val second: Int = 0
          val cal: Calendar = Calendar.getInstance()
          cal.set(year, month, day, hour, minute, second)

          val whichDayInTheYear: Int = ((cal.getTimeInMillis - StartingDate) / MillisInOneDay + 1).toInt
          if(whichDayInTheYear > minDay && whichDayInTheYear <= maxDay) {
            val inOrOut: Int = { //上班记录标记为-1，下班则为1
              if(line.split(",")(2).compareTo("上班签到") == 0) -1
              else 1
            }
            val timeRecord: Long = inOrOut * cal.getTimeInMillis

            if(whichDayInTheYear == currentDay) { //同一天的数据的处理
            //如果上一个记录为下班，则一定处理过了，且确实处理了
              if(lastRationalTime < 0) { //上个记录为上班
                if(timeRecord < 0) { //此条记录也为上班
                  val t_workHour = lastRationalTime - timeRecord //统计两条记录的差值
                  personCounter(whichDayInTheYear-1) += { //注意下标
                    if(t_workHour > personRecord.averageEffectiveDuration)
                      personRecord.averageEffectiveDuration
                    else
                      t_workHour}
                }
                else if(timeRecord > 0) { //此条记录为下班
                  personCounter(whichDayInTheYear-1) += (timeRecord + lastRationalTime)
                }
                else { //此条记录为奇怪0
                  println("Something Error!")
                  System.exit(-1)
                }
              }
              else if(lastRationalTime > 0){ //上条记录为下班
                if(timeRecord < 0) { //此条记录为上班

                }
                else if(timeRecord > 0) { //此条记录也为下班
                  val t_workHour = timeRecord - lastRationalTime //统计两条记录的差值
                  personCounter(whichDayInTheYear-1) += { //注意下标
                    if(t_workHour > personRecord.averageEffectiveDuration)
                      personRecord.averageEffectiveDuration
                    else
                      t_workHour}
                }
                else { //此条记录为奇怪0
                  println("Something Error!")
                  System.exit(-1)
                }
              }
              else { //上条记录为奇怪0
                println("Something Error!")
                System.exit(-1)
              }
            }
            else {  //变天了 重置
            // 如果上一个记录为下班，则一定处理过了
              if(lastRationalTime > 0) { //上一天的最后一条记录是下班，正常

              }
              else if(lastRationalTime < 0) { //上一天的最后一条记录是上班，为上一天添加时间
                val cutoffTime: Long = currentDay * MillisInOneDay + StartingDate - (3 * 3600 * 1000) //上天午夜
                val t_workHour = cutoffTime + lastRationalTime
                personCounter(currentDay - 1) += { //注意下标
                  if(t_workHour > personRecord.averageEffectiveDuration)
                    personRecord.averageEffectiveDuration
                  else
                    t_workHour}
              }
              else { //此条记录为第一条记录

              }
              currentDay = whichDayInTheYear //换天统计
              if(timeRecord > 0){// 本天第一条是下班 非常
                val cutoffTime: Long = (currentDay - 1) * MillisInOneDay + StartingDate + (3 * 3600 * 1000) //早上6点
                val t_workHour = timeRecord - cutoffTime
                personCounter(currentDay - 1) += { //注意下标
                  if(t_workHour > personRecord.averageEffectiveDuration)
                    personRecord.averageEffectiveDuration
                  else
                    t_workHour}
              }
              else if(timeRecord < 0) { //本天第一条是上班 正常

              }
              else { //当前记录是奇怪0
                println("Something Error!")
                System.exit(-1)
              }
            }
            lastRationalTime = timeRecord
          }
        }
      }

      if(lastRationalTime < 0) { //对最后一条记录是上班的处理，为那天添加时间
      val cutoffTime: Long = currentDay * MillisInOneDay + StartingDate - (3 * 3600 * 1000) //上天午夜
      val t_workHour = cutoffTime + lastRationalTime
        personCounter(currentDay - 1) += { //注意下标
          if(t_workHour > personRecord.averageEffectiveDuration)
            personRecord.averageEffectiveDuration
          else
            t_workHour}
      }

      fileSource1.close()

      for(i <- personCounter.indices){
        println("第"+(i+1)+"天："+personCounter(i)*1.0/(3600*1000))
      }
    }
  }
}
class PersonRecord (val name: String, val days: Int) {
  //处理非法输入时，最大有效打卡间隔（补齐一次进入和一次出去）
  //避免就合法一次的，比如18个小时，这样每次只打一次就默认18小时计算了，显然不科学。
  //（不合法会依据平均表现补齐，让你的每次打卡都有价值）
  val maxAverageEffectiveDuration: Long = 6 * 3600 * 1000
  var effectiveTotalDuration: Long = 0L //有效情况下的总时间
  var effectivePairTimes: Int = 0      //有效总对数 一次（上班 + 下班）
  var averageEffectiveDuration: Long = 0L //处理非法输入时，依据你的平时打卡间隔补齐。最大限制看上面那个max参数。
//  val monthRecord = new ArrayBuffer[MonthRecord]()

  val personCounter = new Array[Long](days)

  def setAverageEffectiveDuration(): Unit = {
    val temp_average: Long =
      if(this.effectivePairTimes == 0) 0L
      else this.effectiveTotalDuration / this.effectivePairTimes
    if(temp_average > maxAverageEffectiveDuration)
      this.averageEffectiveDuration = this.maxAverageEffectiveDuration
    else this.averageEffectiveDuration = temp_average
  }
}
//class DayRecord() {
//  var workingHours: Long = 0L
//}
//class MonthRecord (val month: String) { //例如201602
//  val year = month.substring(0, 4).toInt
//  val days: Int = {
//  month.substring(4, 6).toInt match {
//    case 1 => 31
//    case 2 => {
//      if((year % 400 == 0) || (year % 4 == 0 && year % 100 != 0)) 29
//      else 28
//    }
//    case 3 => 31
//    case 4 => 30
//    case 5 => 31
//    case 6 => 30
//    case 7 => 31
//    case 8 => 31
//    case 9 => 30
//    case 10 => 31
//    case 11 => 30
//    case 12 => 31
//    case _ => {
//      println("Wrong Month!");0
//    }
//  }}
//  val datRecords = new Array[DayRecord](days)
//}
//class DayRecord (val Date: String) {
//  val inRecord = new ArrayBuffer[String]()
//  val outRecord = new ArrayBuffer[String]()
//}
