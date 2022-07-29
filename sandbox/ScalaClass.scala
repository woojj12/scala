import uk.ac.manchester.tornado.api.TaskSchedule
import uk.ac.manchester.tornado.api.annotations.Parallel
import uk.ac.manchester.tornado.api.annotations.Reduce
import java.util.Random

object ScalaClass {
  def calcMarginScala(i: Array[Int], data: Array[Double], weightsArray: Array[Double], @Reduce margin: Array[Double]): Unit = {
    margin(0) = 0
    @Parallel var index: Int = 0
    while(index < data.length) {
      margin(0) += data(index) * weightsArray(i(0) + index)
      index += 1
    }
  }
  def createAndRunTaskScala(funccall: Long, numClasses: Int, data: Array[Double], weightsArray: Array[Double]): Array[Double] = {
    println("createAndRunTaskScala")
    val funcin = System.nanoTime
    val stTaskSchedule = new TaskSchedule("s0")

    val iArray = new Array[Int](1)
    val margin = new Array[Double](1)

    val clazz = ScalaClass.getClass
    val mirror = scala.reflect.runtime.universe.runtimeMirror(clazz.getClassLoader)

    val sym = mirror.classSymbol(clazz)
    println(sym)

    stTaskSchedule.task("t0", ScalaClass.calcMarginScala, iArray, data, weightsArray, margin)
    stTaskSchedule.streamIn(iArray);
    stTaskSchedule.streamOut(margin);

    var margins = new Array[Double](10)

    for (i <- 0 until (numClasses - 1))
    {
        margin(0) = 0;
        iArray(0) = i * data.length;
        stTaskSchedule.execute();
        printf("%d %f\n", i, margin(0))
        margins(i) = margin(0);
    }

    margins
  }
  def doSomething(): Unit = {
    //i: Int, data: Array[Double], weightsArray: Array[Double], @Reduce result: Array[Double]

    val random = new Random();
    val numClasses = 10

    //val datasize = 10000000
    val datasize = 1000
    val weightsarraysize = (numClasses -1) * datasize

    for (i <- 0 until 5)
    {

      //val data = new Array[Double](datasize)
      //val weightsArray = new Array[Double](weightsarraysize)

      /*
      for (i <- 0 until datasize) 
      {
        data(i) = 1
      }

      for (i <- 0 until weightsarraysize)
      {
        weightsArray(i) = scala.math.pow(2,i)
      }
      */

      
      
      val dataList = random.doubles(datasize)
      val data = dataList.toArray()
      val weightsArray = random.doubles(weightsarraysize).toArray()


      val cpustart = System.nanoTime()
      val marginsCpu = Array.tabulate(numClasses - 1) { i =>
        var margin = 0.0
        for (index <-0 until datasize) {
          if (data(index) != 0.0) {
            margin += data(index) * weightsArray((i * datasize) + index)
          }
        }
        margin
      }
      val cputime = System.nanoTime() - cpustart

      var margins = new Array[Double](numClasses - 1)
      val tornadostart = System.nanoTime()

      //margins = WrapperClass.createAndRunTask4(tornadostart, numClasses,data,weightsArray)
      margins = createAndRunTaskScala(tornadostart, numClasses,data,weightsArray)

      val tornadotime = System.nanoTime() - tornadostart

      println("" + tornadotime + " " + cputime)
      
      for ( i <- 0 to (marginsCpu.length - 1) ) 
      {
        val diff = marginsCpu(i) - margins(i)
        if ((diff > 0.1) || (diff < -0.1))
        {
          printf("diff!! %d %f %f\n", i, marginsCpu(i), margins(i))
          // printf("diff!! %d %s %s\n", i, marginsCpu(i).toInt.toBinaryString, margins(i).toInt.toBinaryString)
        }
      }
    }

  }

  def _main(): Unit = {
    doSomething()
  }
}
