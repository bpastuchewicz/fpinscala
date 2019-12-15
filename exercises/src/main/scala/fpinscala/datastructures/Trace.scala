import com.github.johnreedlol.logging.TraceLogging
/**
  * Created by johnreed on 3/23/16. Run with sbt test:run
  */
object Main extends TraceLogging {

  val x = List(1,2,3,4,5,6,7,8,9,0)
  /**
    * To prevent output mangling
    */
  def sleep(): Unit = {
    val milliseconds = 60L
    Thread.sleep(milliseconds)
  }

import com.github.johnreedlol.implicits.Pos
@SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
def doLogging(message: String)(implicit position: Pos): Unit = {
  println(message + position.text)
}

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def main(args: Array[String]): Unit = {
    // This tests the logging functionality:
    trace("Test0")
    debug("Test1")
    info("Test2")
    warn("Test3")
    warn(null)
    sleep()
    error("Test4")
    error(null)
    sleep()
    import com.github.johnreedlol._
    doLogging("Foo bar") // Foo bar at my.pkg.Main.main(Main.scala:56)
    out(5)
    out("Hello")
    sleep()
    err("World")
    err(6)
    sleep()
    println("This will contain a compiler generated stack trace" + pos())
    println("This line will not contain a compiler generated stack trace.")
    val one = 1
    val two = 2
    codeOut(one + two)
    sleep()
    val three = 3
    val four = 4
    codeErr(three * four)
    check(three != four)
    checkWithMessage(three != four, "Three must not equal four")
    // This should generate a stack trace.
    check(three == four)
    checkWithMessage(three == four, "Three must not equal four")
    sleep()
  }

  /**
    * pos must not throw a null pointer even if null is passed in.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def runNullSafetyTest(): Unit = {
    import com.github.johnreedlol._
    val nullVal: String = null
    out(nullVal)
    sleep()
    err(nullVal)
    sleep()
    out.apply(nullVal)
    sleep()
    err.apply(nullVal)
    sleep()
    println(nullVal)
    sleep()
    codeOut.apply(nullVal)
    sleep()
    codeErr.apply(nullVal)
    sleep()
  }
}
