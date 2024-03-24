package module3

import module3.zio_homework.config.AppConfig
import module3.zio_homework.timed.ZioHomework
import zio.{Has, Task, UIO, ULayer, URIO, URLayer, ZIO, ZLayer}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram = {
    val number = nextIntBetween(1,4)
    for {
      _ <- putStrLn("Guess the number from 1 to 3")
      guess <- getStrLn.flatMap(guess => ZIO.effect(guess.toInt))
      num <- number
      _ <- if (guess == num) putStr("you guessed it!") else putStr("you guessed wrong!")
    } yield ()
  }

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile(condition: => Boolean)(body: => Unit): Task[Unit] = ZIO.effect(body).repeatWhile(_ => !condition)


  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault = for{
    config <- config.load.orElse(ZIO.succeed(AppConfig("localhost", "8080")))
    _ <- putStrLn(s"$config")
  } yield ()


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = for {
    number <- nextIntBetween(0,11)
    _ <- sleep(1.second)
  } yield number

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Seq[ZIO[Clock with Random, Nothing, Int]] = {
    Seq.fill(10)(eff)
  }


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = zioConcurrency.printEffectRunningTime{
    for {
      sum <- ZIO.mergeAll(effects)(0){(a,b) => a + b}
      _ <- putStrLn(s"sum is: $sum")
    } yield ()
  }


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = zioConcurrency.printEffectRunningTime {
    for {
      sum <- ZIO.mergeAllParN(10)(effects)(0) { (a, b) => a + b }
      _ <- putStrLn(s"sum is: $sum")
    } yield ()
  }


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */


  object timed {
    type ZioHomework = Has[ZioHomework.Service]
    @accessible
    object ZioHomework {

      trait Service {
        def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A]
      }

      object Service {
        def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A] =
          zioConcurrency.printEffectRunningTime(zio)

        val live: Service = new Service {
          override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with Console, E, A] =
            Service.printEffectRunningTime(zio)
        }
      }

      val live: ULayer[Has[Service]] = ZLayer.succeed(Service.live)
    }
    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with ZioHomework with Clock with Console, E, A] =
      ZIO.accessM(_.get printEffectRunningTime zio)

  }

  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg: URIO[Console with Clock with Random with ZioHomework, Unit] = timed.printEffectRunningTime{
    app*>appSpeedUp
  }

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp = appWithTimeLogg.provideSomeLayer[Clock with Random with Console](timed.ZioHomework.live)

}
