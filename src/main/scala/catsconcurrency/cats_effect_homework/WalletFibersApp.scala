package catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._
import scala.concurrent.duration.{DurationInt, FiniteDuration}

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      fiber1 <- walletTopUp(wallet1, 100.millis, 100).start
      fiber2 <- walletTopUp(wallet2, 500.millis, 100).start
      fiber3 <- walletTopUp(wallet3, 2000.millis, 100).start
      fiber4 <- IO.sleep(1.second)
        .flatMap(_ => showWallet("1", wallet1) *> showWallet("2", wallet2) *> showWallet("3", wallet3)).foreverM.start
      _ <- IO.readLine
      _ <- fiber1.cancel *> fiber2.cancel *> fiber3.cancel *> fiber4.cancel
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
    } yield ()

  def walletTopUp(wallet: Wallet[IO] ,interval: FiniteDuration, amount: BigDecimal): IO[Unit] = {
    IO.sleep(interval).flatMap{_ =>
      wallet.topup(amount)
    }.foreverM
  }

  def showWallet(name: String, wallet: Wallet[IO]): IO[Unit] = {
    for {
      value <- wallet.balance
          _ <- IO.println(s"wallet${name} balance is $value")
    } yield ()
  }
}
