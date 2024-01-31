package catsconcurrency.cats_effect_homework

import cats.effect.Sync
import cats.implicits._
import Wallet.{WalletError, _}

import java.nio.file._
import scala.util.Try

// DSL управления электронным кошельком
trait Wallet[F[_]] {
  // возвращает текущий баланс
  def balance: F[BigDecimal]
  // пополняет баланс на указанную сумму
  def topup(amount: BigDecimal): F[Unit]
  // списывает указанную сумму с баланса (ошибка если средств недостаточно)
  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]]
}

// Игрушечный кошелек который сохраняет свой баланс в файл
// todo: реализовать используя java.nio.file._
// Насчёт безопасного конкуррентного доступа и производительности не заморачиваемся, делаем максимально простую рабочую имплементацию. (Подсказка - можно читать и сохранять файл на каждую операцию).
// Важно аккуратно и правильно завернуть в IO все возможные побочные эффекты.
//
// функции которые пригодятся:
// - java.nio.file.Files.write
// - java.nio.file.Files.readString
// - java.nio.file.Files.exists
// - java.nio.file.Paths.get
final class FileWallet[F[_]: Sync](id: WalletId) extends Wallet[F] {

  def balance: F[BigDecimal] = Sync[F].delay {
      if (Files.exists(filePath(id))) {
        BigDecimal(Files.readString(filePath(id)))
      } else {
        BigDecimal(0)
      }
    }

  def topup(amount: BigDecimal): F[Unit] =
    for {
      currentValue <- balance
      result <- Sync[F].delay(writeToFile(filePath(id), currentValue + amount))
    } yield result

  def withdraw(amount: BigDecimal): F[Either[WalletError, Unit]] = for {
    currentValue <- balance
    finalValue = currentValue - amount
    _ <- Sync[F].delay(writeToFile(filePath(id),finalValue))
  } yield {
    if(finalValue < 0) Either.left(BalanceTooLow) else Either.right()
  }

  private def writeToFile(filePath: Path, value: BigDecimal): Unit = {
    Files.write(filePath, value.toString.getBytes)
    ()
  }

  private def filePath(id: String): Path = Paths.get(s"wallet${id}.txt")

}

object Wallet {

  // todo: реализовать конструктор
  // внимание на сигнатуру результата - инициализация кошелька имеет сайд-эффекты
  // Здесь нужно использовать обобщенную версию уже пройденного вами метода IO.delay,
  // вызывается она так: Sync[F].delay(...)
  // Тайпкласс Sync из cats-effect описывает возможность заворачивания сайд-эффектов
  def fileWallet[F[_]: Sync](id: WalletId): F[Wallet[F]] = Sync[F].delay(new FileWallet[F](id))

  type WalletId = String

  sealed trait WalletError
  case object BalanceTooLow extends WalletError
}
