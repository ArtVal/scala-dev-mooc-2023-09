package module1.futures

import module1.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val p = Promise[(List[A], List[Throwable])]
    futures.foreach(_.onComplete { _ =>
      if(isAllComplete(futures)){
        val finalResult = futures.foldLeft((List.empty[A], List.empty[Throwable])) { case ((results: List[A], errors: List[Throwable]), elem: Future[A]) =>
          elem.value.map {
            case Success(result) => (results.appended(result), errors)
            case Failure(e) => (results, errors.appended(e))
          }.get
        }
        p.trySuccess(finalResult)
        ()
      }
    })

    def isAllComplete(futures: List[Future[A]]): Boolean = {
      futures.forall(_.isCompleted)
    }

    p.future
  }

}
