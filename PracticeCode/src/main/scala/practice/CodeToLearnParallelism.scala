package practice

import java.util.concurrent.{Callable, ExecutorService, Executors}

import scala.concurrent.duration.Duration

object CodeToLearnParallelism extends App {
  import scala.concurrent._

  private implicit val ec = new ExecutionContext {
    val threadPool: ExecutorService = Executors.newFixedThreadPool(1000)
    def execute(runnable: Runnable) = {
      threadPool
        .submit(new Callable[Any] {
          def call = runnable.run()
        })

    }
    def reportFailure(t: Throwable) { throw t }
  }

  val f = Future {
    println("from future")
  }

  f.onComplete { x =>
    println("I'm done. " + x)
  }(scala.concurrent.ExecutionContext.Implicits.global)

  Await.result(f, Duration.Inf)

}
