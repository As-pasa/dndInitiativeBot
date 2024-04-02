import sttp.capabilities
import sttp.client3.{HttpClientFutureBackend, Request, Response, SttpBackend}
import sttp.monad.MonadError

import scala.concurrent.Future

object CustomSttpBackend {
  def apply(): SttpBackend[Future, Nothing] = {
    new SttpBackend[Future,Nothing] {
      private val delegate:SttpBackend[Future,Nothing]=HttpClientFutureBackend().asInstanceOf[SttpBackend[Future,Nothing]]
      override def send[T, R >: Nothing with capabilities.Effect[Future]](request: Request[T, R]): Future[Response[T]] = {
        delegate.send(request)
      }

      override def close(): Future[Unit] = delegate.close()

      override def responseMonad: MonadError[Future] = delegate.responseMonad
    }

  }
}
