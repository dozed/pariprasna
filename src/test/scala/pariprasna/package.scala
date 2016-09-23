import org.specs2.execute.FailureException

import scalaz._
import Scalaz._

package object pariprasna {


  implicit class XorExt[E, A](e: cats.data.Xor[E, A]) {
    def require: A = e.fold(err => { println(err); throw new FailureException(org.specs2.execute.Failure("require")) }, identity)
  }


}
