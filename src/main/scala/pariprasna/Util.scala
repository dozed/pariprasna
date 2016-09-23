package pariprasna

import iaksmlka._
import io.circe._
import org.http4s._
import pariprasna.OAuthError.TokenResponseError

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

object Util {

  // entity decoder

  def circeDecoderAsEntityDecoder[A:Decoder]: EntityDecoder[A] = {
    circeJsonEntityDecoder.flatMapR { json =>
      Decoder[A].decodeJson(json).fold(
        msg => org.http4s.DecodeResult.failure[A](MalformedMessageBodyFailure(msg.getMessage())),
        a => org.http4s.DecodeResult.success[A](a)
      )
    }
  }

  def circeJsonEntityDecoder: EntityDecoder[Json] =
    EntityDecoder.decodeBy(MediaType.`application/json`) { msg =>
      EntityDecoder.collectBinary(msg).map(bs => new String(bs.toArray, msg.charset.getOrElse(Charset.`UTF-8`).nioCharset)).flatMap { txt =>
        io.circe.parser.parse(txt).fold(
          failure => org.http4s.DecodeResult.failure[Json](MalformedMessageBodyFailure(failure.getMessage())),
          json => org.http4s.DecodeResult.success[Json](json)
        )
      }
    }


}

