package pariprasna

import iaksmlka._
import io.circe._
import org.http4s._

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

import Util._

case class OAuthCredentials(
  clientId: String,
  clientSecret: String)

case class OAuthEndpoint(
  key: String,
  scopes: List[String],
  authorizationUri: Uri,
  tokenUri: Uri
)

object OAuthAttribute {

  val ClientId = AttributeKey[String]("client_id")
  val ClientSecret = AttributeKey[String]("client_secret")
  val RedirectUri = AttributeKey[String]("redirect_uri")
  val GrantType = AttributeKey[String]("grant_type")
  val ResponseType = AttributeKey[String]("response_type")

  val Code = AttributeKey[String]("code")
  val Scope = AttributeKey[String]("scope")
  val Error = AttributeKey[String]("error")
  val Description = AttributeKey[String]("description")
  val State = AttributeKey[String]("state")

}


case class AuthorizationCode(value: String)

case class AccessToken(value: String)

case class AuthorizationRequestRedirect(
  uri: Uri
)

// https://tools.ietf.org/html/rfc6749#section-4.1.2
case class AuthorizationResponse(
  code: AuthorizationCode,
  state: Option[String]
)

// https://tools.ietf.org/html/rfc6749#section-4.1.4
// https://tools.ietf.org/html/rfc6749#section-5.1
case class TokenResponse(
  accessToken: AccessToken,
  tokenType: String,
  expiresIn: Long,
  refreshToken: Option[String],
  idToken: Option[Jwt]
)

object TokenResponse {


  type StringDecoder = String => DecodeResult[TokenResponse]


  val tokenResponseJsonDecoder: Decoder[TokenResponse] = Decoder.instance[TokenResponse] { json =>

    json.downField("error").focus.cata(
      _ => cats.data.Xor.Left(DecodingFailure("error response", json.history)),
      {

        for {
          accessToken <- json.downField("access_token").as[String]
          tokenType <- json.downField("token_type").as[String]
          expiresIn <- json.downField("expires_in").as[Long]
          refreshToken <- json.downField("refresh_token").as[Option[String]]
          idToken <- json.downField("id_token").as[Option[Jwt]]
        } yield {
          TokenResponse(AccessToken(accessToken), tokenType, expiresIn, refreshToken, idToken)
        }

      }
    )

  }

  implicit lazy val tokenResponseEntityDecoder: EntityDecoder[TokenResponse] =
    EntityDecoder.decodeBy(MediaRange.`*/*`) { msg =>
      tokenResponseJsonEntityDecoder.decode(msg, false).orElse(tokenResponseFormEntityDecoder.decode(msg, false))
    }

  def tokenResponseJsonEntityDecoder: EntityDecoder[TokenResponse] = Util.circeJsonEntityDecoder.flatMapR {
    json =>
      tokenResponseJsonDecoder.decodeJson(json).fold(
        failure => {
          json.as[OAuthError.TokenResponseError].fold(
            _ => DecodeResult.failure(MalformedMessageBodyFailure("error decoding token response", Some(OAuthError.ParseError(failure.getMessage())))),
            tokenResponseError => DecodeResult.failure(MalformedMessageBodyFailure("token response error", Some(tokenResponseError)))
          )
        },
        token => DecodeResult.success(token)
      )
  }

  // facebook api returns response as application/x-www-form-urlencoded
  // access_token=...&expires=...
  def tokenResponseFormEntityDecoder: EntityDecoder[TokenResponse] = UrlForm.entityDecoder.flatMapR {
    form =>
      form.getFirst("access_token").cata(
        token => DecodeResult.success(TokenResponse(AccessToken(token), "", 0, None, None)),
        DecodeResult.failure(MalformedMessageBodyFailure("error decoding token response", Some(OAuthError.ParseError("access token"))))
      )
  }

}



sealed trait OAuthError extends Throwable

object OAuthError {

  case object AuthResponseLocationHeaderMissing extends OAuthError

  case object AuthRequestCodeMissing extends OAuthError

  case class TokenResponseError(code: String, msg: String) extends OAuthError

  case class AuthorizationError(code: String, msg: Option[String]) extends OAuthError

  case class ParseError(msg: String) extends OAuthError


  object TokenResponseError {

    implicit val tokenResponseErrorJsonDecoder: Decoder[TokenResponseError] = facebookErrorJsonDecoder.or(googleErrorJsonDecoder)

    def facebookErrorJsonDecoder: Decoder[OAuthError.TokenResponseError] = Decoder.instance[OAuthError.TokenResponseError] { json =>
      for {
        code <- json.downField("error").downField("code").as[String]
        msg <- json.downField("error").downField("message").as[String]
      } yield {
        OAuthError.TokenResponseError(code, msg)
      }
    }

    def googleErrorJsonDecoder: Decoder[OAuthError.TokenResponseError] = Decoder.instance[OAuthError.TokenResponseError] { json =>
      for {
        code <- json.downField("error").as[String]
        msg <- json.downField("error_description").as[String]
      } yield {
        OAuthError.TokenResponseError(code, msg)
      }
    }


  }

}

