package pariprasna

import io.circe._
import org.http4s._

import scalaz._, Scalaz._

trait OAuthTypes {

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

  case class AuthorizationRequestRedirect(
    uri: Uri
  )

  case class AuthorizationResponse(
    code: AuthorizationCode,
    state: Option[String]
  )

  case class TokenResponse(
    idToken: String,
    accessToken: AccessToken,
    refreshToken: Option[String]
  )

  case class AccessToken(
    value: String,
    expiresIn: Long,
    tokenType: String
  )


  sealed trait OAuthError extends Throwable

  object OAuthError {

    case object AuthResponseLocationHeaderMissing extends OAuthError

    case object AuthRequestCodeMissing extends OAuthError

    case class TokenResponseError(code: String, msg: String) extends OAuthError

    case class AuthorizationError(code: String, msg: Option[String]) extends OAuthError

    case class ParseError(msg: String) extends OAuthError

  }

}

trait OAuthInstances extends OAuthTypes {

  implicit val tokenResponseJsonDecoder: Decoder[TokenResponse] = Decoder.instance[TokenResponse] { json =>

    json.downField("error").focus.cata(
      _ => cats.data.Xor.Left(DecodingFailure("error response", json.history)),
      {

        for {
          accessToken <- json.downField("access_token").as[String]
          idToken <- json.downField("id_token").as[String]
          expiresIn <- json.downField("expires_in").as[Long]
          tokenType <- json.downField("token_type").as[String]
          refreshToken <- json.downField("refresh_token").as[Option[String]]
        } yield {
          TokenResponse(idToken, AccessToken(accessToken, expiresIn, tokenType), refreshToken)
        }

      }
    )

  }

  implicit val tokenResponseErrorJsonDecoder = facebookErrorJsonDecoder.or(googleErrorJsonDecoder)

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



  // entity decoder

  implicit val tokenResponseEntityDecoder: EntityDecoder[TokenResponse] = tokenResponseJsonEntityDecoder.orElse(tokenResponseFormEntityDecoder)

  def tokenResponseJsonEntityDecoder: EntityDecoder[TokenResponse] = circeJsonEntityDecoder.flatMapR {
    json =>
      json.as[TokenResponse].fold(
        failure => {
          json.as[OAuthError.TokenResponseError].fold(
            msg => DecodeResult.failure(MalformedMessageBodyFailure("error decoding token response", Some(OAuthError.ParseError(msg.message)))),
            tokenResponseError => DecodeResult.failure(MalformedMessageBodyFailure("token response error", Some(tokenResponseError)))
          )
        },
        token => DecodeResult.success(token)
      )
  }

  def circeDecoderAsEntityDecoder[A:Decoder]: EntityDecoder[A] = {
    circeJsonEntityDecoder.flatMapR(json => Decoder[A].decodeJson(json).fold(
      _ => org.http4s.DecodeResult.failure[A](MalformedMessageBodyFailure("bad request")),
      a => org.http4s.DecodeResult.success[A](a)
    ))
  }

  def circeJsonEntityDecoder: EntityDecoder[Json] =
    EntityDecoder.decodeBy(MediaRange.`*/*`) { msg =>
      EntityDecoder.collectBinary(msg).map(bs => new String(bs.toArray, msg.charset.getOrElse(Charset.`UTF-8`).nioCharset)).flatMap { txt =>
        io.circe.parser.parse(txt).fold(
          _ => org.http4s.DecodeResult.failure[Json](MalformedMessageBodyFailure("bad request")),
          a => org.http4s.DecodeResult.success[Json](a)
        )
      }
    }

  // facebook api returns response as application/x-www-form-urlencoded
  // access_token=...&expires=...
  def tokenResponseFormEntityDecoder: EntityDecoder[TokenResponse] = UrlForm.entityDecoder.flatMapR {
    form =>
      form.getFirst("access_token").cata(
        token => DecodeResult.success(TokenResponse("", AccessToken(token, 0, ""), None)),
        DecodeResult.failure(MalformedMessageBodyFailure("error decoding token response", Some(OAuthError.ParseError("access token"))))
      )
  }


}

