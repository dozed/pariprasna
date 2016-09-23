package pariprasna

import io.circe._
import org.http4s._
import scalaz._, Scalaz._

object UserInfoEndpoint {

  case class UserInfo(
    id: String,
    email: String,
    verifiedEmail: Boolean,
    name: String,
    givenName: String,
    familyName: String,
    profile: String,
    picture: String,
    gender: String
  )

  // Google doesn't always send gender and profile
  case class BasicUserInfo(
    id: String,
    email: String,
    verifiedEmail: Boolean,
    name: String,
    givenName: String,
    familyName: String,
    picture: String
  )

  implicit val userInfoEq = Equal.equalA[UserInfo]
  implicit val userInfoShow = Show.showA[UserInfo]

  sealed trait UserInfoClaim

  object UserInfoClaim {

    case class Sub(value: String) extends UserInfoClaim
    case class Name(value: String) extends UserInfoClaim
    case class GivenName(value: String) extends UserInfoClaim
    case class FamilyName(value: String) extends UserInfoClaim
    case class MiddleName(value: String) extends UserInfoClaim
    case class Nickname(value: String) extends UserInfoClaim
    case class PreferredUsername(value: String) extends UserInfoClaim
    case class Profile(value: String) extends UserInfoClaim
    case class Picture(value: String) extends UserInfoClaim
    case class Website(value: String) extends UserInfoClaim
    case class Email(value: String) extends UserInfoClaim
    case class EmailVerified(value: Boolean) extends UserInfoClaim
    case class Gender(value: String) extends UserInfoClaim
    case class Birthdate(value: String) extends UserInfoClaim
    case class Zoneinfo(value: String) extends UserInfoClaim
    case class Locale(value: String) extends UserInfoClaim
    case class PhoneNumber(value: String) extends UserInfoClaim
    case class PhoneNumberVerified(value: Boolean) extends UserInfoClaim
    case class Address(value: Json) extends UserInfoClaim
    case class UpdatedAt(value: Long) extends UserInfoClaim

    case class Custom(key: String, value: Json) extends UserInfoClaim

  }

  val strBooleanDecoder: Decoder[Boolean] = Decoder[String].map(str => if (str === "true") true else false)

  implicit val userInfoClaimDecoder: Decoder[List[UserInfoClaim]] = Decoder[JsonObject].flatMap { obj =>
      Decoder.instance[List[UserInfoClaim]] { json =>

        import cats.std.list._
        import cats.syntax.traverse._

        val xs: List[Decoder.Result[UserInfoClaim]] = obj.toList.map {
          case ("sub", x) => x.as[String] map UserInfoClaim.Sub
          case ("name", x) => x.as[String] map UserInfoClaim.Name
          case ("given_name", x) => x.as[String] map UserInfoClaim.GivenName
          case ("family_name", x) => x.as[String] map UserInfoClaim.FamilyName
          case ("middle_name", x) => x.as[String] map UserInfoClaim.MiddleName
          case ("nickname", x) => x.as[String] map UserInfoClaim.Nickname
          case ("preferred_username", x) => x.as[String] map UserInfoClaim.PreferredUsername
          case ("profile", x) => x.as[String] map UserInfoClaim.Profile
          case ("picture", x) => x.as[String] map UserInfoClaim.Picture
          case ("website", x) => x.as[String] map UserInfoClaim.Website
          case ("email", x) => x.as[String] map UserInfoClaim.Email
          case ("email_verified", x) => x.as[Boolean](Decoder[Boolean].or(strBooleanDecoder)) map UserInfoClaim.EmailVerified
          case ("gender", x) => x.as[String] map UserInfoClaim.Gender
          case ("birthdate", x) => x.as[String] map UserInfoClaim.Birthdate
          case ("zoneinfo", x) => x.as[String] map UserInfoClaim.Zoneinfo
          case ("locale", x) => x.as[String] map UserInfoClaim.Locale
          case ("phone_number", x) => x.as[String] map UserInfoClaim.PhoneNumber
          case ("phone_number_verified", x) => x.as[Boolean](Decoder[Boolean].or(strBooleanDecoder)) map UserInfoClaim.PhoneNumberVerified
          case ("address", x) => cats.data.Xor.Right(UserInfoClaim.Address(x))
          case ("updated_at", x) => x.as[Long] map UserInfoClaim.UpdatedAt
          case (key, x) => cats.data.Xor.Right(UserInfoClaim.Custom(key, x))
        }

        xs.sequence

      }
    }


  // client.fetchAs[UserInfo](fetchUserInfo(.., ..))
  def fetchUserInfo(endpointUri: Uri, accessToken: AccessToken): Request = {
    Request(Method.GET, endpointUri).putHeaders(headers.Authorization(OAuth2BearerToken(accessToken.value)))
  }


  def lookupUserInfoDecoder(providerKey: String): Decoder[UserInfo] = providerKey match {
    case "facebook" => facebookUserInfoDecoder
    case "google" =>  openIdConnectUserInfoDecoder
    case "google+" =>  googlePlusUserInfoDecoder
  }

  def lookupUserInfoEndpoint(providerKey: String): Uri = providerKey match {
    case "facebook" => facebookUserInfoUri
    case "google" =>  googleUserInfoUri
    case "google+" =>  googlePlusGetOpenIdConnectUri
  }


  // UserInfo endpoints
  // http://openid.net/specs/openid-connect-core-1_0.html#UserInfo

  // Google: OpenID Connect
  // https://developers.google.com/identity/protocols/OpenIDConnect
  // https://accounts.google.com/.well-known/openid-configuration
  val googleUserInfoUri = Uri.uri("https://www.googleapis.com/oauth2/v3/userinfo")

  // Google+ API user profile in OpenID Connect-like format
  // https://developers.google.com/+/web/api/rest/openidconnect/getOpenIdConnect
  val googlePlusGetOpenIdConnectUri = Uri.uri("https://www.googleapis.com/plus/v1/people/me/openIdConnect")

  // Facebook: proprietary OAuth 2.0 authentication extension
  val facebookUserInfoUri = Uri.uri("https://graph.facebook.com/me")



  // OpenID Connect UserInfo format
  // http://openid.net/specs/openid-connect-core-1_0.html#StandardClaims
  val openIdConnectUserInfoDecoder = Decoder.instance[UserInfo] { cursor =>
    for {
      id <- cursor.downField("sub").as[String]
      email <- cursor.downField("email").as[String]
      verifiedEmail <- cursor.downField("email_verified").as[Boolean]
      name <- cursor.downField("name").as[String]
      givenName <- cursor.downField("given_name").as[String]
      familyName <- cursor.downField("family_name").as[String]
      profile <- cursor.downField("profile").as[String]
      picture <- cursor.downField("picture").as[String]
      gender <- cursor.downField("gender").as[String]
    } yield {
      UserInfo(id, email, verifiedEmail, name, givenName, familyName, profile, picture, gender)
    }
  }

  // OpenID Connect-like
  // email_verified is a string
  val googlePlusUserInfoDecoder = Decoder.instance[UserInfo] { cursor =>
    for {
      id <- cursor.downField("sub").as[String]
      email <- cursor.downField("email").as[String]
      verifiedEmail <- {
        cursor.downField("email_verified").as[String].map(str => if (str === "true") true else false)
          .orElse(cursor.downField("email_verified").as[Boolean])
      }
      name <- cursor.downField("name").as[String]
      givenName <- cursor.downField("given_name").as[String]
      familyName <- cursor.downField("family_name").as[String]
      profile <- cursor.downField("profile").as[String]
      picture <- cursor.downField("picture").as[String]
      gender <- cursor.downField("gender").as[String]
    } yield {
      UserInfo(id, email, verifiedEmail, name, givenName, familyName, profile, picture, gender)
    }
  }

  val openIdConnectBasicUserInfoDecoder = Decoder.instance[BasicUserInfo] { cursor =>
    for {
      id <- cursor.downField("sub").as[String]
      email <- cursor.downField("email").as[String]
      verifiedEmail <- {
        cursor.downField("email_verified").as[String].map(str => if (str === "true") true else false)
          .orElse(cursor.downField("email_verified").as[Boolean])
      }
      name <- cursor.downField("name").as[String]
      givenName <- cursor.downField("given_name").as[String]
      familyName <- cursor.downField("family_name").as[String]
      picture <- cursor.downField("picture").as[String]
    } yield {
      BasicUserInfo(id, email, verifiedEmail, name, givenName, familyName, picture)
    }
  }

  val facebookUserInfoDecoder = Decoder.instance[UserInfo] { json =>
    for {
      id <- json.downField("id").as[String]
      email <- json.downField("email").as[String]
      verified <- json.downField("verified").as[Boolean]
      name <- json.downField("name").as[String]
      firstName <- json.downField("first_name").as[String]
      lastName <- json.downField("last_name").as[String]
      link <- json.downField("link").as[String]
      gender <- json.downField("gender").as[String]
    } yield {
      UserInfo(id, email, verified, name, firstName, lastName, link, f"https://graph.facebook.com/v2.3/$id/picture", gender)
    }
  }

  val facebookBasicUserInfoDecoder = Decoder.instance[BasicUserInfo] { json =>
    for {
      id <- json.downField("id").as[String]
      email <- json.downField("email").as[String]
      verified <- json.downField("verified").as[Boolean]
      name <- json.downField("name").as[String]
      firstName <- json.downField("first_name").as[String]
      lastName <- json.downField("last_name").as[String]
    } yield {
      BasicUserInfo(id, email, verified, name, firstName, lastName, f"https://graph.facebook.com/v2.3/$id/picture")
    }
  }

}

