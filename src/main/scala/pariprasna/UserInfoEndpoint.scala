package pariprasna

import cats.data.Xor
import io.circe._
import org.http4s._

import scalaz._
import Scalaz._

object UserInfoEndpoint {


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
  val facebookUserInfoUri = Uri.uri("https://graph.facebook.com/v2.8/me")



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


  // all fields, but "sub" are optional

  // TODO use claims
  case class UserInfo(
    sub: String,
    email: String,
    verifiedEmail: Boolean,
    name: String,
    givenName: String,
    familyName: String,
    profile: String,
    picture: String,
    gender: String
  )

  case class BasicUserInfo(
    sub: String,
    email: String,
    verifiedEmail: Boolean,
    name: String,
    givenName: String,
    familyName: String,
    picture: String
  )

  implicit val userInfoEq = Equal.equalA[UserInfo]
  implicit val userInfoShow = Show.showA[UserInfo]


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

  implicit val nonStandardUserInfoClaimDecoder: Decoder[List[UserInfoClaim]] = Decoder[JsonObject].flatMap { obj =>
      Decoder.instance[List[UserInfoClaim]] { json =>

        import cats.std.list._
        import cats.syntax.traverse._

        val xs: List[Decoder.Result[List[UserInfoClaim]]] = obj.toList.map {
          case ("sub", x) => x.as[String].map(UserInfoClaim.Sub).map(x => List[UserInfoClaim](x))
          case ("name", x) => x.as[String].map(UserInfoClaim.Name).map(x => List[UserInfoClaim](x))
          case ("given_name", x) => x.as[String].map(UserInfoClaim.GivenName).map(x => List[UserInfoClaim](x))
          case ("family_name", x) => x.as[String].map(UserInfoClaim.FamilyName).map(x => List[UserInfoClaim](x))
          case ("middle_name", x) => x.as[String].map(UserInfoClaim.MiddleName).map(x => List[UserInfoClaim](x))
          case ("nickname", x) => x.as[String].map(UserInfoClaim.Nickname).map(x => List[UserInfoClaim](x))
          case ("preferred_username", x) => x.as[String].map(UserInfoClaim.PreferredUsername).map(x => List[UserInfoClaim](x))
          case ("profile", x) => x.as[String].map(UserInfoClaim.Profile).map(x => List[UserInfoClaim](x))
          case ("picture", x) => x.as[String].map(UserInfoClaim.Picture).map(x => List[UserInfoClaim](x))
          case ("website", x) => x.as[String].map(UserInfoClaim.Website).map(x => List[UserInfoClaim](x))
          case ("email", x) => x.as[String].map(UserInfoClaim.Email).map(x => List[UserInfoClaim](x))
          case ("email_verified", x) => x.as[Boolean](Decoder[Boolean].or(strBooleanDecoder)).map(UserInfoClaim.EmailVerified).map(x => List[UserInfoClaim](x))
          case ("gender", x) => x.as[String].map(UserInfoClaim.Gender).map(x => List[UserInfoClaim](x))
          case ("birthdate", x) => x.as[String].map(UserInfoClaim.Birthdate).map(x => List[UserInfoClaim](x))
          case ("locale", x) => x.as[String].map(UserInfoClaim.Locale).map(x => List[UserInfoClaim](x))
          case ("phone_number", x) => x.as[String].map(UserInfoClaim.PhoneNumber).map(x => List[UserInfoClaim](x))
          case ("phone_number_verified", x) => x.as[Boolean](Decoder[Boolean].or(strBooleanDecoder)).map(UserInfoClaim.PhoneNumberVerified).map(x => List[UserInfoClaim](x))
          case ("address", x) => cats.data.Xor.Right(List[UserInfoClaim](UserInfoClaim.Address(x)))
          case ("updated_at", x) => x.as[Long].map(UserInfoClaim.UpdatedAt).map(x => List[UserInfoClaim](x))

          // facebook
          case ("id", x) => x.as[String].map(x => List[UserInfoClaim](UserInfoClaim.Sub(x), UserInfoClaim.Picture(s"https://graph.facebook.com/v2.8/$x/picture")))
          case ("first_name", x) => x.as[String].map(UserInfoClaim.GivenName).map(x => List[UserInfoClaim](x))
          case ("last_name", x) => x.as[String].map(UserInfoClaim.FamilyName).map(x => List[UserInfoClaim](x))
          case ("link", x) => x.as[String].map(UserInfoClaim.Profile).map(x => List[UserInfoClaim](x))
          case ("timezone", x) => x.as[Int] map (x => List[UserInfoClaim](UserInfoClaim.Zoneinfo(x.toString)))
          case ("verified", x) => x.as[Boolean](Decoder[Boolean].or(strBooleanDecoder)).map(UserInfoClaim.EmailVerified).map(x => List[UserInfoClaim](x))

          // custom
          case (key, x) => cats.data.Xor.Right(List[UserInfoClaim](UserInfoClaim.Custom(key, x)))
        }

        xs.sequence.map(_.flatten)

      }
    }

  implicit val userInfoClaimEncoder: Encoder[List[UserInfoClaim]] = Encoder.instance[List[UserInfoClaim]] { xs =>

    val xs1: List[(String, Json)] = xs.map {
      case UserInfoClaim.Sub(x) => ("sub", Json.fromString(x))
      case UserInfoClaim.Name(x) => ("name", Json.fromString(x))
      case UserInfoClaim.GivenName(x) => ("given_name", Json.fromString(x))
      case UserInfoClaim.FamilyName(x) => ("family_name", Json.fromString(x))
      case UserInfoClaim.MiddleName(x) => ("middle_name", Json.fromString(x))
      case UserInfoClaim.Nickname(x) => ("nickname", Json.fromString(x))
      case UserInfoClaim.PreferredUsername(x) => ("preferred_username", Json.fromString(x))
      case UserInfoClaim.Profile(x) => ("profile", Json.fromString(x))
      case UserInfoClaim.Picture(x) => ("picture", Json.fromString(x))
      case UserInfoClaim.Website(x) => ("website", Json.fromString(x))
      case UserInfoClaim.Email(x) => ("email", Json.fromString(x))
      case UserInfoClaim.Gender(x) => ("gender", Json.fromString(x))
      case UserInfoClaim.Birthdate(x) => ("birthdate", Json.fromString(x))
      case UserInfoClaim.Zoneinfo(x) => ("zoneinfo", Json.fromString(x))
      case UserInfoClaim.Locale(x) => ("locale", Json.fromString(x))
      case UserInfoClaim.PhoneNumber(x) => ("phone_number", Json.fromString(x))
      case UserInfoClaim.UpdatedAt(x) => ("updated_at", Json.fromLong(x))
      case UserInfoClaim.Address(x) => ("address", x)
      case UserInfoClaim.EmailVerified(x) => ("email_verified", Json.fromBoolean(x))
      case UserInfoClaim.PhoneNumberVerified(x) => ("phone_number_verified", Json.fromBoolean(x))
      case UserInfoClaim.Custom(key, x) => (key, x)
    }

    Json.obj(xs1:_*)
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




  // OpenID Connect UserInfo format
  // http://openid.net/specs/openid-connect-core-1_0.html#StandardClaims
  val openIdConnectUserInfoDecoder = Decoder.instance[UserInfo] { cursor =>
    for {
      sub <- cursor.downField("sub").as[String]
      email <- cursor.downField("email").as[String]
      verifiedEmail <- cursor.downField("email_verified").as[Boolean]
      name <- cursor.downField("name").as[String]
      givenName <- cursor.downField("given_name").as[String]
      familyName <- cursor.downField("family_name").as[String]
      profile <- cursor.downField("profile").as[String]
      picture <- cursor.downField("picture").as[String]
      gender <- cursor.downField("gender").as[String]
    } yield {
      UserInfo(sub, email, verifiedEmail, name, givenName, familyName, profile, picture, gender)
    }
  }

  // OpenID Connect-like
  // email_verified is a string
  val googlePlusUserInfoDecoder = Decoder.instance[UserInfo] { cursor =>
    for {
      sub <- cursor.downField("sub").as[String]
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
      UserInfo(sub, email, verifiedEmail, name, givenName, familyName, profile, picture, gender)
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
      UserInfo(id, email, verified, name, firstName, lastName, link, f"https://graph.facebook.com/v2.8/$id/picture", gender)
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
      BasicUserInfo(id, email, verified, name, firstName, lastName, f"https://graph.facebook.com/v2.8/$id/picture")
    }
  }

}

