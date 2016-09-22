package pariprasna

import io.circe._
import org.http4s._
import scalaz._, Scalaz._

object OAuthProfileApi {

  case class UserProfile(
    id: String,
    email: String,
    verifiedEmail: Boolean,
    name: String,
    givenName: String,
    familyName: String,
    link: String,
    picture: String,
    gender: String
  )

  implicit val userProfileEq = Equal.equalA[UserProfile]

  // client.fetchAs[UserProfile](fetchUserProfile(.., ..))
  def fetchUserProfile(endpointUri: Uri, accessToken: AccessToken): Request = {
    Request(Method.GET, endpointUri).putHeaders(headers.Authorization(OAuth2BearerToken(accessToken.value)))
  }


  def lookupProfileReader(s: String): Decoder[UserProfile] = s match {
    case "facebook" => readFacebookUser
    case "google" =>  openIdConnectUserDecoder
    case "google+" =>  googlePlusUserDecoder
  }

  def lookupProfileApi(s: String): Uri = s match {
    case "facebook" => facebookProfileUri
    case "google" =>  googleProfileUri
    case "google+" =>  googlePlusGetOpenIdConnectUri
  }


  // Google: OpenID Connect
  // https://accounts.google.com/.well-known/openid-configuration
  val googleProfileUri = Uri.uri("https://www.googleapis.com/oauth2/v3/userinfo")

  // Google+ API user profile in OpenID Connect-like format
  // https://developers.google.com/+/web/api/rest/openidconnect/getOpenIdConnect
  val googlePlusGetOpenIdConnectUri = Uri.uri("https://www.googleapis.com/plus/v1/people/me/openIdConnect")

  // Facebook: proprietary OAuth 2.0 authentication extension
  val facebookProfileUri = Uri.uri("https://graph.facebook.com/me")



  // person's profile in OpenID Connect format
  // http://openid.net/specs/openid-connect-core-1_0.html#StandardClaims
  val openIdConnectUserDecoder = Decoder.instance[UserProfile] { cursor =>
    for {
      id <- cursor.downField("sub").as[String]
      email <- cursor.downField("email").as[String]
      verifiedEmail <- cursor.downField("email_verified").as[Boolean]
      name <- cursor.downField("name").as[String]
      givenName <- cursor.downField("given_name").as[String]
      familyName <- cursor.downField("family_name").as[String]
      link <- cursor.downField("profile").as[String]
      picture <- cursor.downField("picture").as[String]
      gender <- cursor.downField("gender").as[String]
    } yield {
      UserProfile(id, email, verifiedEmail, name, givenName, familyName, link, picture, gender)
    }
  }

  // OpenID Connect-like
  // email_verified is a string
  val googlePlusUserDecoder = Decoder.instance[UserProfile] { cursor =>
    for {
      id <- cursor.downField("sub").as[String]
      email <- cursor.downField("email").as[String]
      verifiedEmailStr <- cursor.downField("email_verified").as[String]
      verifiedEmail = if (verifiedEmailStr === "true") true else false
      name <- cursor.downField("name").as[String]
      givenName <- cursor.downField("given_name").as[String]
      familyName <- cursor.downField("family_name").as[String]
      link <- cursor.downField("profile").as[String]
      picture <- cursor.downField("picture").as[String]
      gender <- cursor.downField("gender").as[String]
    } yield {
      UserProfile(id, email, verifiedEmail, name, givenName, familyName, link, picture, gender)
    }
  }

  val readFacebookUser = Decoder.instance[UserProfile] { json =>
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
      UserProfile(id, email, verified, name, firstName, lastName, link, f"https://graph.facebook.com/v2.3/$id/picture", gender)
    }
  }

}

