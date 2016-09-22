package pariprasna

import org.http4s._
import scalaz._, Scalaz._

object OAuthEndpoints {

  val fb = OAuthEndpoint("facebook", List("email", "public_profile"), Uri.uri("https://www.facebook.com/dialog/oauth"), Uri.uri("https://graph.facebook.com/oauth/access_token"))
  val google = OAuthEndpoint("google", List("openid", "email", "profile"), Uri.uri("https://accounts.google.com/o/oauth2/v2/auth"), Uri.uri("https://www.googleapis.com/oauth2/v4/token"))

  val fbCreds = OAuthCredentials(???, ???)
  val googleCreds = OAuthCredentials(???, ???)

  def lookupEndpoint(s: String) = s match {
    case "facebook" => fb.some
    case "google" =>  google.some
    case _ => none
  }

  def lookupCredentials(s: String) = {
    s match  {
      case "facebook" => fbCreds.some
      case "google" =>  googleCreds.some
      case _ => None
    }
  }

}
