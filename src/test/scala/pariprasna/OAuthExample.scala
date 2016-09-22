package pariprasna

import org.http4s._

import scalaz._, Scalaz._
import scalaz.concurrent._


object OAuthExample extends App  {


  val redirectUri = Uri.uri("http://localhost/oauth/callback")

  val client = org.http4s.client.blaze.defaultClient

  val res1 = for {
    authorizationRequestRedirect <- OAuthClient.startAuthorizationRequest(OAuthEndpoints.google, OAuthEndpoints.googleCreds, redirectUri, "")
    authorizationResponse <- OAuthClient.finishAuthorizationRequest(authorizationRequestRedirect)
    tokenResponse <- OAuthClient.exchangeCodeForAccessToken(OAuthEndpoints.google, OAuthEndpoints.googleCreds, redirectUri, authorizationResponse.code)
    user <- OAuthClient.fetchUserProfile(OAuthEndpoints.google.key, tokenResponse.accessToken)
    user2 <- OAuthClient.fetchUserProfile("google+", tokenResponse.accessToken)
  } yield (user, user2, user === user2)

  println(res1.run(client).unsafePerformSync)


}
