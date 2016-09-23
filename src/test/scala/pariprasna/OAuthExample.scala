package pariprasna

import org.http4s._

import scalaz._, Scalaz._
import scalaz.concurrent._


object OAuthExample extends App  {


  val redirectUri = Uri.uri("http://localhost/oauth/callback")

  val client = org.http4s.client.blaze.defaultClient

  val res1 = for {
    authorizationRequestRedirect <- OAuthClient.startAuthorization(OAuthEndpoints.google, OAuthEndpoints.googleCreds, redirectUri, "")
    authorizationResponse <- OAuthClient.finishAuthorization(authorizationRequestRedirect)
    tokenResponse <- OAuthClient.exchangeCodeForToken(OAuthEndpoints.google, OAuthEndpoints.googleCreds, redirectUri, authorizationResponse.code)
    user <- OAuthClient.fetchUserProfile(OAuthEndpoints.google.key, tokenResponse.accessToken)
    user2 <- OAuthClient.fetchUserProfile("google+", tokenResponse.accessToken)
  } yield (user, user2, user === user2)

  println(res1.run(client).unsafePerformSyncAttempt)

  val res2 = for {
    authorizationRequestRedirect <- OAuthClient.startAuthorization(OAuthEndpoints.fb, OAuthEndpoints.fbCreds, redirectUri, "")
    authorizationResponse <- OAuthClient.finishAuthorization(authorizationRequestRedirect)
    tokenResponse <- OAuthClient.exchangeCodeForToken(OAuthEndpoints.fb, OAuthEndpoints.fbCreds, redirectUri, authorizationResponse.code)
    user <- OAuthClient.fetchUserProfile(OAuthEndpoints.fb.key, tokenResponse.accessToken)
  } yield user

  println(res2.run(client).unsafePerformSyncAttempt)


}
