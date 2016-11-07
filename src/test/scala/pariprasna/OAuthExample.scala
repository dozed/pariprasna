package pariprasna

import org.http4s._

import scalaz._
import Scalaz._
import scalaz.concurrent._


object OAuthExample extends App  {


  val client = org.http4s.client.blaze.defaultClient

  val creds = OAuthCredentials.fromFile("src/test/resources/credentials.json")
  val endpoints = Map(
    "facebook" -> OAuthEndpoint(List("email", "public_profile"), Uri.uri("https://www.facebook.com/dialog/oauth"), Uri.uri("https://graph.facebook.com/oauth/access_token")),
    "google" -> OAuthEndpoint(List("openid", "email", "profile"), Uri.uri("https://accounts.google.com/o/oauth2/v2/auth"), Uri.uri("https://www.googleapis.com/oauth2/v4/token"))
  )

  val redirectUri = Uri.uri("http://localhost/oauth/callback")


  def showUserInfo(provider: String, credentialsStore: Map[String, OAuthCredentials], endpointStore: Map[String, OAuthEndpoint]) = {
    for {
      credentials <- OAuthClient.req(_ => credentialsStore.get(provider).cata(Task.now, Task.fail(new RuntimeException(s"unknown credentials $provider"))))
      endpoint <- OAuthClient.req(_ => endpointStore.get(provider).cata(Task.now, Task.fail(new RuntimeException(s"unknown endpoint $provider"))))
      authorizationRequestRedirect <- OAuthClient.startAuthorization(endpoint, credentials, redirectUri, "")
      authorizationResponse <- OAuthClient.finishAuthorization(authorizationRequestRedirect)
      tokenResponse <- OAuthClient.exchangeCodeForToken(endpoint, credentials, authorizationResponse.code, redirectUri)
      user <- OAuthClient.fetchUserInfo(provider, tokenResponse.accessToken)
    } yield user
  }

  // println(showUserInfo("google", creds, endpoints).run(client).unsafePerformSyncAttempt)
  println(showUserInfo("facebook", creds, endpoints).run(client).unsafePerformSyncAttempt)




}
