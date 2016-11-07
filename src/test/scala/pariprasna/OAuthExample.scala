package pariprasna

import org.http4s._
import org.http4s.client.Client
import pariprasna.UserInfoEndpoint.UserInfoClaim

import scalaz._
import Scalaz._
import scalaz.concurrent._


object OAuthExample extends App  {

  val client = org.http4s.client.blaze.defaultClient

  val creds = OAuthCredentials.fromFile("src/test/resources/credentials.json")

  val endpoints = Map(
    "facebook" -> OAuthEndpoint("facebook", List("email", "public_profile"), Uri.uri("https://www.facebook.com/v2.8/dialog/oauth"), Uri.uri("https://graph.facebook.com/v2.8/oauth/access_token")),
    "google" -> OAuthEndpoint("google", List("openid", "email", "profile"), Uri.uri("https://accounts.google.com/o/oauth2/v2/auth"), Uri.uri("https://www.googleapis.com/oauth2/v4/token"))
  )

  val redirectUri = Uri.uri("http://localhost/oauth/callback")


  def fetchUserInfo(provider: String, credentialsStore: Map[String, OAuthCredentials], endpointStore: Map[String, OAuthEndpoint]): ReaderT[Task, Client, List[UserInfoClaim]] = {
    for {
      credentials                  <- OAuthClient.req(_ => credentialsStore.get(provider).cata(Task.now, Task.fail(new RuntimeException(s"unknown credentials $provider"))))
      endpoint                     <- OAuthClient.req(_ => endpointStore.get(provider).cata(Task.now, Task.fail(new RuntimeException(s"unknown endpoint $provider"))))
      authorizationRequestRedirect <- OAuthClient.startAuthorization(endpoint, credentials, redirectUri, "")
      authorizationResponse        <- OAuthClient.finishAuthorization(authorizationRequestRedirect)
      tokenResponse                <- OAuthClient.exchangeCodeForToken(endpoint, credentials, authorizationResponse.code, redirectUri)
      claims                       <- OAuthClient.fetchUserInfo(provider, tokenResponse.accessToken, UserInfoEndpoint.nonStandardUserInfoClaimDecoder)
    } yield claims
  }

  println(fetchUserInfo("facebook", creds, endpoints).run(client).unsafePerformSyncAttempt)
  println(fetchUserInfo("google", creds, endpoints).run(client).unsafePerformSyncAttempt)



}
