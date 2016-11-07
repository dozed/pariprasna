


```scala
val creds = OAuthCredentials.fromText(
    """
      |{
      |  "facebook": {
      |    "clientId": "...",
      |    "clientSecret": "..."
      |  },
      |  "google": {
      |    "clientId": "...",
      |    "clientSecret": "..."
      |  }
      |}
    """.stripMargin
  )

val endpoints = Map(
  "facebook" -> OAuthEndpoint("facebook", List("email", "public_profile"), Uri.uri("https://www.facebook.com/v2.8/dialog/oauth"), Uri.uri("https://graph.facebook.com/v2.8/oauth/access_token")),
  "google" -> OAuthEndpoint("google", List("openid", "email", "profile"), Uri.uri("https://accounts.google.com/o/oauth2/v2/auth"), Uri.uri("https://www.googleapis.com/oauth2/v4/token"))
)

val redirectUri = Uri.uri("http://localhost/oauth/callback")

val client = org.http4s.client.blaze.defaultClient

def fetchUserInfo(provider: String, credentialsStore: Map[String, OAuthCredentials], endpointStore: Map[String, OAuthEndpoint]): Kleisli[Task, Client, List[UserInfoClaim]] = {
  for {
    credentials                  <- OAuthClient.req(_ => credentialsStore.get(provider).cata(Task.now, Task.fail(new RuntimeException(s"unknown credentials $provider"))))
    endpoint                     <- OAuthClient.req(_ => endpointStore.get(provider).cata(Task.now, Task.fail(new RuntimeException(s"unknown endpoint $provider"))))
    authorizationRequestRedirect <- OAuthClient.startAuthorization(endpoint, credentials, redirectUri, "")
    authorizationResponse        <- OAuthClient.finishAuthorization(authorizationRequestRedirect)
    tokenResponse                <- OAuthClient.exchangeCodeForToken(endpoint, credentials, authorizationResponse.code, redirectUri)
    claims                       <- OAuthClient.fetchUserInfo(provider, tokenResponse.accessToken, UserInfoEndpoint.nonStandardUserInfoClaimDecoder)
  } yield claims
}

println(fetchUserInfo("google", creds, endpoints).run(client).unsafePerformSync)

```

See [https://github.com/dozed/pariprasna/blob/master/src/test/scala/pariprasna/OAuthExample.scala](https://github.com/dozed/pariprasna/blob/master/src/test/scala/pariprasna/OAuthExample.scala)