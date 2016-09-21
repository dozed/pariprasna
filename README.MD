


```scala

val redirectUri = Uri.uri("http://localhost/oauth/callback")

val client = org.http4s.client.blaze.defaultClient

val res1 = for {
  authorizationRequestRedirect <- OAuthClient.startAuthorizationRequest(OAuthEndpoints.google, OAuthEndpoints.googleCreds, redirectUri, "")
  authorizationResponse <- OAuthClient.finishAuthorizationRequest(authorizationRequestRedirect)
  tokenResponse <- OAuthClient.exchangeCodeForAccessToken(OAuthEndpoints.google, OAuthEndpoints.googleCreds, redirectUri, authorizationResponse.code)
  user <- OAuthClient.fetchUserProfile(OAuthEndpoints.google.key, tokenResponse.accessToken)
} yield user

println(res1.run(client).unsafePerformSync)

```

See [https://github.com/dozed/pariprasna/blob/master/src/test/scala/pariprasna/OAuthExample.scala](https://github.com/dozed/pariprasna/blob/master/src/test/scala/pariprasna/OAuthExample.scala)