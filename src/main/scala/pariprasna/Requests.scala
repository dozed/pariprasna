package pariprasna

import org.http4s._
import org.http4s.client.Client

import scalaz.concurrent._
import scalaz._, Scalaz._

object Requests {

  def authorizationRequest(endpoint: OAuthEndpoint, credentials: OAuthCredentials, redirectUri: Uri, state: String): Request = {

    val uri = endpoint.authorizationUri
      .withQueryParam(OAuthAttribute.ClientId.name, credentials.clientId)
      .withQueryParam(OAuthAttribute.ResponseType.name, "code")
      .withQueryParam(OAuthAttribute.Scope.name, endpoint.scopes.mkString(" "))
      .withQueryParam(OAuthAttribute.RedirectUri.name, redirectUri.toString)
      .withQueryParam(OAuthAttribute.State.name, state)

    Request(Method.GET, uri)

  }

  // .as[TokenResponse]
  def exchangeCodeForAccessTokenRequest(endpoint: OAuthEndpoint, credentials: OAuthCredentials, code: AuthorizationCode): Request = {

    val form = UrlForm(
      OAuthAttribute.Code.name -> code.value,
      OAuthAttribute.ClientId.name -> credentials.clientId,
      OAuthAttribute.ClientSecret.name -> credentials.clientSecret,
      OAuthAttribute.GrantType.name -> "authorization_code"
    )

    Request(Method.POST, endpoint.tokenUri).withBody(form).unsafePerformSync

  }

}
