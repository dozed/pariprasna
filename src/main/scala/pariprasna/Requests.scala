package pariprasna

import org.http4s._
import org.http4s.client.Client

import scalaz.concurrent._
import scalaz._, Scalaz._

object Requests {

  def authorizationUri(endpoint: OAuthEndpoint, credentials: OAuthCredentials, redirectUri: Uri, state: String, display: LoginDisplay = LoginDisplay.Default): Uri = {

    val displayOpt = display match {
      case LoginDisplay.Popup => Some("popup")
      case LoginDisplay.Default => None
    }

    endpoint.authorizationUri
      .withQueryParam(OAuthAttribute.ClientId.name, credentials.clientId)
      .withQueryParam(OAuthAttribute.RedirectUri.name, redirectUri.toString)
      .withQueryParam(OAuthAttribute.ResponseType.name, "code")
      .withQueryParam(OAuthAttribute.Scope.name, endpoint.scopes.mkString(" "))
      .withQueryParam(OAuthAttribute.State.name, state)
      .withOptionQueryParam(OAuthAttribute.Display.name, displayOpt)

  }

  def authorizationRequest(endpoint: OAuthEndpoint, credentials: OAuthCredentials, redirectUri: Uri, state: String, display: LoginDisplay = LoginDisplay.Default): Request = {

    val uri = authorizationUri(endpoint, credentials, redirectUri, state)

    Request(Method.GET, uri)

  }

  // .as[TokenResponse]
  def exchangeCodeForAccessTokenRequest(endpoint: OAuthEndpoint, credentials: OAuthCredentials, code: AuthorizationCode, redirectUri: Uri): Request = {

    val form = UrlForm(
      OAuthAttribute.ClientId.name -> credentials.clientId,
      OAuthAttribute.ClientSecret.name -> credentials.clientSecret,
      OAuthAttribute.RedirectUri.name -> redirectUri.toString,
      OAuthAttribute.Code.name -> code.value,
      OAuthAttribute.GrantType.name -> "authorization_code"
    )

    Request(Method.POST, endpoint.tokenUri).withBody(form).unsafePerformSync

  }

}
