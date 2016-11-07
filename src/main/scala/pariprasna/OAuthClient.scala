package pariprasna

import org.http4s._
import org.http4s.client.Client

import scalaz.concurrent._
import scalaz._, Scalaz._

object OAuthClient {

  def req[A](f: Client => Task[A]): Kleisli[Task, Client, A] = Kleisli(f)


  def startAuthorization(endpoint: OAuthEndpoint, credentials: OAuthCredentials, redirectUri: Uri, state: String) = req[AuthorizationRequestRedirect] { client =>
    client.fetch[AuthorizationRequestRedirect](Requests.authorizationRequest(endpoint, credentials, redirectUri, state)) {
      res =>
        res.headers.get(headers.Location).cata(
          loc => Task.now(AuthorizationRequestRedirect(loc.uri)),
          Task.fail(OAuthError.AuthResponseLocationHeaderMissing)
        )
    }
  }

  // TODO automate
  def finishAuthorization(redirect: AuthorizationRequestRedirect) = req[AuthorizationResponse] { client =>

    println("Visit browser:")
    println(redirect.uri.toString)

    println("Enter redirect uri:")
    Uri.fromString(scala.io.StdIn.readLine.trim).fold(
      _ => Task.fail(OAuthError.ParseError("redirect uri")),
      uri => readAuthorizationResponseUri(uri)
    )

  }

  def exchangeCodeForToken(endpoint: OAuthEndpoint, credentials: OAuthCredentials, code: AuthorizationCode, redirectUri: Uri) = req[TokenResponse] { client =>
    client.fetchAs[TokenResponse](Requests.exchangeCodeForAccessTokenRequest(endpoint, credentials, code, redirectUri))
  }

  def fetchUserInfo(providerKey: String, accessToken: AccessToken) = req[UserInfoEndpoint.UserInfo] { client =>
    val apiEndpointUri = UserInfoEndpoint.lookupUserInfoEndpoint(providerKey)
    val reader = UserInfoEndpoint.lookupUserInfoDecoder(providerKey)
    client.fetchAs[UserInfoEndpoint.UserInfo](UserInfoEndpoint.fetchUserInfo(apiEndpointUri, accessToken))(Util.circeDecoderAsEntityDecoder(reader))
  }


  def readAuthorizationResponseUri(uri: Uri): Task[AuthorizationResponse] = {

    // success case
    // https://www.example.org/oauth2callback?
    //   state=security_token%3D...%26action%3Dlogin&
    //   code=...&
    //   authuser=0&
    //   num_sessions=1&
    //   prompt=consent&
    //   session_state=...
    //
    // error case
    // https://www.example.org/oauth2callback?error=access_denied&state=security_token%3D...%26action%3Dlogin

    uri.params.get(OAuthAttribute.Error.name).cata(
      error => Task.fail(OAuthError.AuthorizationError(error, uri.params.get(OAuthAttribute.Description.name).orElse(uri.params.get(OAuthAttribute.ErrorDescription.name)))),
      uri.params.get(OAuthAttribute.Code.name).cata(
        code => Task.now(AuthorizationResponse(AuthorizationCode(code), uri.params.get(OAuthAttribute.State.name))),
        Task.fail(OAuthError.AuthRequestCodeMissing)
      )
    )
  }



}
