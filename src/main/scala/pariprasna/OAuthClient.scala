package pariprasna

import org.http4s._
import org.http4s.client.Client

import scalaz.concurrent._
import scalaz._, Scalaz._

object OAuthClient {

  def req[A](f: Client => Task[A]): Kleisli[Task, Client, A] = Kleisli(f)


  def startAuthorizationRequest(endpoint: OAuthEndpoint, credentials: OAuthCredentials, redirectUri: Uri, state: String) = req[AuthorizationRequestRedirect] { client =>
    client.fetch[AuthorizationRequestRedirect](OAuthRequests.authorizationRequest(endpoint, credentials, redirectUri, state)) {
      res =>
        res.headers.get(headers.Location).cata(
          loc => Task.now(AuthorizationRequestRedirect(loc.uri)),
          Task.fail(OAuthError.AuthResponseLocationHeaderMissing)
        )
    }
  }

  // TODO automate
  def finishAuthorizationRequest(redirect: AuthorizationRequestRedirect) = req[AuthorizationResponse] { client =>

    println("Visit browser:")
    println(redirect.uri.toString)

    println("Enter redirect uri:")
    Uri.fromString(scala.io.StdIn.readLine.trim).fold(
      _ => Task.fail(OAuthError.ParseError("redirect uri")),
      uri => readAuthorizationResponse(uri).run(client)
    )

  }

  def readAuthorizationResponse(uri: Uri) = req[AuthorizationResponse] { client =>

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
      error => Task.fail(OAuthError.AuthorizationError(error, uri.params.get(OAuthAttribute.Description.name))),
      uri.params.get(OAuthAttribute.Code.name).cata(
        code => Task.now(AuthorizationResponse(AuthorizationCode(code), uri.params.get(OAuthAttribute.State.name))),
        Task.fail(OAuthError.AuthRequestCodeMissing)
      )
    )
  }

  def exchangeCodeForAccessToken(endpoint: OAuthEndpoint, credentials: OAuthCredentials, redirectUri: Uri, code: AuthorizationCode) = req[TokenResponse] { client =>
    client.fetchAs[TokenResponse](OAuthRequests.exchangeCodeForAccessTokenRequest(endpoint, credentials, code, redirectUri))(tokenResponseEntityDecoder)
  }

  def fetchUserProfile(providerKey: String, accessToken: AccessToken) = req[OAuthProfileApi.UserProfile] { client =>
    val apiEndpointUri = OAuthProfileApi.lookupProfileApi(providerKey)
    val reader = OAuthProfileApi.lookupProfileReader(providerKey)
    client.fetchAs[OAuthProfileApi.UserProfile](OAuthProfileApi.fetchUserProfile(apiEndpointUri, accessToken))(circeDecoderAsEntityDecoder(reader))
  }



}
