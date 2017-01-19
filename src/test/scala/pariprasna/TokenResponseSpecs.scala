package pariprasna

import io.circe.parser.parse
import org.specs2.mutable.Specification

class TokenResponseSpecs extends Specification {

  val tokenResponseErrorFacebook =
    parse("""
      |{
      |  "error": {
      |    "message": "This authorization code has expired.",
      |    "type": "OAuthException",
      |    "code": 100,
      |    "fbtrace_id": "foo"
      |  }
      |}
    """.stripMargin).require

  val tokenResponseErrorGoogle =
    parse("""
      |{
      |  "error": "invalid_grant",
      |  "error_description": "Bad Request"
      |}
    """.stripMargin).require

  "TokenResponseError can be parsed from examples" in {

    tokenResponseErrorFacebook.as[OAuthError.TokenResponseError].isRight should beTrue
    tokenResponseErrorGoogle.as[OAuthError.TokenResponseError].isRight should beTrue

  }




}
