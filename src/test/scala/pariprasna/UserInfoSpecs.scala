package pariprasna

import org.specs2.mutable.Specification

import UserInfoEndpoint._

import io.circe.parser.parse

import scalaz._
import Scalaz._

class UserInfoSpecs extends Specification {

  val googleOpenIdUserInfo =
    parse("""
      |{
      |  "sub" : "42",
      |  "name" : "Soon Shine Sun",
      |  "given_name" : "Soon Shine",
      |  "family_name" : "Sun",
      |  "profile" : "https://plus.google.com/42",
      |  "picture" : "https://lh5.googleusercontent.com/-42/AA/AA/42/photo.jpg",
      |  "email" : "foo.bar@gmail.com",
      |  "email_verified" : true,
      |  "gender" : "male",
      |  "locale" : "en-GB"
      |}
    """.stripMargin).require

  val googleOpenIdUserInfo2 =
    parse("""
      |{
      | "sub": "42",
      | "name": "Soon Shine Sun",
      | "given_name": "Soon Shine",
      | "family_name": "Sun",
      | "picture": "https://lh3.googleusercontent.com/-42/AA/AA/42/photo.jpg",
      | "email": "foo.bar@gmail.com",
      | "email_verified": true,
      | "locale": "en"
      |}
      |
    """.stripMargin).require

  val googlePlusUserInfo =
    parse("""
      |{
      |  "kind" : "plus#personOpenIdConnect",
      |  "gender" : "male",
      |  "sub" : "42",
      |  "name" : "Soon Shine Sun",
      |  "given_name" : "Soon Shine",
      |  "family_name" : "Sun",
      |  "profile" : "https://plus.google.com/42",
      |  "picture" : "https://lh5.googleusercontent.com/-42/AA/AA/42/photo.jpg?sz=50",
      |  "email" : "foo.bar@gmail.com",
      |  "email_verified" : "true",
      |  "locale" : "en-GB"
      |}
    """.stripMargin).require

  val facebookUserInfo =
    parse("""
      |{
      |  "id" : "42",
      |  "email" : "foo.bar@gmail.com",
      |  "first_name" : "Soon Shine",
      |  "gender" : "male",
      |  "last_name" : "Sun",
      |  "link" : "https://www.facebook.com/app_scoped_user_id/42/",
      |  "locale" : "de_DE",
      |  "name" : "Soon Shine Sun",
      |  "timezone" : 2,
      |  "updated_time" : "2012-05-07T18:51:42+0000",
      |  "verified" : true
      |}
    """.stripMargin).require


  "UserInfo claims can be parsed from examples" in {

    googleOpenIdUserInfo.as[List[UserInfoClaim]].isRight should beTrue
    googleOpenIdUserInfo2.as[List[UserInfoClaim]].isRight should beTrue
    googlePlusUserInfo.as[List[UserInfoClaim]].isRight should beTrue

    // facebook fields would mostly parse into a list of UserInfoClaim.Custom
    // facebookUserInfo.as[List[UserInfoClaim]]

  }

  "UserInfo can be parsed from examples" in {

    googleOpenIdUserInfo.as[UserInfo](openIdConnectUserInfoDecoder).isRight should beTrue

    // only basic info
    googleOpenIdUserInfo2.as[UserInfo](openIdConnectUserInfoDecoder).isLeft should beTrue
    googlePlusUserInfo.as[UserInfo](googlePlusUserInfoDecoder).isRight should beTrue

    facebookUserInfo.as[UserInfo](facebookUserInfoDecoder).isRight should beTrue

  }

  "BasicUserInfo can be parsed from examples" in {

    googleOpenIdUserInfo.as[BasicUserInfo](openIdConnectBasicUserInfoDecoder).isRight should beTrue
    googleOpenIdUserInfo2.as[BasicUserInfo](openIdConnectBasicUserInfoDecoder).isRight should beTrue
    googlePlusUserInfo.as[BasicUserInfo](openIdConnectBasicUserInfoDecoder).isRight should beTrue
    facebookUserInfo.as[BasicUserInfo](facebookBasicUserInfoDecoder).isRight should beTrue

  }



}
