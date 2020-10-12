#!/bin/bash

echo ""
echo "Applying migration ClaimStartDate"

echo "Adding routes to conf/app.routes"

echo "" >> ../conf/app.routes
echo "GET        /claimStartDate                  controllers.ClaimStartDateController.onPageLoad(mode: Mode = NormalMode)" >> ../conf/app.routes
echo "POST       /claimStartDate                  controllers.ClaimStartDateController.onSubmit(mode: Mode = NormalMode)" >> ../conf/app.routes

echo "GET        /changeClaimStartDate                        controllers.ClaimStartDateController.onPageLoad(mode: Mode = CheckMode)" >> ../conf/app.routes
echo "POST       /changeClaimStartDate                        controllers.ClaimStartDateController.onSubmit(mode: Mode = CheckMode)" >> ../conf/app.routes

echo "Adding messages to conf.messages"
echo "" >> ../conf/messages.en
echo "claimStartDate.title = ClaimStartDate" >> ../conf/messages.en
echo "claimStartDate.heading = ClaimStartDate" >> ../conf/messages.en
echo "claimStartDate.checkYourAnswersLabel = ClaimStartDate" >> ../conf/messages.en
echo "claimStartDate.error.required.all = Enter the claimStartDate" >> ../conf/messages.en
echo "claimStartDate.error.required.two = The claimStartDate" must include {0} and {1} >> ../conf/messages.en
echo "claimStartDate.error.required = The claimStartDate must include {0}" >> ../conf/messages.en
echo "claimStartDate.error.invalid = Enter a real ClaimStartDate" >> ../conf/messages.en

echo "Adding to UserAnswersEntryGenerators"
awk '/trait UserAnswersEntryGenerators/ {\
    print;\
    print "";\
    print "  implicit lazy val arbitraryClaimStartDateUserAnswersEntry: Arbitrary[(ClaimStartDatePage.type, JsValue)] =";\
    print "    Arbitrary {";\
    print "      for {";\
    print "        page  <- arbitrary[ClaimStartDatePage.type]";\
    print "        value <- arbitrary[Int].map(Json.toJson(_))";\
    print "      } yield (page, value)";\
    print "    }";\
    next }1' ../test/generators/UserAnswersEntryGenerators.scala > tmp && mv tmp ../test/generators/UserAnswersEntryGenerators.scala

echo "Adding to PageGenerators"
awk '/trait PageGenerators/ {\
    print;\
    print "";\
    print "  implicit lazy val arbitraryClaimStartDatePage: Arbitrary[ClaimStartDatePage.type] =";\
    print "    Arbitrary(ClaimStartDatePage)";\
    next }1' ../test/generators/PageGenerators.scala > tmp && mv tmp ../test/generators/PageGenerators.scala

echo "Adding to UserAnswersGenerator"
awk '/val generators/ {\
    print;\
    print "    arbitrary[(ClaimStartDatePage.type, JsValue)] ::";\
    next }1' ../test/generators/UserAnswersGenerator.scala > tmp && mv tmp ../test/generators/UserAnswersGenerator.scala

echo "Adding helper method to CheckYourAnswersHelper"
awk '/class CheckYourAnswersHelper/ {\
     print;\
     print "";\
     print "  def claimStartDate: Option[AnswerRow] = userAnswers.get(ClaimStartDatePage) map {";\
     print "    x =>";\
     print "      AnswerRow(";\
     print "        HtmlFormat.escape(messages(\"claimStartDate.checkYourAnswersLabel\")),";\
     print "        HtmlFormat.escape(x.format(dateFormatter)),";\
     print "        routes.ClaimStartDateController.onPageLoad(CheckMode).url";\
     print "      )";\
     print "  }";\
     next }1' ../app/utils/CheckYourAnswersHelper.scala > tmp && mv tmp ../app/utils/CheckYourAnswersHelper.scala

echo "Migration ClaimStartDate completed"
