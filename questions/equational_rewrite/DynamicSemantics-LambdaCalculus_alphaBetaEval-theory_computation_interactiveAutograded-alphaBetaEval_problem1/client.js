define(["SimpleClient", "text!./question.html", "text!./answer.html", "text!./submission.html", "clientCode/lambda-checker", "clientCode/lambda-checker-helper"], function(SimpleClient, questionTemplate, answerTemplate, submissionTemplate, lambdaChecker, lambdaCheckerHelper) {

    // ------------------------------------
    // BEGIN Question Specific Values
    // ------------------------------------
    var maxAttemps = 3,
        alphaValue = 0,
        betaValue = 3,
        maxScore = 27,
        question = '(% x. x (% y. x y)) (% z. (% v. (% w. (z w) (w v) y))) ((% u. u u)(%w. w))';
    // ------------------------------------
    // END Question Specific Values
    // ------------------------------------

    var client = new SimpleClient.SimpleClient({questionTemplate: questionTemplate, answerTemplate: answerTemplate}),
        lambdaCheckerHelper = new LambdaCheckerHelper(question, maxAttemps, maxScore, checkAlphaBetaAnswer, client, "alphaBetaEval_problem1.json");


    client.on("renderQuestionFinished", function() {
        lambdaCheckerHelper.init();


        // https://developer.mozilla.org/en-US/docs/Web/API/WindowBase64/Base64_encoding_and_decoding#The_Unicode_Problem
        function b64EncodeUnicode(str) {
            // first we use encodeURIComponent to get percent-encoded UTF-8,
            // then we convert the percent encodings into raw bytes which
            // can be fed into btoa.
            return btoa(encodeURIComponent(str).replace(/%([0-9A-F]{2})/g,
                                                        function toSolidBytes(match, p1) {
                                                            return String.fromCharCode('0x' + p1);
                                                        }));
        }

        // https://developer.mozilla.org/en-US/docs/Web/API/WindowBase64/Base64_encoding_and_decoding#The_Unicode_Problem
        function b64DecodeUnicode(str) {
            // Going backwards: from bytestream, to percent-encoding, to original string.
            return decodeURIComponent(atob(str).split('').map(function(c) {
                return '%' + ('00' + c.charCodeAt(0).toString(16)).slice(-2);
            }).join(''));
        }

        client.addAnswer("_files");
        client.submittedAnswer.set
        ("_files",
         [{name: "alphaBetaEval_problem1.json",
           contents: b64EncodeUnicode(client.submittedAnswer.get("inputHistory"))
           
          }]);
    });

    return client;
});
