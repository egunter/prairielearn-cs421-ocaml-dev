define(["QServer", "underscore"], function(QServer, _) {

    var server = new QServer();

    server.getData = function(vid) {
        var params = {};
        var answers = {};
        var trueAnswer = {
          answers: answers
        };

        var questionData = {
            trueAnswer: trueAnswer
        };

        return questionData;
    };

    server.gradeAnswer = function(vid, params, trueAnswer, submittedAnswer, options) {
        var score = 0.0;
        var keys = Object.keys(trueAnswer.answers);

        console.log(trueAnswer.answers);
        console.log(submittedAnswer);

        for (var i = 0; i < keys.length; i++) {
          var key = keys[i];
          if (((key == 'allFine' && submittedAnswer[key]) || submittedAnswer[key]) &&
              (trueAnswer.answers[key] == submittedAnswer[key])) {
            score = 1.0;
          }
        }

        return {score: score};
    };

    return server;
});
