define(["SimpleClient", "text!./question.html", "text!./answer.html", "text!./submission.html", "underscore"], function(SimpleClient, questionTemplate, answerTemplate, submissionTemplate, _) {

    var client = new SimpleClient.SimpleClient({questionTemplate: questionTemplate, answerTemplate: answerTemplate, submissionTemplate: submissionTemplate});

    client.on('renderQuestionFinished', function() {
        client.addAnswer('_files');

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

        // We have to decode from base-64
        if (client.submittedAnswer.has('_files')) {
            var files = client.submittedAnswer.get('_files')
            _.each(files, function(file) {
                if (file.name === 'hoare_logic_problem.json') {
                    $('#serializedTree').val(b64DecodeUnicode(file.contents));
                }
            });
        }

        // Note: file is base-64 encoded!
        $('#serializedTree').on('click', function(e) {
            var jsonTree = JSON.parse($('#serializedTree').val());
            var tupleStrings = _.keys(jsonTree).map(function (key) {
                var val = jsonTree[key];
                var record = '{' +
                    'str_label  = "'        + val.label         + '";' +
                    'str_left   = "'        + val.left          + '";' +
                    'str_middle = "'        + val.middle        + '";' +
                    'str_right  = "'        + val.right         + '";' +
                    'str_sideCondition = "' + val.sideCondition + '"' +
                    '}';
                return "(\"" + key + "\", " + record + ")";
            });
            console.log(tupleStrings);

            var ocamlFile = "open Genutils;;\n let tree = [";
            _.each(tupleStrings, function (tuple) {
                ocamlFile += tuple + ";\n";
            });
            ocamlFile += "]";
            console.log(ocamlFile);

            var files = [{
                name: 'hoare_logic_problem.ml',
                contents: b64EncodeUnicode(ocamlFile)
            },
                         {
                name: 'hoare_logic_problem.json',
                contents: b64EncodeUnicode($('#serializedTree').val())
            }];
            client.submittedAnswer.set('_files', files);
        });
    });

    return client;
});
