// Upon user clicking on file upload button and making a change, re-enable upload button.
$('#fileUpload').on('change', function(e) {
  $('#das_buttons > .save').removeAttr('disabled');
  loopLogFileSubmit();

  if (undefined !== window.extraCredit && window.extraCredit) {
    loopAddBypassExtraCreditTimer();
  }
});

function loopLogFileSubmit() {
  window.setTimeout(function() {
    if ($('#das_buttons > .save').attr('onclick') === undefined) {
        $('#das_buttons > .save').attr('onclick', "logFileSubmit()");
        loopLogFileSubmit();
    }
  }, 10);
}

function genericLogFileSubmit(itemToLog) {
  // If a cookie exists for this particular question, get it.
  var questionLog = getCookie(fullQuestionName);
  if (questionLog === undefined) {
    questionLog = [];
  }

  var fileLog = {
    'filename': itemToLog,
    'timestamp': Date.now()
  };

  questionLog.unshift(fileLog);
  setCookie(window.fullQuestionName, JSON.stringify(questionLog));
  showLastUpload();
}

function logFileSubmit() {
  genericLogFileSubmit(getFilenameOfUpload());
}

$(document).ajaxComplete(function(event, xhr, settings){
  var urlParts = settings.url.split('/');
  if (urlParts[urlParts.length - 1] === 'submission') {
     logFileSubmit();
  }
});

// TODO check if #fileUpload exists
// Returns the name of the file.
function getFilenameOfUpload() {
  var fullPath = $('#fileUpload').val();
  return fullPath.substring(fullPath.lastIndexOf('\\') + 1);
}

// Updates the last upload log for display.
function showLastUpload() {
  var questionLog = getCookie(window.fullQuestionName);
  if (questionLog !== undefined) {
    var uploadLog = questionLog[0];
    var uploadFilename = uploadLog['filename'];
    var uploadTimestamp = uploadLog['timestamp'];

    var uploadDatetime = new Date();
    uploadDatetime.setTime(uploadTimestamp);

    var message = "Last Submission: " +
      uploadFilename + " at " + uploadDatetime.toString();

    $('#uploadLog').css('display', 'block').text(message);
  }
}

// so unique amirite
function generateUniqueQuestionId() {
  return fullQuestionName;
}

$(document).ready(function(){
    var btn_container_count = $('#qsubmit > div').length;

  if (btn_container_count == 1) {
    $('#qsubmit > div').attr('id', 'das_buttons');
  }

  showLastUpload();
});
