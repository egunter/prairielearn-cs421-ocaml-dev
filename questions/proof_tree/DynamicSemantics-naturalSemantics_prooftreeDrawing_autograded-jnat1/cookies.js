/******************************
 * cookies.js
 * v1.1
 *
 * Written by Terence Nip
 *****************************/


function setCookie(name, value) {
  var now = new Date();
  var time = now.getTime();
  var expireTime = time + (1209600 * 1000);
  now.setTime(expireTime);

  timestamp = now.toGMTString();
  document.cookie = name + "=" + value + ";expires=" + timestamp;
}

function getCookie(name) {
  // Get all the cookies
  var allCookies = document.cookie;

  // Split on semicolons because primitive/hack
  allCookies = allCookies.split('; ');

  var cookieMap = {};
  for (var i = 0; i < allCookies.length; i++) {
    var cookieName = allCookies[i].substring(0, allCookies[i].indexOf('='));
    var cookieContents = allCookies[i].substring(allCookies[i].indexOf('=') + 1);
    cookieMap[cookieName] = cookieContents;
  }

  if (name in cookieMap) {
    return $.parseJSON(cookieMap[name]);
  } else {
    return undefined;
  }
}
