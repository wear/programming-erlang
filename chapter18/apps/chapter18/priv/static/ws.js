(function(){
  var w = new WebSocket("ws:127.0.0.1:8080/clock");
  showScreen('Connecting...');

  w.onopen = function() {
    showScreen("opened");
  }
  w.onmessage = function(e) {
    updateTime(e.data);
  }
  w.onclose = function(e) {
    showScreen("closed");
  }
  w.onerror = function(e) {
    showScreen("error");
  }

  function showScreen(txt){
    $("p.tips").html(txt);
  }

  function updateTime(txt){
    $('.message').text(txt);
  }

  $('.action-item').on('click', function(e){
    var actoin = $(e.target).text().toLowerCase();
    var blob = new Blob([actoin], {type: 'text/plain'});
    w.send(blob);
  })
})()