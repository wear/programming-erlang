function init(socket){
  var ws = new WebSocket("ws:127.0.0.1:8080/"+socket);
  showScreen('Connecting...');

  ws.onopen = function() {
    showScreen("opened");
  }
  ws.onclose = function(e) {
    showScreen("closed");
  }
  ws.onerror = function(e) {
    showScreen("error");
  }

  function showScreen(txt){
    $(".tips").html(txt);
  }

  $('.action-item').on('click', function(e){
    var msg = $(e.target).text().toLowerCase();
    ws.send(makeCmd('click', msg));
  })

  $("form#message").on('submit', function(e){
    e.preventDefault();
    sendCmd('input', $('#msg'))
  })

  $("form#join").on('submit', function(e){
    e.preventDefault();
    sendCmd('join', $('#user-name'))
  })

  function sendCmd(cmd, el){
    var message = $(el).val();
    if(message == ""){
      alert("cant not be blank!")
    } else {
      ws.send(makeCmd(cmd, message));
      $(el).val('');
    }
  }

  function makeCmd(k, v){
    var action = {};
    action[k] = v;
    var Cmd = JSON.stringify(action);
    return new Blob([Cmd], {type : 'application/json'});
  }

  window.ws = ws;
  window.makeCmd = makeCmd;
}