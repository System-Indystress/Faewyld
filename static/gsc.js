console.log("hello");
var socket = new WebSocket("ws://localhost:8081/ws");

socket.onmessage = function(event){
  console.log(event.data);
};

$("#closeModal").click(function(event) {
  $("#modal").addClass("closeAnimate");
  setTimeout(function(){
    $("#modal").removeClass("closeAnimate");
    $("#modal").addClass("hide");
  },1000);

});
