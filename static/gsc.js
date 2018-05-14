console.log("hello")
var socket = new WebSocket("ws://localhost:8081/ws")

socket.onmessage = function(event){
  console.log(event.data)
}
