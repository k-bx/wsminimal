function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host
                + (window.location.port ? ':' + window.location.port : '')
                + path;

    var Socket = window.WebSocket;
    return new Socket(uri);
}

document.addEventListener('DOMContentLoaded', function(){
    console.log("producer.js begins");
    var ws = createWebSocket('/');
    ws.onopen = function() {
        console.log("onopen, will send ping");
        ws.send('ping');
    };
    ws.onmessage = function(event) {
        var evData = event.data;
        console.log("Got this: ", evData);
    };
    ws.onerror = function(err) {
        console.log("Error happened: ", err);
    };

    window.onbeforeunload = function(){
        console.log("onbeforeunload. will close ws.");
        ws.close();
    };
});
