function Subscription(id,evtHandlerPid,evtName,genMsgFn,variantTag) {
	this.evtHandlerPid = evtHandlerPid;
	this.id = id;
	this.evtName = evtName;
	this.genMsgFn = genMsgFn;
	this.variantTag = variantTag;
}

Subscription.prototype.addListener = function() {
	genMsgFn = this.genMsgFn;
	evtName = this.evtName;
	evtHandlerPid = this.evtHandlerPid;
	function cont(res) {
    	_Send(evtHandlerPid, res);
	}

	switch(this.variantTag) {
		case "VirtualDom.TimeHandler":
			window.setInterval(function() {
      		genMsgFn(cont);
    		}, evtName);
			break;
		case "VirtualDom.StringHandler":
		    if (evtName == "mousemove") {
      			document.addEventListener("mousemove", function(event) {
        			genMsgFn({1:event.clientX,2:event.clientY}, cont);
      			});
    		} else if (evtName == "keypress") {
      			document.addEventListener("keyup", function(event) {
        			var keycode = event.keyCode.toString();
        			genMsgFn(keycode, cont);
      			});
    		}
    		break;
    	default:
    		console.log("default");
	}
}

Subscription.prototype.log = function() {
	console.log(typeof this.genMsgFn);
}
