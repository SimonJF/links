/////// STUFF FOR VIRTUAL DOM INTEGRATION
// Assumes virtual-dom.js has been loaded.

// Our current ****virtual-dom style**** representation of the VDom.
// Note that this isn't the Links representation -- you'll have to perform
// some processing to get it into virtual-dom form.
var currentVDom = undefined;
var rootNode = undefined;
var h = virtualDom.h;
var diff = virtualDom.diff;
var patch = virtualDom.patch;
var createElement = virtualDom.create;
var evtHandlerPid = undefined;
var inputText = "";
var currentSubID = 0;
var subscriptions = {};
var keyboardEvents = ["oninput", "onkeydown", "onkeypress", "onkeyup", "keydown", "keyup"];
var focusEvents = ["onfocus"];
fNames = [];
subEvents = {};

var elementID = 0;

function genElementID() {
  const ret = elementID;
  elementID++;
  return "elem_" + ret.toString();
}

function getUniqueID(handler,k) {
  const variantTag = handler["_label"];
  const evtName = handler["_value"]["1"];
  const genMsgFn = handler["_value"]["2"];

  if (evtName == "keydown" || evtName == "keyup") {
    var subID = evtName + genMsgFn.name;
  } else if (variantTag == "VirtualDom.TimeHandler") {
    var subID = "time" + genMsgFn.name;
  } else if (variantTag == "VirtualDom.TupleIntHandler") {
    var subID = "mousepos" + genMsgFn.name;
  } else if (variantTag == "VirtualDom.UnitHandler" && evtName == "animationFrame") {
    var subID = "animationframe" + genMsgFn.name;
  } else if (variantTag == "VirtualDom.UnitHandler") {
    var subID = "unit" + genMsgFn.name;
  }
  _applyCont(k, subID);
}

function setupSubscription(subscription) {
  if (subscription === undefined || subscription["_value"] === undefined) { return; }
  const cont = _makeCont(function(msg) {
    _Send(evtHandlerPid, msg);
  });

  const subID = subscription["_value"]["1"];
  const evtName = subscription["_value"]["2"]["_value"]["1"];
  const genMsgFn = subscription["_value"]["2"]["_value"]["2"];
  const variantTag = subscription["_value"]["2"]["_label"];
  console.log(variantTag);

  switch(variantTag) {
    case "VirtualDom.TimeHandler":
      subEvents[subID] = window.setInterval(function() {
          genMsgFn(cont);
        }, evtName);
      if (fNames.indexOf(genMsgFn.name) == -1) fNames.push(genMsgFn.name);
      break;
    case "VirtualDom.StringHandler":
        if (keyboardEvents.indexOf(evtName) != -1) {
            subEvents[subID] = function(event) {
              var keycode = event.keyCode.toString();
              genMsgFn(keycode, cont);
            };
            document.addEventListener(evtName, subEvents[subID]);
        }
        break;
    case "VirtualDom.UnitHandler":
        if (evtName == "animationFrame") {
          subEvents[subID] = function(event) {
            genMsgFn(cont);
            window.requestAnimationFrame(subEvents[subID]);
          };
          window.requestAnimationFrame(subEvents[subID]);
        }
        break;
    case "VirtualDom.TupleIntHandler":
        if (evtName == "mousemove") {
            subEvents[subID] = function(event) {
              genMsgFn({1:event.clientX,2:event.clientY}, cont);
            };
            document.addEventListener("mousemove", subEvents[subID]);
        }
      default:
        console.log("default");
    }
}

function removeSubscription(subscription) {
  if (subscription === undefined || subscription["_value"] === undefined) { return; }

  const subID = subscription["_value"]["1"];
  const evtName = subscription["_value"]["2"]["_value"]["1"];
  const variantTag = subscription["_value"]["2"]["_label"];

  switch(variantTag) {
    case "VirtualDom.TimeHandler":
      window.clearInterval(subEvents[subID]);
      break;
    case "VirtualDom.StringHandler":
        if (evtName == "mousemove") {
            document.removeEventListener("mousemove", subEvents[subID]);
        } else if (keyboardEvents.indexOf(evtName) != -1) {
            document.removeEventListener("keydown", subEvents[subID]);
        }
        break;
      default:
        console.log("default");
    }
}

function subKeyArray(subs) {
  var keyArr = [];
  for (var i = 0; i < subs.length; i++) {
    keyArr.push(subs[i]["_value"][1]);
  }
  return keyArr;
}

// returns [[a],[b]] where [a] is the list of subs in previous but not new,
// [b] is the list of subs in new but not previous.
function deadBornSubs(oldSubs, newSubs) {
  oldSubsKeys = subKeyArray(oldSubs);
  newSubsKeys = subKeyArray(newSubs);
  // console.log(oldSubsKeys);
  // console.log(newSubsKeys);

  function getDeadSubs() {
    var deadSubs = [];
    for (var i = 0; i < oldSubs.length; i++) {
      if (newSubsKeys.indexOf(oldSubs[i]["_value"][1]) == -1) {
        deadSubs.push(oldSubs[i]);
      }
    }
    return deadSubs;
  }
  function getBornSubs() {
    var bornSubs = [];
    for (var i = 0; i < newSubs.length; i++) {
      if (oldSubsKeys.indexOf(newSubs[i]["_value"][1]) == -1) {
        bornSubs.push(newSubs[i]);
      }
    }
    return bornSubs;
  }
  return [getDeadSubs(), getBornSubs()];
}

function diffSubscriptions(oldSubs, newSubs) {
  deadBorn = deadBornSubs(oldSubs, newSubs);
  deadSubs = deadBorn[0];
  bornSubs = deadBorn[1];

  for (var i = 0; i < bornSubs.length; i++) {
    setupSubscription(bornSubs[i]);
    console.log("Subscribed to " + bornSubs[i]["_value"][1]);
  }

  for (var i = 0; i < deadSubs.length; i++) {
    removeSubscription(deadSubs[i]);
    delete subEvents[deadSubs[i]["_value"][1]];
    console.log("Unsubscribed from " + deadSubs[i]["_value"][1]);
  }
}

function _updateSubscriptions(subs) {
  // console.log(Object.keys(subs[1]));
  // console.log(Object.keys(subs[2]));
  diffSubscriptions(LINKEDLIST.toArray(subs[1]), LINKEDLIST.toArray(subs[2]));
  console.log("Current subscriptions: " + Object.keys(subEvents))
  // console.log(subEvents);
  // console.log("---");
}

/* Turn pair representation of attributes to JS map */
function toAttrsArray(attrs) {
  var attrsArr = [];
  LINKEDLIST.forEach(attrs, function(attr) {
    _debug(attr["1"] + " : " + attr["2"]);
    attrsArr[attr["1"]] = attr["2"];
  });
  return attrsArr;
}

// Input: Array of Links variants representing the "shapes" of event
// handlers we can get, along with the name of the event, and callback functions in CPS form to generate a message
// Output: Array of objects of the form { evtName : f }, where f is a direct-style callback which
// produces a message and dispatches it to the event handler process
function setupEvtHandlers(attrs, evtHandlers) {
  if (evtHandlers === undefined || evtHandlers === null) { return; }

  function setupEvtHandler(handler) {
    const variantTag = handler["_label"];
    const evtName = handler["_value"]["1"];
    const genMsgFn = handler["_value"]["2"];
    // SJF TODO: Change this to AP-style communication rather than
    // MB-style communication
    const cont = _makeCont(function(msg) {
        _Send(evtHandlerPid, msg);
      });

    if (variantTag == "VirtualDom.UnitHandler") {
      attrs[evtName] = function() { genMsgFn(cont) };
    } else if (variantTag == "VirtualDom.StringHandler") {
      if (evtName == "keycode") {
        attrs["onkeydown"] = function(event) {
          var keycode = event.keyCode.toString();
          genMsgFn(keycode, cont);
        };
      } else if (keyboardEvents.indexOf(evtName) != -1) {
        // SJF TODO: IDs should be assigned automatically to all elements
        if (!attrs["id"]) {
            throw("Element with StringHandler event requires id attribute")
        }
        if (document.getElementById(attrs["id"]) != null) {
          inputText = document.getElementById(attrs["id"]).value;
        }  else {
          inputText = "";
        }
        attrs[evtName] = function() {
          if (document.getElementById(attrs["id"]) != null) {
            var inputText = document.getElementById(attrs["id"]).value;
          } else {
            var inputText = "";
          }
          genMsgFn(inputText, cont) };
      }
    } else {
      throw("Unsupported event handler form");
    }
  }

  LINKEDLIST.forEach(evtHandlers, function (hndlr) { setupEvtHandler(hndlr) });
  /*
  for (let i = 0; i < evtHandlers.length; i++) {
    setupEvtHandler(evtHandlers[i]);
  }
 */
}

function setupSubscriptions() {
  if (subscriptions === undefined) { return; }
  for (let i = 0; i < subscriptions.length; i++) {
    setupSubscription(subscriptions[i]);
  }
}

function jsonToVtree(jsonContent) {
  if (jsonContent["_label"] == "VirtualDom.DocTagNode") {
    var treeArr = [];
    var tagContent = jsonContent["_value"];
    var attrs = toAttrsArray(tagContent["attrs"]);
    // If there's no user-assigned ID (and realistically, there shouldn't be) --
    // assign a fresh one
    if (!attrs["id"]) {
      attrs["id"] = genElementID();
    }
    setupEvtHandlers(attrs, tagContent["eventHandlers"]);
    var children = tagContent["children"];
    LINKEDLIST.forEach(children, function(child) { treeArr.push(jsonToVtree(child)) });
    return h(tagContent["tagName"], attrs, treeArr);
  }
  if (jsonContent["_label"] == "VirtualDom.DocTextNode") {
    return [String(jsonContent["_value"])];
  }
}

function _runDom(str, doc, pid, subs) {
  subscriptions = LINKEDLIST.toArray(subs);
  evtHandlerPid = pid;
  currentVDom = jsonToVtree(doc);
  rootNode = createElement(currentVDom);
  document.getElementById(str).appendChild(rootNode);
  setupSubscriptions();
}

function _updateDom(doc) {
  var newTree = jsonToVtree(doc);
  var patches = diff(currentVDom, newTree);
  currentVDom = newTree;
  rootNode = patch(rootNode, patches);
}

// Magic, don't worry about these
var runDom = LINKS.kify(_runDom);
var updateDom = LINKS.kify(_updateDom);
var updateSubscriptions = LINKS.kify(_updateSubscriptions);
// var getUniqueID = LINKS.kify(_getUniqueID);

