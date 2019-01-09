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
//var evtHandlerPid = undefined;
var evtHandlerAP = undefined;
var inputText = "";
var currentSubID = 0;
var subscriptions = {};
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
  let subID = "";

// SJF TODO: I wonder whether we need this logic, or there's a better way of doing it.
  if (variantTag == "VirtualDom.EventDataHandler") {
    const attrName = handler["_value"]["2"];
    const genMsgFn = handler["_value"]["3"];
    subID = evtName + attrName + genMsgFn.name;
  } else if (variantTag == "VirtualDom.TimeHandler") {
    const genMsgFn = handler["_value"]["2"];
    subID = "time" + genMsgFn.name;
  } else if (variantTag == "VirtualDom.TupleIntHandler") {
    const genMsgFn = handler["_value"]["2"];
    subID = "mousepos" + genMsgFn.name;
  } else if (variantTag == "VirtualDom.UnitHandler" && evtName == "animationFrame") {
    const genMsgFn = handler["_value"]["2"];
    subID = "animationframe" + genMsgFn.name;
  } else if (variantTag == "VirtualDom.UnitHandler") {
    const genMsgFn = handler["_value"]["2"];
    subID = "unit" + genMsgFn.name;
  } else {
    throw("Unsupported subscription")
  }
  _applyCont(k, subID);
}

function setupSubscription(subscription) {
  if (subscription === undefined || subscription["_value"] === undefined) { return; }
  const unconditionalDispatch = _makeCont(dispatchMessage);
  const conditionalDispatch =
    _makeCont(function(fnRes) {
        if(fnRes["_label"] == "Just") {
          dispatchMessage(fnRes["_value"]);
        }
    });
  const subID = subscription["_value"]["1"];
  const variantTag = subscription["_value"]["2"]["_label"];
  const evtName = subscription["_value"]["2"]["_value"]["1"];
  console.log(variantTag);
  let genMsgFn = undefined; // Ugly, but necessary since switch cases aren't block-scoped.

  switch(variantTag) {
      case "VirtualDom.TimeHandler":
        genMsgFn = subscription["_value"]["2"]["_value"]["2"];
        subEvents[subID] = window.setInterval(function() {
            genMsgFn(unconditionalDispatch);
          }, evtName);
        if (fNames.indexOf(genMsgFn.name) == -1) fNames.push(genMsgFn.name);
        break;
      case "VirtualDom.EventDataHandler":
        const evtKey = subscription["_value"]["2"]["_value"]["2"];
        genMsgFn = subscription["_value"]["2"]["_value"]["3"];
        subEvents[subID] = function(evt) {
          const evtProp = evt[evtKey].toString();
          let arg = "";
          if (evtProp != undefined) {
            arg = evtProp;
          }
          genMsgFn(arg, conditionalDispatch);
        };
        document.addEventListener(evtName, subEvents[subID]);
        break;
      case "VirtualDom.UnitHandler":
        genMsgFn = subscription["_value"]["2"]["_value"]["2"];
        if (evtName == "animationFrame") {
          subEvents[subID] = function(event) {
            genMsgFn(unconditionalDispatch);
            window.requestAnimationFrame(subEvents[subID]);
          };
          window.requestAnimationFrame(subEvents[subID]);
        }
        break;
      case "VirtualDom.TupleIntHandler":
        genMsgFn = subscription["_value"]["2"]["_value"]["2"];
        if (evtName == "mousemove") {
            subEvents[subID] = function(event) {
              genMsgFn({1:event.clientX,2:event.clientY}, unconditionalDispatch);
            };
            document.addEventListener("mousemove", subEvents[subID]);
        }
        break;
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
    case "VirtualDom.EventDataHandler":
        if (subID in subEvents) {
          document.removeEventListener(evtName, subEvents[subID]);
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

/*
 * Dispatches a message to the event handler.
*/
function dispatchMessage(msg) {
  // OLD: _Send(evtHandlerPid, msg);
  // NEW (but inefficient...):
  //   1. Request on AP, set in global variable
  //   2. Send message on returned channel
  //   3. Close buffer (BUG: this does not delete the peer buffer yet.)
  _spawn(function() {
      request(evtHandlerAP, _makeCont(function(c) {
        delete _buffers[c._sessEP2];
        _send(msg, c);
      }))
  });
}

// Input: Array of Links variants representing the "shapes" of event
// handlers we can get, along with the name of the event, and callback functions in CPS form to generate a message
// Output: Array of objects of the form { evtName : f }, where f is a direct-style callback which
// produces a message and dispatches it to the event handler process
function setupEvtHandlers(attrs, evtHandlers) {
  if (evtHandlers === undefined || evtHandlers === null) { return; }
  // Event name |-> [Callback] mapping
  const eventCallbacks = {};
  function addCallback(evtName, callback) {
    if (evtName in eventCallbacks) {
      eventCallbacks[evtName].unshift(callback);
    } else {
      eventCallbacks[evtName] = [callback];
    }
  }

  // Some handlers (conditional) only dispatch a message if a condition is met.
  // Others (unconditional) always dispatch a message.
  const unconditionalDispatch = _makeCont(dispatchMessage);
  const conditionalDispatch =
    _makeCont(function(fnRes) {
        if(fnRes["_label"] == "Just") {
          dispatchMessage(fnRes["_value"]);
        }
    });

  function setupEvtHandler(handler) {
    const variantTag = handler["_label"];

    if (variantTag == "VirtualDom.UnitHandler") {
      const evtName = handler["_value"]["1"];
      const genMsgFn = handler["_value"]["2"];
      const cont = _makeCont(dispatchMessage);
      addCallback(evtName, function() { genMsgFn(unconditionalDispatch) });
    } else if (variantTag == "VirtualDom.PropertyHandler") {
      const evtName = handler["_value"]["1"];
      const propName = handler["_value"]["2"];
      const genMsgFn = handler["_value"]["3"];

      addCallback(evtName, function() {
          let propVal = "";
          if (document.getElementById(attrs["id"]) != null) {
            const pv = document.getElementById(attrs["id"])[propName];
            if(pv != undefined) {
              propVal = pv;
            }
          }
          genMsgFn(propVal, conditionalDispatch)
        }
      );
    } else if (variantTag == "VirtualDom.EventDataHandler") {
      const evtName = handler["_value"]["1"];
      const evtPropertyName = handler["_value"]["2"];
      const genMsgFn = handler["_value"]["3"];
      addCallback(evtName, function(evt) {
          const pv = evt[evtPropertyName];
          let propVal = "";
          if(pv != undefined) { propVal = pv.toString(); }
          genMsgFn(propVal, conditionalDispatch)
        }
      );
    } else {
      throw("Unsupported event handler " + variantTag);
    }
  }
  // First, populate the "eventCallbacks" object
  LINKEDLIST.forEach(evtHandlers, function (hndlr) { setupEvtHandler(hndlr) });
  // Second, assign to each event a callback which invokes all event handlers
  Object.keys(eventCallbacks).forEach(function(eventName) {
    attrs[eventName] = function(evt) {
      eventCallbacks[eventName].forEach(function(f) {
        f(evt);
      })
    }
  });
}

function setupSubscriptions() {
  if (subscriptions === undefined) { return; }
  for (let i = 0; i < subscriptions.length; i++) {
    setupSubscription(subscriptions[i]);
  }
}

function jsonToVtree(jsonContent) {
  if (jsonContent["_label"] == "VirtualDom.HTMLTagNode") {
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
  if (jsonContent["_label"] == "VirtualDom.HTMLTextNode") {
    return [String(jsonContent["_value"])];
  }
}

function _runDom(str, doc, ap, subs) {
  subscriptions = LINKEDLIST.toArray(subs);
  evtHandlerAP = ap;
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

