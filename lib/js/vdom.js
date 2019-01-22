/*
 * Virtual DOM integration.
 */
// Assumes virtual-dom.js has been loaded.

// Our current ****virtual-dom style**** representation of the VDom.

/* Global variables. */

// loaded: This module should only be loaded once per page; it is an error
// to load the page more than once. "loaded" is set to true when "runDom" is
// called for the first time.
let loaded = false;

// Current VDOM representation of the page. Initially undefined.
let currentVDom = undefined;

// Event handler AP, for event dispatch.
var evtHandlerAP = undefined;
// Running subscription ID.
var currentSubID = 0;

// Root VDom node.
var rootNode = undefined;

// Quick-access functions from the virtualDom JS module.
const h = virtualDom.h;
const diff = virtualDom.diff;
const patch = virtualDom.patch;
const createElement = virtualDom.create;

// Globals for subscriptions
const windowEvents = {}; // Event name |-> { key: Key, handler: function }
const intervalEvents = {}; // Key |-> Interval reference


// Running element ID.
let elementID = 0;

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


/*
 * evalSubscription: Takes a Links variant of type `MvuSubscriptions.Sub`
 * and returns an object detailing the new event handlers.
 *
 * Argument: `subscription`: a Links variant of type `MvuSubscriptions.Sub`
 * Result: { windowEvents: { eventName |-> {key: unique key, handler: handler} },
 *           intervalEvents: { key |-> {interval: int, handler: callback} },
 *           animationFrameHandler: either `null` or { key: unique key, handler: handler } }
 *
 * TODO: Update intervalEvents to the new representation.
 *
 * The "unique key" is the concatenation of all of the (sorted) Links
 * function names for the callbacks. This means that we only need to set / unset
 * event handlers when there is a change, and also supports duplicate invocations
 * of the same function per event.
 */
function evalSubscription(subscription) {

  // First, evaluate the subscription to a point where we have a list of
  // callbacks for each event, and a list of animation frame handlers.
  // We can then batch later.
  // Result: { windowEvents: { eventName: [callback] },
  //           intervalEvents: [{ interval: int, handler: callback }],
  //           animationFrameHandlers: [callback] }
  function evalSubscriptionInner(subscription) {
    const tag = subscription["_label"];
    const val = subscription["_value"];

    function makeRes(windowEvents, intervalEvents, animationFrames) {
      return
        { windowEvents: windowEvents,
          intervalEvents: intervalEvents,
          animationFrameHandlers: animationFrames };
    }

    if (tag == "MvuSubscriptions.SubEmpty") {
      return makeRes([], [], []);
    } else if (tag == "MvuSubscriptions.SubAppend") {
      const res1 = evalSubscriptionInner(val["1"]);
      const res2 = evalSubscriptionInner(val["2"]);
      return makeRes(
        res1.windowEvents.concat(res2.windowEvents),
        res1.intervalEvents.concat(res2.intervalEvents),
        res1.animationFrameHandlers.concat(res2.animationFrameHandlers));
    } else if (tag == "MvuSubscriptions.SubEventHandler") {
      const evtTag = val["_label"];
      const handler = val["_value"];

      // Now, case split on the event handler tag.
      if (evtTag == "MvuEvents.UnitHandler") {
        const evtName = handler["1"];
        const genMsgFn = handler["2"];
        const retEvents = {};
        retEvents[evtName] = [function() { genMsgFn(unconditionalDispatch) }];
        return makeRes(retEvents, [], []);
      } else if (variantTag == "MvuEvents.EventDataHandler") {
        const evtName = handler["1"];
        const evtPropertyName = handler["2"];
        const genMsgFn = handler["3"];
        const fn = function(evt) {
            const pv = evt[evtPropertyName];
            let propVal = "";
            if(pv != undefined) { propVal = pv.toString(); }
            genMsgFn(propVal, conditionalDispatch)
          }
        );
        const retEvents = {};
        retEvents[evtName] = [fn];
        return makeRes(retEvents, [], []);

      // FIXME: TupleIntHandler is a hack left over from the original implementation.
      // It only makes sense for mouse events. The Right Way To Do It would be
      // to encode the Keyboard / Mouse events as data types in their own
      // right, and have KeyboardEventHandler and MouseEventHandler types.
      } else if (variantTag == "MvuEvents.TupleIntHandler") {
        const evtName = handler["1"];
        const genMsgFn = handler["2"]
        if (!isMouseEvent(evtName)) {
          throw("TupleIntHandler only defined for mouse events.");
        }

        const retEvents = {};
        const fn =
          function(evt) {
            const arg = {};
            arg["1"] = evt.mouseX;
            arg["2"] = evt.mouseY;
            genMsgFun(arg, unconditionalDispatch)
          });
        retEvents[evtName] = [fn];
        return makeRes(retEvents, [], []);
      } else {
        // PropertyHandlers not supported for subscriptions.
        throw("Unsupported event handler " + variantTag);
      }
    } else if (tag == "MvuSubscriptions.SubInterval") {
      const interval = val["1"];
      const handler = val["2"];
      return makeRes([], [{interval: interval, handler: handler}], []);
    } else if (tag == "MvuSubscriptions.SubAnimationFrame") {
      return makeRes([], [], [val]);
    } else {
      throw("Unsupported subscription type: " + tag)
    }
  }

  const res = evalSubscriptionInner(subscription);
  // windowEvents: Batched window events, complete with unique key comprising
  // the different Links function names, and the callback which invokes all of
  // them.
  const windowEvents = {};
  Object.keys(res.windowEvents).forEach(function(eventName) {
    // Key: names of all functions
    let key = "subsKey_" + eventName + "_" +
      res.windowEvents[eventName]
        .map((fn) => fn.name)
        .sort()
        .join("");

    debug("Event subscription key (" + eventName + "): " + key);

    windowEvents[eventName] =
    { key: key,
      handler:
        function(evt) {
          eventCallbacks[eventName].forEach(function(f) { f(evt); })
        }
    };
  });

  let animationFrameHandler = null;
  if (res.animationFrameHandlers.length > 0) {
    let afkey = "subsKeyAnim_" +
      res.animationFrameHandlers
        .map((fun) => fn.name)
        .sort()
        .join("");
    debug("AF subscription key: " + key);

    animationFrameHandler =
    { key: key,
      handler: function(timestamp) {
        res.animationFrameHandlers.forEach((f) => f(timestamp));
      }
    }
  }

  return { windowEvents: windowEvents,
           intervalEvents: res.intervalEvents,
           animationFrameHandler: animationFrameHandler };
}

const SubscriptionCommand =
  Object.freeze({
    UNSET_INTERVAL: "UNSET_INTERVAL",
    SET_INTERVAL: "SET_INTERVAL",
    ANIMATION_FRAME: "ANIMATION_FRAME",
    UNSET_WINDOW_EVENT: "UNSET_WINDOW_EVENT",
    SET_WINDOW_EVENT: "SET_WINDOW_EVENT"});

/* Diffs subscriptions, producing a list of commands to enact on the DOM.
 * Inputs: subscription datatypes produced by `evalSubscriptions`.
 * Output: A small list of instructions:
 *  [
 *      { command: SubscriptionCommand.UNSET_INTERVAL, key: unique interval key }
 *    | { command: SubscriptionCommand.SET_INTERVAL, key: UIK, interval: int, handler: callback function }
 *    | { command: SubscriptionCommand.ANIMATION_FRAME, handler: callback function }
 *    | { command: SubscriptionCommand.UNSET_WINDOW_EVENT, eventName: event name }
 *    | { command: SubscriptionCommand.SET_WINDOW_EVENT, eventName: event name,
 *          key: unique event key, handler: fn }
 *  ]
 * */
function diffSubscriptions(oldSubscriptions, newSubscriptions) {
  const commands = [];

  function setWindowEvent(eventName, key, handler) {
    commands.push({ command: SubscriptionCommand.SET_WINDOW_EVENT,
      eventName: eventName, key: key, handler: handler });
  }

  function unsetWindowEvent(eventName) {
    commands.push({ command: SubscriptionCommand.UNSET_WINDOW_EVENT,
      eventName: eventName });
  }

  function setNewInterval(key, interval, handler) {
    commands.push({ command: SubscriptionCommand.SET_INTERVAL,
      key: key, interval: interval, handler: handler });
  }

  function unsetOldInterval(key) {
    commands.push({ command: SubscriptionCommand.UNSET_INTERVAL, key: key });
  }

  // First, handle window event diffs
  Object.keys(newSubscriptions.windowEvents).forEach((evtName) =>
    const key = newSubscriptions.windowEvents.key;
    const handler = newSubscriptions.windowEvents.handler;
    // Check whether we have an event handler for evtName
    if (evtName in windowEvents) {
      // Only update if key has changed.
      if (windowEvents[evtName].key != key) {
        unsetWindowEvent(evtName);
        setWindowEvent(evtName, key, handler);
      }
    } else {
      // If not, we need to setup the new event handler
      setWindowEvent(evtName, key, handler);
    }
  );
  // -- We also need to remove any event handlers which appear in
  //    oldSubscriptions but not newSubscriptions
  const oldEvtSet = new Set(Object.keys(oldSubscriptions.windowEvents));
  const newEvtSet = new Set(Object.keys(newSubscriptions.windowEvents));
  oldEvtSet.filter((evt) => !newEvtSet.has(evt))
           .forEach((evt) => unsetWindowEvent(evt));

  // Second, handle interval event diffs
  Object.keys(newSubscriptions.intervalEvents).forEach((key) =>
    const interval = newSubscriptions.intervalEvents[key].interval;
    const handler = newSubscriptions.intervalEvents[key].handler;
    if (!(key in oldSubscriptions.intervalEvents)) {
      setNewInterval(key, interval, handler);
    }
  );

  const oldIntervalSet = new Set(Object.keys(oldSubscriptions.intervalEvents));
  const newIntervalSet = new Set(Object.keys(newSubscriptions.intervalEvents));
  oldIntervalSet.filter((key) => !newIntervalSet.has(key))
                .forEach((key) => unsetOldInterval(key));

  // Third, add a command to do an animation frame, if necessary.
  const afh = newSubscriptions.animationFrameHandler;
  if (afh != null) {
    commands.push(
      { command: SubscriptionCommand.ANIMATION_FRAME,
        handler: afh.handler });
  }

  return commands;
}

/* Interprets the commands produced by `diffSubscriptions`. */
function interpretSubscriptionCommands(cmds) {
  cmds.forEach((cmd) =>
    if (cmd.command == SubscriptionCommand.UNSET_INTERVAL) {
      const key = cmd.key;
      if (key in intervalEvents) {
        clearInterval(intervalEvents[key]);
        delete intervalEvents[key];
      } else {
        debug("WARN: Tried to delete invalid interval with key " + key);
      }
    } else if (cmd.command == SubscriptionCommand.SET_INTERVAL) {
      intervalEvents[key] = setInterval(cmd.interval, cmd.handler);
    } else if (cmd.command == SubscriptionCommand.ANIMATION_FRAME) {
      window.requestAnimationFrame(cmd.handler);
    } else if (cmd.command == SubscriptionCommand.UNSET_WINDOW_EVENT) {
      if (eventName in windowEvents) {
        window.removeEventListener(windowEvents[eventName].handler);
      } else {
        debug("WARN: Tried to delete nonexistent event handler for " + eventName);
      }
    } else if (cmd.command == SubscriptionCommand.SET_WINDOW_EVENT) {
      window.addEventListener(cmd.eventName, cmd.handler);
      windowEvents[eventName] = { key: cmd.key, handler: cmd.handler };
    } else {
      throw("Invalid subscription command: " + cmd.command);
    }
  );
}



function setupSubscription(subscription) {
  if (subscription === undefined || subscription["_value"] === undefined) { return; }



// Old stuff.
// const subID = subscription["_value"]["1"];
// const variantTag = subscription["_value"]["2"]["_label"];
// const evtName = subscription["_value"]["2"]["_value"]["1"];
// console.log(variantTag);
// let genMsgFn = undefined;
//
// switch(variantTag) {
//     case "VirtualDom.TimeHandler":
//       genMsgFn = subscription["_value"]["2"]["_value"]["2"];
//       subEvents[subID] = window.setInterval(function() {
//           genMsgFn(unconditionalDispatch);
//         }
//         , evtName);
//       if (fNames.indexOf(genMsgFn.name) == -1) fNames.push(genMsgFn.name);
//       break;
//     case "VirtualDom.EventDataHandler":
//       const evtKey = subscription["_value"]["2"]["_value"]["2"];
//       genMsgFn = subscription["_value"]["2"]["_value"]["3"];
//       subEvents[subID] = function(evt) {
//         const evtProp = evt[evtKey].toString();
//         let arg = "";
//         if (evtProp != undefined) {
//           arg = evtProp;
//         }
//         genMsgFn(arg, conditionalDispatch);
//       };
//       document.addEventListener(evtName, subEvents[subID]);
//       break;
//     case "VirtualDom.UnitHandler":
//       genMsgFn = subscription["_value"]["2"]["_value"]["2"];
//       if (evtName == "animationFrame") {
//         subEvents[subID] = function(event) {
//           genMsgFn(unconditionalDispatch);
//           window.requestAnimationFrame(subEvents[subID]);
//         };
//         window.requestAnimationFrame(subEvents[subID]);
//       }
//       break;
//     case "VirtualDom.TupleIntHandler":
//       genMsgFn = subscription["_value"]["2"]["_value"]["2"];
//       if (evtName == "mousemove") {
//           subEvents[subID] = function(event) {
//             genMsgFn({1:event.clientX,2:event.clientY}, unconditionalDispatch);
//           };
//           document.addEventListener("mousemove", subEvents[subID]);
//       }
//       break;
//     default:
//       console.log("default");
//   }
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

/*
function _updateSubscriptions(subs) {
  // console.log(Object.keys(subs[1]));
  // console.log(Object.keys(subs[2]));
  diffSubscriptions(LINKEDLIST.toArray(subs[1]), LINKEDLIST.toArray(subs[2]));
  console.log("Current subscriptions: " + Object.keys(subEvents))
  // console.log(subEvents);
  // console.log("---");
}
*/

function _updateSubscriptions(subs) {
  // Not (re)-implemented yet.
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
/* Some event handlers unconditionally dispatch the message.
 * Others may choose not to dispatch a message upon invocation.
 * These two helpers encode these two modes of dispatch. */
const unconditionalDispatch = _makeCont(dispatchMessage);
const conditionalDispatch =
  _makeCont(function(fnRes) {
      if(fnRes["_label"] == "Just") {
        dispatchMessage(fnRes["_value"]);
      }
  });


/* evalEventHandlers: Generates JS event handlers from Links event handlers
 * for a given element.
 *
 * Note that we are taking an *array* of Links event handlers for the element
 * as an input, as opposed to a single EH, in order to batch them correctly.
 *
 * Input:
 *   elementID: ID of the element to which the event handlers will be attached.
 *   evtHandler: JS Array of Links variants of type MvuEvent.EventHandler.
 *
 * Output: Array of objects of the form { evtName : f }, where f is a
 * direct-style callback which produces a message and dispatches it to
 * the event handler process.
 */
function setupEventHandlers(elementID, evtHandlers) {
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

  function setupEventHandler(handler) {
    const variantTag = handler["_label"];

    if (variantTag == "MvuEvents.UnitHandler") {
      const evtName = handler["_value"]["1"];
      const genMsgFn = handler["_value"]["2"];
      addCallback(evtName, function() { genMsgFn(unconditionalDispatch) });
    } else if (variantTag == "MvuEvents.PropertyHandler") {
      const evtName = handler["_value"]["1"];
      const propName = handler["_value"]["2"];
      const genMsgFn = handler["_value"]["3"];

      addCallback(evtName, function() {
          let propVal = "";
          if (document.getElementById(elementID) != null) {
            const pv = document.getElementById(elementID)[propName];
            if(pv != undefined) {
              propVal = pv;
            }
          }
          genMsgFn(propVal, conditionalDispatch)
        }
      );
    } else if (variantTag == "MvuEvents.EventDataHandler") {
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
  evtHandlers.forEach(function (hndlr) { setupEventHandler(hndlr) });

  // Second, assign to each event a callback which invokes all event handlers
  const ret = {};
  Object.keys(eventCallbacks).forEach(function(eventName) {
    ret[eventName] = function(evt) {
      eventCallbacks[eventName].forEach(function(f) {
        f(evt);
      })
    }
  });
  return ret;
}

function setupSubscriptions(subscriptions) {
  if (subscriptions === undefined) { return; }
  for (let i = 0; i < subscriptions.length; i++) {
    setupSubscription(subscriptions[i]);
  }
}

/**
 * evalAttr: Evaluates a Links MvuAttrs.Attr, resulting in a list of
 * plain HTML attributes and event handlers to install.
 *
 * Argument: `attr`, a Links variant of type "MvuAttrs.Attr".
 * Result: An object of the form
 *   { attributes: {key: value},
 *     eventHandlers: [MvuEvent.EventHandler] }
 **/
function evalAttr(attr) {
  const lbl = attr["_label"]
  const val = attr["_value"]

  function makeRes(attrs, handlers) {
    return { attributes: attrs, eventHandlers: handlers }
  }

  if (lbl == "MvuAttrs.AttrEmpty") {
    // Nothing doing.
    return makeRes([], []);
  } else if (lbl == "MvuAttrs.AttrAppend") {
    // Recursively evaluate both.
    const a1 = val["1"];
    const a2 = val["2"];
    const a1Res = evalAttr(a1);
    const a2Res = evalAttr(a2);
    return makeRes(
      Object.assign(a1Res.attributes, a2Res.attributes),
      a1Res.eventHandlers.concat(a2Res.eventHandlers));
  } else if (lbl == "MvuAttrs.AttrAttribute") {
    const attr = {};
    attr[val["1"]] = val["2"];
    return makeRes(attr, []);
  } else if (lbl == "MvuAttrs.AttrEventHandler") {
    return makeRes([], [val])
  } else {
    throw("Unknown attribute type: " + lbl);
  }
}


/**
 * evalHTML: Evaluates a Links MvuHTML.HTML to a VDom element.
 * Argument: A Links MvuHTML.HTML variant.
 * Result: A list of child VDom nodes.
 **/
function evalHTML(html) {
  const lbl = html["_label"];
  const val = html["_value"];

  if (lbl == "MvuHTML.HTMLEmpty") {
    return [];
  } else if (lbl =="MvuHTML.HTMLAppend") {
    return evalHTML(val["1"]).concat(evalHTML(val["2"]));
  } else if (lbl == "MvuHTML.HTMLText") {
    return [String(val)];
  } else if (lbl == "MvuHTML.HTMLTag") {
    // First, get ourselves a dictionary of attributes and event handlers
    const attrRes = evalAttr(val.attrs);
    const attrs = attrRes.attributes;
    const evtHandlers = attrRes.eventHandlers;

    // If there's no user-assigned ID, assign a fresh one
    if (!attrs["id"]) {
      attrs["id"] = genElementID();
    }

    const evtHandlerAttrs = setupEventHandlers(attrs["id"], evtHandlers);
    // Add the event handlers to the generated attributes.
    const combinedAttrs = Object.assign(attrs, evtHandlerAttrs);
    const children = evalHTML(val.children);
    return [h(val["tagName"], combinedAttrs, children)];
  } else {
    throw("Unsupported HTML type: " + lbl);
  }
}

// Top-level node must be a tree instead of a forest, so wrap it in a div.
function evalToplevelHTML(html) {
  return h("div", [], evalHTML(html));
}

function _runDom(str, doc, ap, subs) {
  evtHandlerAP = ap;
  currentVDom = evalToplevelHTML(doc);
  rootNode = createElement(currentVDom);
  document.getElementById(str).appendChild(rootNode);
  setupSubscriptions(subs);
}

function _updateDom(doc) {
  var newTree = evalToplevelHTML(doc);
  var patches = diff(currentVDom, newTree);
  currentVDom = newTree;
  rootNode = patch(rootNode, patches);
}

// Wrappers to make direct-style functions callable from the FFI.
var runDom = LINKS.kify(_runDom);
var updateDom = LINKS.kify(_updateDom);
var updateSubscriptions = LINKS.kify(_updateSubscriptions);

