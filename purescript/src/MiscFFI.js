exports.onPopstate = function (cb) {
  return function () {
    return $(window).on("popstate", cb);
  };
};

exports.delegate_  = function (el,  sel, eventName, cb) {
//  return function () {
//    return $(el).delegate(sel, eventName, cb);
//  };
};

exports.eventTarget = function (jqEvent) { return function () { return jqEvent.eventTarget; }; };
exports.altKey = function (jqEvent) { return function () { return jqEvent.altKey; }; };
exports.ctrlKey = function (jqEvent) { return function () { return jqEvent.ctrlKey; }; };
exports.shiftKey = function (jqEvent) { return function () { return jqEvent.shiftKey; }; };
exports.metaKey = function (jqEvent) { return function () { return jqEvent.metaKey; }; };
exports.shiftKey = function (jqEvent) { return function () { return jqEvent.shiftKey; }; };
exports.which = function (jqEvent) { return function () { return jqEvent.which; }; };
exports.getAttr = function (attr) { return function (jq) { return function () { return jq.attr(attr); }; }; };
