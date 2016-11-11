exports.onPopstate = function (cb) {
  return function () {
    return $(window).on("popstate", cb);
  };
};

exports.delegate2 = function (_sel1) {
  return function (_sel2) {
    return function (el) {
      return function (sel) {
        return function (eventName) {
          return function (cb) {
            return function () {
              return $(el).delegate
                ( sel
                , eventName
                , function (e) { cb(e)(); }
                );
            };
          };
        };
      };
    };
  };
};

exports.delegate_ = function (el,  sel, eventName, cb) {
  debugger;
  return function () {
    return $(el).delegate(sel, eventName, cb);
  };
};

exports.target = function (jqEvent) { return function () { return jqEvent.target; }; };
exports.altKey = function (jqEvent) { return function () { return jqEvent.altKey; }; };
exports.ctrlKey = function (jqEvent) { return function () { return jqEvent.ctrlKey; }; };
exports.shiftKey = function (jqEvent) { return function () { return jqEvent.shiftKey; }; };
exports.metaKey = function (jqEvent) { return function () { return jqEvent.metaKey; }; };
exports.shiftKey = function (jqEvent) { return function () { return jqEvent.shiftKey; }; };
exports.which = function (jqEvent) { return function () { return jqEvent.which; }; };
exports.getAttr = function (attr) { return function (jq) { return function () { return jq.attr(attr); }; }; };

exports.unsafeLog = function (a) {
  return function () {
    console.log(a);
  };
};

exports.delay = function (f) {
  return function () {
    setTimeout(f, 0);
  };
};

exports.historyPushState_ = function (title, uri) {
  return function () {
    window.history.pushState(null, title, uri.toString());
  }
};

exports.historyReplaceState_ = function (title, uri) {
  return function () {
    window.history.replaceState(null, title, uri.toString());
  }
};

exports.setDocumentTitle = function (title) {
  return function () {
    window.document.title = title;
  };
};

exports.autocomplete_ = function (jq, source, select) {
  console.log("autocomplete_");
  return function () {
    console.log("autocomplete_()");
    jq.autocomplete
      ( { source : source
        , select : function (_, v) { console.log("autocomplete select", v, select); select(v)(); }
        }
      )
  };
};
