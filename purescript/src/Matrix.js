// module MatrixApi

exports.newApi = function (rootUrl) {
  return function (secureUrl) {
    return function () {
      return new MatrixApi(rootUrl, secureUrl);
    };
  };
};

exports.userByName = function (api) {
  return function (name) {
    console.log("a");
    return function (ok) {
      console.log("b");
      return function (err) {
        console.log("c");
        return function () {
          console.log("Run user byname");
          api.User.byName(name).get( function (v) { ok(v)(); }
                                   , function (e) { er(e)(); }
                                   );
        };
      };
    };
  };
};

exports.tagList = function (api, ok, er) {
  return function () {
    api.Tag.list
      ( function (v) { ok(v)(); }
      , function (e) { er(e)(); }
      );
  };
};

exports.packageList = function (api, range, ok, err) {
  return function () {
    api.Package.list
      ( function (v) { ok(v)(); }
      , function (e) { er(e)(); }
      , fromRange(range)
      );
  };
};

function fromRange (range) {
  var params = {};
  if (typeof range.count.value0 === "number") {
    params.count = range.count.value0;
  }
  if (typeof range.offset.value0 === "number") {
    params.offset = range.offset.value0;
  }
  return params;
}
