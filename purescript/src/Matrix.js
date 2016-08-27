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
    api.Tag.list( function (v) { ok(v)(); }
                , function (e) { er(e)(); }
                );
  };
};

exports.packageList = function (api, count, offset, ok, err) {
  return function () {
    var params = {};
    if (typeof count.value0 === "number") {
      params.count = count.value0;
    }
    if (typeof offset.value0 === "number") {
      params.offset = offset;
    }
    api.Package.list
      ( function (v) { ok(v)(); }
      , function (e) { er(e)(); }
      , params
      );
  };
};
