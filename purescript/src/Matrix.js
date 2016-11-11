exports.newApi = function (rootUrl) {
  return function (secureUrl) {
    return function () {
      return new MatrixApi(rootUrl, secureUrl);
    };
  };
};

exports.userByName_ = function (api, name, ok, err) {
  return function () {
    api.User.byName(name).get
      ( function (v) { ok(v)(); }
      , function (e) { er(e)(); }
      );
  };
};

exports.tagList_ = function (api, ok, er) {
  return function () {
    api.Tag.list
      ( function (v) { ok(v)(); }
      , function (e) { er(e)(); }
      );
  };
};

exports.packageList_ = function (api, range, ok, err) {
  return function () {
    api.Package.list
      ( function (v) { ok(v)(); }
      , function (e) { er(e)(); }
      , fromRange(range)
      );
  };
};

exports.latestReportByPackageName_ = function (api, pkgName, ok, err) {
  return function () {
    api.Package.byName(pkgName).Report.latest().get
      ( function (v) { ok(v)(); }
      , function (e) { er(e)(); }
      );
  };
};

exports.packageByName_ = function (api, pkgName, ok, err) {
  return function () {
    api.Package.byName(pkgName).get
      ( function (v) { ok(v)(); }
      , function (e) { er(e)(); }
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

exports.getVersionedPackageName_ = function (uri) {
  debugger;
  var reg = /^\/package\/((?:[^\/\d-][^\/-]+)(?:-(?:[^\/\d-][^\/-]+))*)-([\d.]+)$/;
  return (reg.test(uri.path())
       && RegExp.$1 && RegExp.$2
       && { packageName : RegExp.$1, packageVersion : RegExp.$2 }
         ) || null;
};
