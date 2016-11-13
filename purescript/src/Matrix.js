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

exports.packageByName_ = function (api, pkgName, ok, err, normal, unPreferred, deprecated) {
  return function () {
    api.Package.byName(pkgName).get
      ( function (v) {
        v.versions = v.versions.map(function (w) {
          return (
            { version  : w.version
            , revision : w.revision
            , preference
              : w.preference == "normal" ? normal
              : w.preference == "unPreferred" ? unPreferred
              : w.preference == "deprecated" ? deprecated
              : (function () { throw new Error("Unexpected preference: " + JSON.stringify(w.preference)) })()
            });
        })
        ok(v)();
      }
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
  var reg = /^\/package\/((?:[^\/\d-][^\/-]+)(?:-(?:[^\/\d-][^\/-]+))*)-([\d.]+)$/;
  var m = uri.path().match(reg)
  console.log("getVersionedPackageName", uri.path(), m, m && m[1], m && m[2])
  return (m
       && m[1] && m[2]
       && { packageName : m[1], packageVersion : m[2] }
         ) || null;
};

exports.getPackageName_ = function (uri) {
  var reg = /^\/package\/((?:[^\/\d-][^\/-]+)(?:-(?:[^\/\d-][^\/-]+))*)$/
  var m = uri.path().match(reg)
  console.log("getPackageName", uri.path(), m, m && m[1])
  return (m && m[1]) || null;
};
