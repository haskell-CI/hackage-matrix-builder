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

exports.queueByName_ = function (api, pkgName, ok, err, low, medium, high, just, nothing) {
  return function () {
    api.Queue.byName(pkgName).get
      ( function (v) {
        var qi = (
          { packageName : v.packageName
          , modified : v.modified
          , priority
            : v.priority === "low" ? low
            : v.priority === "medium" ? medium
            : v.priority === "high" ? high
            : (function () { throw new Error("Unexpected Priority: " + JSON.stringify(r)) })()
          }
        );
        ok(just(qi))();
      }
      , function (e) {
        if (e && e.responseJSON && e.responseJSON.notFound)
          ok(nothing)();
        else
          err(e)();
      });

  };
};

exports.queueCreate_ = function (api, pkgName, prio, ok, err) {
  return function () {
    api.Queue.create
      ( { packageName : pkgName, priority : prio }
      , function (v) { ok(v)(); }
      , function (e) { er(e)(); }
      );
  };
};


exports.tagByName_ = function (api, tagName, ok, err, just, nothing) {
  return function () {
    api.Tag.byName(tagName).get
      ( function (v) {
        var tg = (
          { name : v.name
          , packages : v.packages
          }
        );
        ok(just(tg))();
      }
      , function (e) {
        if (e && e.responseJSON && e.responseJSON.notFound)
          ok(nothing)();
        else
          err(e)();
      });

  };
};

exports.tagSaveByName_ = function (api, pkgName, tag, ok, err) {
  return function () {
    api.Tag.saveByName
      ( pkgName
      , { name : tag.name, packages : tag.packages }
      , function (v) { ok(v)(); }
      , function (e) { err(e)(); }
      );
  }
}
        
exports.tagRemove_ = function (api, pkgName, tag, ok, err) {
  return function () {
    api.Tag.byName(pkgName).remove
      ( { name : tag.name, packages : tag.packages }
      , function (v) { ok(v)(); }
      , function (e) { err(e)(); }
      );
  }
}

exports.packageTags_ = function (api, pkgName, tagName, ok, err) {
  return function () {
    api.Package.byName(pkgName).tags
      ( function (v) { ok(v)(); }
      , function (e) { er(e)(); }
      , tagName
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

exports.singleResult_ =
  function
    ( api, pkgName, cell
    , succ, err
    , nothing, just
    , ok, nop, noIp, noIpBjLimit, noIpFail, fail, failDeps
    ) {
  api.Package.byName(pkgName).Report.latest().Cell.byId(cell).get
    ( function (v) {
        var sr = (
          { ghcVersion : v.ghcVersion
          , ghcFullVersion : v.ghcFullVersion
          , resultA : !v.resultA
            ? nothing
            : just((function (vr) {
                var r = vr.result
                return (
                  { packageVersion : vr.packageVersion
                  , packageRevesion : vr.packageRevision
                  , result
                    : "ok" in r ? ok
                    : "nop" in r ? nop
                    : "noIp" in r ? noIp
                    : "noIpBjLimit" in r ? noIpBjLimit(r.noIpBjLimit)
                    : "noIpFail" in r ? noIpFail({ err : r.noIpFail.err, out : r.noIpFail.out })
                    : "fail" in r ? fail(r.fail)
                    : "failDeps" in r ? failDeps(r.failDeps)
                    : (function () { throw new Error("Unexpected ShallowVersionResult: " + JSON.stringify(r)) })()
                  }
                );
              }
            )(v.resultA))
          }
        );
        console.log("sr", sr);
        succ(sr)();
      }
    , function (e) { err(e)(); }
    )
};

exports.latestReportByPackageName_ =
  function
    ( api, pkgName
    , okF, err
    , ok, nop, noIp, noIpBjLimit, noIpFail, fail, failDeps
    ) {
  return function () {
    api.Package.byName(pkgName).Report.latest().get
      ( function (shallowReport) {
          var v = (
            { packageName : shallowReport.packageName
            , modified    : shallowReport.modified
            , results     : shallowReport.results.map
              ( function (shallowGhcResult) {
                  return(
                    { ghcVersion : shallowGhcResult.ghcVersion
                    , ghcFullVersion : shallowGhcResult.ghcFullVersion
                    , ghcResult : shallowGhcResult.ghcResult.map
                      ( function (shallowVersionResult) {
                        var r = shallowVersionResult.result;
                        return (
                          { packageVersion : shallowVersionResult.packageVersion
                          , packageRevision : shallowVersionResult.packageRevision
                          , result
                            : r.ok ? ok
                            : r.nop ? nop
                            : r.noIp ? noIp
                            : r.noIpBjLimit ? noIpBjLimit(r.noIpBjLimit)
                            : r.noIpFail ? noIpFail
                            : r.fail ? fail
                            : r.failDeps ? failDeps(r.failDeps)
                            : (function () { throw new Error("Unexpected ShallowVersionResult: " + JSON.stringify(r)) })()
                          });
                      })
                  });
                }
              )
            });
          okF(v)();
        }
      , function (e) { err(e)(); }
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
      , function (e) { err(e)(); }
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

exports.runFn11 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return function (j) {
                      return function (k) {
                        return fn(a, b, c, d, e, f, g, h, i, j, k);
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn12 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return function (j) {
                      return function (k) {
                        return function (l) {
                          return fn(a, b, c, d, e, f, g, h, i, j, k, l);
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn13 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return function (j) {
                      return function (k) {
                        return function (l) {
                          return function (m) {
                            return fn(a, b, c, d, e, f, g, h, i, j, k, l, m);
                          };
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};

exports.runFn14 = function (fn) {
  return function (a) {
    return function (b) {
      return function (c) {
        return function (d) {
          return function (e) {
            return function (f) {
              return function (g) {
                return function (h) {
                  return function (i) {
                    return function (j) {
                      return function (k) {
                        return function (l) {
                          return function (m) {
                            return function (n) {
                              return fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n);
                            };
                          };
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};
