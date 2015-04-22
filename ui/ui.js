(function () {
  $(window).ready(main);

  var api = new MatrixApi( "http://localhost:3000/api"
                         , "http://localhost:3000/api"
                         );

  function fail (msg) {
    return function () {
      console.warn("request failed", msg, arguments);
    };
  }

  function main () {
    var pkgName = /\/package\/([^\/]+)$/.test(window.location.href)
                && RegExp.$1;

    api.Package.list(function ok (l) {

      var s = $("<select/>").attr("id", "select-package");
      s.change(function () {
        selectedPackage($(this).val());
      });
      l.items.forEach(function (v) {
        var li = $("<option/>").text(v).attr("value", v);
        s.append(li);
      });
      $("#package-list").append(s);

      if (pkgName) {
        selectedPackage(pkgName);
      } else if (l.items.length > 0) {
        selectedPackage(l.items[0]);
      }

      setupPicker(l.items);

    }, fail("Package.list"));

  }

  function setupPicker (items) {
    $("#search").autocomplete(
      { source : items
      , select : function (_, v) {
          selectedPackage(v.item.value);
        }
      });
  }

  function selectedPackage (pkgName) {
    $("#select-package").val(pkgName);
    api.Package.byName(pkgName).get(packageLoaded.bind(null, pkgName), packageNotFound.bind(null, pkgName));
  }

  function packageNotFound (pkgName) {
    $("#buildreport").hide();
    $("#notfound-pkgname").text(pkgName);
    $("#notfound").show();
  }

  function packageUrl (pkgName) {
    return "/package/" + pkgName
  }

  function packageLoaded (pkgName, p) {
    $("#notfound").hide();
    window.history.replaceState(null, pkgName, packageUrl(pkgName));
    $("#package").html("");
    renderSingleVersionMatrix(pkgName, p);
    setupBuildQueuer(pkgName);
    cleanupTabs();
    $("#buildreport").show();
  }

  function setupBuildQueuer (pkgName) {
    cleanupBuildQueuer();
    $("#build-queuer").click(function () {
      $(this).hide();
      $("#build-queuer-pass").show().submit(function (e) {
        e.preventDefault();
        setTimeout(function () {
          api.Package.byName(pkgName).Resource.create($("#build-queuer-password").val(), function () {
            $("#build-queuer").hide();
            $("#build-queuer-pass").hide();
            $("#build-queuer-ok").show();
          }, function () {
            $("#build-queuer-pass-error").show();
          });
        }, 0);
      });
    });
  }

  function cleanupBuildQueuer () {
    $("#build-queuer").off("click");
    $("#build-queuer-form").off("submit");
    $("#build-queuer-ok").hide();
    $("#build-queuer-pass").hide();
    $("#build-queuer").show();
  }

  function setupTabs (header, messages) {
    cleanupTabs();
    var tabs = $("<div>").attr("id", "tabs").append
      ( $("<ul>").append
          ( messages.map(function (v, i) {
              return $("<li>").append
                ( $("<a>").attr("href", "#fragment-" + (i+1)).text(v.label)
                );
            })
          )
      , messages.map(function (v, i)  {
          return $("<div>").attr("id", "fragment-" + (i+1)).append(v.contents);
        })
      );
    tabs.tabs();
    tabs.appendTo($("#tabs-container"));
  }
  function cleanupTabs () {
    $("#tabs-container").html("");
  }

  function renderSingleVersionMatrix (pkgName, p) {
    var t = $("<table>");

    var cols = p.ghcVersions.length;
    var rows = p.versions.length;

    var corner = $("<th>").append($("<a>").attr("href", "https://hackage.haskell.org/package/" + pkgName)
                                          .text(pkgName));
    var headers = p.ghcVersions.map(function (ghcVersion) {
      var ghcVersionName = ghcVersion.ghcVer.name;
      return $("<th>").text(ghcVersionName);
    });
    t.append($("<thead>").append($("<tr>").append(corner, headers)));

    var trs = [];
    var iii = -1;
    var newMajorVersion = false;
    var newMinorVersion = false;
    p.versions.forEach(function (minors) {
      newMajorVersion = true;
      minors.forEach(function (versions) {
        newMinorVersion = true;
        versions.forEach(function (version) {
          iii++;
          var versionName = version.version.name;

          var th = $("<th>")
            .addClass("pkgv")
            .append
              ( $("<a>").text("Δ").attr("href", "http://hdiff.luite.com/cgit/" + pkgName + "/commit?id=" + versionName)
              , " "
              , $("<a>").attr("href", "https://hackage.haskell.org/package/" + pkgName + "-" + versionName + "/" + pkgName + ".cabal/edit").text(versionName)
              , version.revision
                  ? $("<sup>").append($("<a>").text("(" + version.revision +  ")").attr( "href", "https://hackage.haskell.org/package/" + pkgName + "-" + versionName + "/revisions"))
                  : null
              );

          var tds = p.ghcVersions.map(function (ghcVersion, ghcI) {
            var td = $("<td>").addClass("stcell");
            var ghcVersion = ghcVersion;
            var ghcVersionName = ghcVersion.ghcVer.name;
            var res = ghcVersion.resultsA[iii];
            td.attr("data-ghc-version", ghcVersionName)
              .attr("data-package-version", versionName);
            var r;
            if (res.result.ok) {
              td.text("OK")
                .addClass("pass-build");
            } else if (res.result.noIp) {
              td.text("OK (no-ip)")
                .addClass("pass-no-ip");
            } else if (r = res.result.fail) {
              (function (r) {
                td.text("FAIL (pkg)")
                  .addClass("fail-build")
                  .click(function (e) {
                    var ghcVersion = $(e.target).attr("data-ghc-version");
                    var packageVersion = $(e.target).attr("data-package-version");
                    setupTabs
                      ("Compilation failure"
                      , [{ label    : "GHC-" + ghcVersion + "/" + pkgName + "-" + packageVersion
                         , contents : $("<pre>").addClass("log-entry").text(r)
                        }]
                      );
                  });
              })(r);
            } else if (r = res.result.failDeps) {
              (function () {
                td.text("FAIL (" + r.length + " deps)")
                  .addClass("fail-dep-build")
                  .click(function (e) {
                    var ghcVersion = $(e.target).attr("data-ghc-version");
                    // setLog(r.length + " dependencies failed to compile", r.map(function (v) { return v.message; }));
                    setupTabs
                      ( r.length + " dependencies failed to compile"
                      , r.map(function (v, i) {
                          return { label    : "GHC-" + ghcVersion + "/" + v.pkgId.pPackageName + "-" + v.pkgId.pPackageVersion.name
                                 , contents : $("<div>").append
                                                ( $("<a>").addClass("package-link")
                                                          .attr("href", packageUrl(pkgName))
                                                          .text("Go to this package")
                                                , $("<pre>").addClass("log-entry").text(v.message)
                                                )
                                 };
                        })
                      );
                  });
              })(r);
            } else if (res.result.nop) {
              td.text("OK (boot)")
                .addClass("pass-no-op");
            } else {
              console.warn("unhandled result: ", res.result);
              td.addClass("fail-unknown");
            }
            return td;
          });
          var tr = $("<tr>").addClass("solver-row").append(th).append(tds);
          if (newMajorVersion) {
            tr.addClass("first-major");
            newMajorVersion = false;
          }
          if (newMinorVersion) {
            tr.addClass("first-minor");
            newMinorVersion = false;
          }
          trs.push(tr);
        });
      });
    });
    $("#package").append(t.append(trs));
  }

})();
