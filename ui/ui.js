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
      , select : function () {
          console.log("change", this, arguments);
          selectedPackage($(this).val());
        }
      });
  }

  function selectedPackage (pkgName) {
    $("#select-package").val(pkgName);
    api.Package.byName(pkgName).get(packageLoaded.bind(null, pkgName), fail("Package.byName"));
  }

  function packageLoaded (pkgName, p) {
    window.history.replaceState(null, pkgName, "/package/" + pkgName);
    $("#package").html("");
    clearLog();
    renderSingleVersionMatrix(pkgName, p);
  }

  function setLog (header, messages) {
    $("#log-container").html("");
    var logHeader = $("<div>").text(header);
    $("#log-container").append(logHeader);
    messages.forEach(function (m) {
      var pre = $("<pre>").addClass("log-entry").text(m);
      $("#log-container").append(pre);
    });
    $("#log-container").show();
  }

  function clearLog () {
    $("#log-container").html("");
    $("#log-container").hide();
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
                    setLog("Compilation failure", [r]);
                  });
              })(r);
            } else if (r = res.result.failDeps) {
              (function () {
                td.text("FAIL (" + r.length + " deps)")
                  .addClass("fail-dep-build")
                  .click(function (e) {
                    setLog(r.length + " dependencies failed to compile", r.map(function (v) { return v.message; }));
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
