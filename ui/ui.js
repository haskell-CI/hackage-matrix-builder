(function () {
  var api = new MatrixApi( "http://localhost:3000/api"
                         , "http://localhost:3000/api"
                         );

  $(window).ready(main);

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

    }, fail("Package.list"));
  }

  function selectedPackage (pkgName) {
    $("#select-package").val(pkgName);
    api.Package.byName(pkgName).get(packageLoaded.bind(null, pkgName), fail("Package.byName"));
  }

  function packageLoaded (pkgName, p) {
    window.history.replaceState(null, pkgName, "/package/" + pkgName);
    $("#package").html("");
    clearLog();

    var t = $("<table>");

    var cols = p.ghcVersions.length;
    var rows = p.versions.length;

    for (var y = 0; y < rows; y++) {
      var tr = $("<tr>").addClass("solver-row");
      var versionName = p.versions[y].version.name;
      for (var x = 0; x < cols; x++) {
        var td = $("<td>").addClass("stcell").addClass("lastmaj");
        var ghcVersion = p.ghcVersions[x];
        var ghcVersionName = ghcVersion.ghcVer.name;
        var res = ghcVersion.resultsA[y];
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
        tr.append(td);
      }
      t.append(tr);
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

    var header = $("<tr>");
    t.find("tr").each(function (i, tr) {
      var th = $("<th>").addClass("pkgv");
      var versionName = p.versions[i].version.name;
      th.append
        ( $("<a>").text("Δ").attr("href", "http://hdiff.luite.com/cgit/" + pkgName + "/commit?id=" + versionName)
        , " "
        , $("<a>").attr("href", "https://hackage.haskell.org/package/" + pkgName + "-" + versionName + "/" + pkgName + ".cabal/edit").text(versionName)
        );
      $(tr).prepend(th);
    });

    t.prepend((function () {
      var tr = $("<tr>");
      tr.append($("<th>").append($("<a>").attr("href", "https://hackage.haskell.org/package/" + pkgName).text(pkgName)));
      for (var i = 0; i < p.ghcVersions.length; i++) {
        var versionName = p.ghcVersions[i].ghcVer.name;
        tr.append($("<th>").text(versionName));
      }
      return tr;
    })());

    $("#package").append(t);
  }

})();
