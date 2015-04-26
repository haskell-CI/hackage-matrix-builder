(function () {
  $(window).ready(main);

  var api = new MatrixApi
    ( "http://localhost:3000/api"
    , "http://localhost:3000/api"
    );

  function fail (msg) {
    return function () {
      console.warn("request failed", msg, arguments);
    };
  }

  function getPackageName (uri)
  {
    return (/^\/package\/([^\/]+)$/.test(uri.path()) && RegExp.$1) || null;
  }

  function setupRouting ()
  {

    $("body").delegate("a", "click", function (e) {
      var currentUri = new Uri(window.location.href);
      var linkUri = new Uri($(this).attr("href"));

      if ( (currentUri.host() !== linkUri.host() && linkUri.host())
        || ((currentUri.path() === linkUri.path() || !linkUri.path()) && linkUri.anchor())
         ) {
        return true;
      }

      e.preventDefault();
      setTimeout(function () { fromUri(linkUri); }, 0);
    });

    fromUri(new Uri(window.location.href), true);
  }

  function fromUri (uri, force) {
    if (!force && uri.path() === new Uri(window.location.href).path()) {
      return;
    }
    setPath(uri.path());

    if (uri.path() === "/") {
      renderHome();
      return;
    }

    var pkgName;
    if (pkgName = getPackageName(uri)) {
      selectedPackage(pkgName);
      return;
    }

    renderNotFound();
  }

  function hidePages () {
    $(".page").hide();
  }

  function renderNotFound (pkgName) {
    hidePages();

    var msg = "Page not found, sorry!";
    if (pkgName) {
      msg = $("<div>").append("The package ", $("<strong>").text(pkgName), " could not be found");
    }

    $("#page-notfound .message").html("").append(msg);
    $("#page-notfound").show();
  }

  function renderHome () {
    hidePages();
    $("#page-home").show();
  }

  function setPath (path)
  {
    window.history.replaceState(null, "", path);
  }

  function main () {
    setupRouting();

    api.Package.list(function ok (l) {

      var s = $("<select/>").attr("id", "select-package");
      s.change(function () {
        fromUri(packageUri($(this).val()));
      });
      l.items.forEach(function (v) {
        var li = $("<option/>").text(v).attr("value", v);
        s.append(li);
      });
      $("#package-list").append(s);
      setupPicker(l.items);

    }, fail("Package.list"));

  }

  function setupPicker (items) {
    $("#search").autocomplete(
      { source : items
      , select : function (_, v) {
          fromUri(packageUri(v.item.value));
        }
      });
  }

  function selectedPackage (pkgName) {
    $("#select-package").val(pkgName);
    api.Package.byName(pkgName).get(renderPackage.bind(null, pkgName), renderNotFound.bind(null, pkgName));
  }

  function packageUri (pkgName) {
    return new Uri("/package/" + pkgName);
  }

  function renderPackage (pkgName, p) {
    hidePages();
    $("#package").html("");
    renderSingleVersionMatrix(pkgName, p);
    setupBuildQueuer(pkgName);
    cleanupTabs();
    $("#buildreport").show();
    $("#page-buildreport").show();
  }

  function setupBuildQueuer (pkgName) {
    cleanupBuildQueuer();

    $("#queueing .action").click(function () {
      $("#queueing #action-1").hide();
      api.Package.byName(pkgName).Report.create(function () {
        $("#queueing .success").show();
      }, function () {
        $("#queueing .error").show();
      });
    });
  }

  function cleanupBuildQueuer () {
    $("#queueing .action").off("click");
    $("#queueing .success").hide();
    $("#queueing .error").hide();
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
                    setupTabs
                      ( r.length + " dependencies failed to compile"
                      , r.map(function (v, i) {
                          return { label    : "GHC-" + ghcVersion + "/" + v.pkgId.pPackageName + "-" + v.pkgId.pPackageVersion.name
                                 , contents : $("<div>").append
                                                ( $("<a>").addClass("package-link")
                                                          .attr("href", packageUri(v.pkgId.pPackageName))
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
