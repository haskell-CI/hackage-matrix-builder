(function () {
  $(window).ready(main);

  var apiRootUrl = ((appConfig && appConfig.apiHost) || "") + "/api";
  var api = new MatrixApi(apiRootUrl, apiRootUrl);

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

    $(window).on("popstate", function () {
      fromUri(new Uri(window.location.href), true, true);
    });

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

    fromUri(new Uri(window.location.href), true, true);
  }

  function fromUri (uri, force, isPopping) {
    if (!force && uri.path() === new Uri(window.location.href).path()) {
      return;
    }
    if (!isPopping) {
      setPath(uri.path());
    }

    if (uri.path() === "/") {
      renderHome();
      return;
    }

    var pkgName;
    if (pkgName = getPackageName(uri)) {
      selectedPackage(pkgName);
      return;
    }

    if (uri.path() === "/latest") {
      renderLatest();
      return;
    }

    if (uri.path() === "/packages") {
      renderPackages();
      return;
    }

    renderNotFound();
  }

  function hidePages () {
    $(".page").hide();
  }

  function packageLink (pkgName) {
    return $("<a>").attr("href", packageUri(pkgName).toString()).text(pkgName);
  }

  function renderPackages () {
    hidePages();
    var page = $("#page-packages");
    var headers = page.find(".headers").html("");
    var pkgList = page.find(".packages").html("");
    var onlyReports = page.find("#packages-only-reports");
    var headers = [];
    var selectedPrefix = "A";
    for (var i = 65; i <= 90; i++) {
      headers.push(String.fromCharCode(i));
    }
    page.find(".headers").append
      ( headers.map(function (v) {
          return $("<li>").append
            ( $("<a>").attr("data-prefix", v)
                      .addClass("header")
                      .attr("href","")
                      .text(v)
                      .click(function (e) {
              e.preventDefault();
              e.stopPropagation();
              selectedPrefix = $(this).attr("data-prefix");
              showPrefix();
              })
            );
        })
      );
    onlyReports.change(showPrefix);
    function showPrefix () {
      var showOnlyReports = onlyReports.is(":checked");
      pkgList.html("");
      pkgList.append
        ( window.allPackages.filter(function (v) {
              return v[0].toUpperCase() === selectedPrefix
                 && (!showOnlyReports || window.allPackagesMore[v].lastReport);
          }).map(function (v) {
            var more = window.allPackagesMore[v].lastReport;
            return $("<li>").append
              ( packageLink(v)
              , more && $("<small>").text(" - last built: " + formatDate(more))
              );
          })
        );
    }
    showPrefix();
    page.show();
  }

  function renderLatest () {
    hidePages();

    var cont = $("#page-latest");
    cont.find(".refresh").off("click").on("click", function (e) {
      e.preventDefault();
      setTimeout(renderLatest, 0);
    });

    api.Queue.list(function (queue) {
      api.Package.listLatestReports(ok.bind(null, queue), fail("Package.listLatestReports"), { count : 10 });
    }, fail("Queue.list"));

    function ok (queue, res) {


      cont.find("#queue-list").html("").append
        ( queue.items.map(function (i) {
            return $("<li>").append(packageLink(i));
          })
        );

      cont.find("#build-list").html("").append
        ( res.items.map(function (i) {
            return $("<li>").append
              ( packageLink(i.packageName)
              , $("<small>").text(" - built " + formatDate(i.reportStamp))
              );
          })
        );
      cont.show();
    }
  }

  function renderNotFound (pkgName) {
    hidePages();

    var msg = "Page not found, sorry!";
    if (pkgName) {
      msg = $("<div>").append("The package ", $("<strong>").text(pkgName), " could not be found.");
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
    window.history.pushState(null, "", path);
  }
  function setHash (hash)
  {
    var uri = new Uri(window.location.href);
    if (uri.anchor() == hash) {
      return;
    }
    uri.anchor(hash);
    window.history.replaceState(null, "", uri.toString());
  }

  function main () {

    // Preload package metadata for all packages
    window.allPackages = [];
    window.allPackagesMore = {};
    var responses = 0;
    for (var i = 0; i < 10; i++) {
      (function (i) {
        api.Package.list(function (l) {
          l.items.forEach(function (v) {
            window.allPackages.push(v.name);
            window.allPackagesMore[v.name] = { name : v.name, lastReport : v.reportStamp };
          });
          responses++;
          checkDone();
        }, function () {
          fail("Package.list: " + i);
          responses++;
          checkDone();
        }, { count : 1000, offset : i*1000 });
      })(i);
    }
    function checkDone () {
      if (responses < 10) {
        return;
      }
      setupRouting();
      setupPicker(window.allPackages);
    }

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
    if (window.allPackages.indexOf(pkgName) === -1) {
      renderNotFound(pkgName);
      return;
    }
    api.Package.byName(pkgName).Report.latest().get(renderPackage.bind(null, pkgName), renderPackage.bind(null, pkgName, null));
  }

  function packageUri (pkgName) {
    return new Uri("/package/" + pkgName);
  }

  function renderPackage (pkgName, p) {
    hidePages();
    $("#page-package .package-name").text(pkgName);
    $("#package").html("");
    if (p) {
      renderSingleVersionMatrix(pkgName, p);
      $(".package-header").show();
      $(".logs-header").show();
      $("#package-not-built").hide();
    } else {
      $(".package-header").hide();
      $(".logs-header").hide();
      $("#package-not-built").show();
    }
    setupBuildQueuer(pkgName);
    cleanupTabs();
    $("#buildreport").show();
    $("#page-package").show();
  }

  function setupBuildQueuer (pkgName) {
    cleanupBuildQueuer();

    $("#queueing .action").click(function () {
      var prio = $("#queueing .prio").val();
      $("#queueing .form").hide();
      api.Package.byName(pkgName).Report.create(prio, function () {
        $("#queueing .success").show();
      }, function () {
        $("#queueing .error").show();
      });
    });
  }

  function cleanupBuildQueuer () {
    $("#queueing .form").show();
    $("#queueing .action").off("click");
    $("#queueing .success").hide();
    $("#queueing .error").hide();
  }

  function setupTabs (messages) {
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
                    setHash("GHC-" + ghcVersion + "/" + pkgName + "-" + packageVersion);
                    setupFailTabs(ghcVersion, pkgName, packageVersion, r);
                  });
              })(r);
            } else if (r = res.result.failDeps) {
              (function () {
                td.text("FAIL (" + r.length + " deps)")
                  .addClass("fail-dep-build")
                  .click(function (e) {
                    var ghcVersion = $(e.target).attr("data-ghc-version");
                    var packageVersion = $(e.target).attr("data-package-version");
                    setHash("GHC-" + ghcVersion + "/" + pkgName + "-" + packageVersion);
                    setupFailDepsTabs(ghcVersion, r);
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

    function setupFailTabs (ghcVersion, pkgName, packageVersion, r) {
      setupTabs
        ( [{ label    : "GHC-" + ghcVersion + "/" + pkgName + "-" + packageVersion
           , contents : $("<pre>").addClass("log-entry").text(r)
          }]
        );
    }

    function setupFailDepsTabs (ghcVersion, r) {
      setupTabs
        ( r.map(function (v, i) {
            return { label    : "GHC-" + ghcVersion + "/" + v.pkgId.pPackageName + "-" + v.pkgId.pPackageVersion.name
                   , contents : $("<div>").append
                                  ( packageLink(v.pkgId.pPackageName).text("Go to this package")
                                  , $("<pre>").addClass("log-entry").text(v.message)
                                  )
                   };
          })
        );

    }

    if (/^#GHC-([^\/]+)\/[^.]+-(.+?)$/.test(window.location.hash)) {
      setTimeout(function () {
        /^#GHC-([^\/]+)\/[^.]+-(.+?)$/.test(window.location.hash);
        var ghcVersion     = RegExp.$1;
        var packageVersion = RegExp.$2;
        var ghcVer = p.ghcVersions.filter(function (v) { return v.ghcVer.name === ghcVersion; })[0];
        if (!ghcVer) {
          console.warn("Could not find ghc version: GHC-" + ghcVersion);
          return;
        }
        var res = ghcVer.resultsA.filter(function (v) { return v.pkgVersion.name === packageVersion; })[0];
        if (!res) {
          console.warn("Could not find ghc/package version: GHC-" + ghcVersion + "/" + pkgName + "-" + packageVersion);
          return;
        }
        var r;
        if (r = res.result.fail) {
          setupFailTabs(ghcVersion, pkgName, packageVersion, r);
        }
        else if (r = res.result.failDeps) {
          setupFailDepsTabs(ghcVersion, r);
        } else {
          console.warn("No build failure found for: GHC-" + ghcVersion + "/" + pkgName + "-" + packageVersion);
        }
      }, 0);
    }
  }

  function formatDate (d) {
    d = new Date(d);
    return d.toLocaleString().replace("T", " ");
  }

})();
