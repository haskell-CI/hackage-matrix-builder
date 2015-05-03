(function () {
  $(window).ready(main);

  var apiRootUrl = ((appConfig && appConfig.apiHost) || "") + "/api";
  var api = new MatrixApi(apiRootUrl, apiRootUrl);

  window.ghcVersions = ["7.0", "7.2", "7.4", "7.6", "7.8", "7.10"];

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
      if (e.altKey || e.ctrlKey || e.metaKey || e.shiftKey || e.which != 1) {
        return;
      }

      var currentUri = new Uri(window.location.href);
      var linkUri = new Uri($(this).attr("href"));

      if ( !$(this).attr("href")
        || (currentUri.host() !== linkUri.host() && linkUri.host())
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
      window.history.pushState(null, "", uri.toString());
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
                 && (!showOnlyReports || window.allPackagesMore[v].report);
          }).map(function (v) {
            var date = window.allPackagesMore[v].report;
            return $("<li>").append
              ( packageLink(v)
              , date && $("<small>").text(" - last built: " + formatDate(date))
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

    api.Package.listLatestReports(renderReports, fail("Package.listLatestReports"), { count : 10 });
    function loadQueue () {
      api.Queue.list(renderQueue, fail("Queue.list"));
    }
    loadQueue();

    function renderReports (reports) {
      cont.find("#build-list").html("").append
        ( reports.items.map(function (i) {
            return $("<li>").append
              ( packageLink(i.packageName)
              , $("<small>").text(" - built " + formatDate(i.modified))
              );
          })
        );
    }

    function renderQueue (queue) {

      function prioUp (pkgName, currPrio, e) {
        e.preventDefault(); e.stopPropagation();

        if (currPrio === "high") return;

        var newPrio = (function () {
          if (currPrio === "medium") return "high";
          if (currPrio === "low"   ) return "medium";
        })();

        api.Queue.saveByName(pkgName, newPrio, loadQueue);
      }

      function prioDown (pkgName, currPrio, e) {
        e.preventDefault(); e.stopPropagation();

        if (currPrio === "low") return;

        var newPrio = (function () {
          if (currPrio === "high"  ) return "medium";
          if (currPrio === "medium") return "low";
        })();

        api.Queue.saveByName(pkgName, newPrio, loadQueue);
      }

      function remove (pkgName, e) {
        e.preventDefault(); e.stopPropagation();

        api.Queue.byName(pkgName).remove(loadQueue);
      }

      cont.find("#queue-list").html("").append
        ( queue.items.map(function (i,x) {
            return $("<tr>").addClass(i.priority).append
                      ( $("<td>").addClass("num").text(x+1)
                      , $("<td>").addClass("package-name").append(packageLink(i.packageName))
                      , $("<td>").addClass("priority").addClass(i.priority).text(i.priority)
                      , $("<td>").append
                          ( $("<a>").addClass("up").text("↑").click(prioUp.bind(null, i.packageName, i.priority)) )
                      , $("<td>").append
                          ( $("<a>").addClass("down").text("↓").click(prioDown.bind(null, i.packageName, i.priority)) )
                      , $("<td>").append
                          ( $("<a>").addClass("remove").text("╳").click(remove.bind(null, i.packageName)) )
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
            window.allPackagesMore[v.name] = { name : v.name, report : v.report };
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
    $("#search").keydown(function (e) {
      if (e.which === 13) {
        e.preventDefault();
        e.stopPropagation();
        fromUri(packageUri($(this).val()));
        return;
      }
    });
  }

  function selectedPackage (pkgName) {
    $("#select-package").val(pkgName);
    if (window.allPackages.indexOf(pkgName) === -1) {
      renderNotFound(pkgName);
      return;
    }
    api.Package.byName(pkgName).get(function (pkg) {
      api.Package.byName(pkgName).Report.latest().get(renderPackage.bind(null, pkgName, pkg), renderPackage.bind(null, pkgName, pkg, null));
    }, renderPackage.bind(null, pkgName, null, null, null));
  }

  function renderPackage (pkgName, pkg, report) {
    hidePages();
    $("#page-package .package-name").text(pkgName);
    $("#package").html("");
    if (pkg && report) {
      renderTable(pkgName, pkg, window.ghcVersions);
      renderSingleVersionMatrix(pkgName, pkg, report, window.ghcVersions);
      $(".package-header").show();
      $(".logs-header").show();
      $("#package-not-built").hide();
    } else {
      $(".package-header").hide();
      $(".logs-header").hide();
      renderTable(pkgName, pkg, window.ghcVersions);
      $("#package-not-built").show();
    }
    setupBuildQueuer(pkgName);
    cleanupTabs();
    $("#buildreport").show();
    $("#page-package").show();
  }

  function setupBuildQueuer (pkgName) {
    cleanupBuildQueuer();
    api.Queue.byName(pkgName).get(function (q) {
      $("#queueing .already-queued").show();
    });

    $("#queueing .action").click(function () {
      var prio = $("#queueing .prio").val();
      $("#queueing .form").hide();
      api.Queue.create({ packageName : pkgName, priority : prio }, function () {
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
    $("#queueing .already-queued").hide();
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

  function renderTable (pkgName, pkg, ghcVersions) {

    var cols = ghcVersions.length;
    var rows = pkg.versions.length;

    var t = $("<table>");

    var corner = $("<th>").append
      ( $("<a>").attr("href", "https://hackage.haskell.org/package/" + pkgName)
                .text(pkgName)
      );
    var headers = ghcVersions.map(function (ghcVersion) {
      return $("<th>").text(ghcVersion);
    });
    t.append($("<thead>").append($("<tr>").append(corner, headers)));

    var trs = [];
    for (var row = 0; row < rows; row++) {
      var v = pkg.versions[row];

      var versionName = v.version;
      var revision    = v.revision;
      var unpreferred = v.unpreferred;

      var th = $("<th>")
        .addClass("pkgv")
        .append
          ( $("<a>").text("Δ").attr("href", hdiffUrl(pkgName, versionName))
          , " "
          , $("<a>").attr("href", hackageUrl(pkgName, versionName)).text(versionName)
          , revision
              ? $("<sup>").append
                  ( $("<a>").text("(" + revision +  ")")
                            .attr("href", revisionsUrl(pkgName, versionName))
                            .attr("data-revision", revision)
                            .attr("data-version", versionName)
                            .addClass("revision")
                  )
              : null
          );

      var tds = ghcVersions.map(function (ghcVersionName, ghcI) {
        return $("<td>").addClass("stcell")
                        .addClass("fail-unknown")
                        .attr("data-ghc-version", ghcVersionName)
                        .attr("data-package-version", versionName);
      });

      var tr = $("<tr>").addClass("solver-row").append(th).append(tds);
      if (newMajor(pkg.versions[row-1], pkg.versions[row])) {
        tr.addClass("first-major");
      }
      if (newMinor(pkg.versions[row-1], pkg.versions[row])) {
        tr.addClass("first-minor");
      }
      trs.push(tr);
    }
    $("#package").append(t.append(trs));

    function newMajor (a,b) {
      if (!a) return true;
      var x = a.version.split(".");
      var y = b.version.split(".");
      return x[0] !== y[0] || (x[1] || "0") !== (y[1] || "0");
    }
    function newMinor (a,b) {
      if (!a) return true;
      var x = a.version.split(".");
      var y = b.version.split(".");
      return newMajor(a,b) || (x[2] || "0") !== (y[2] || "0");
    }
  }

  function renderSingleVersionMatrix (pkgName, pkg, report, ghcs) {

    report.results.forEach(function (ghcResult, i) {
      var ghcVersionName = ghcResult.ghcVersion;
      var ghcFullVersionName = ghcResult.ghcFullVersion;
      ghcResult.resultsA.forEach(function (versionResult, j) {
        var versionName = versionResult.packageVersion;
        var revision    = versionResult.packageRevision;
        var res         = versionResult.result;

        var th = $("#package .pkgv .revision[data-version='" + versionName + "']");
        var newestRevision = parseInt(th.attr("data-revision"), 10);
        if (revision !== newestRevision) {
          th.addClass("newer-revision");
        }

        var td = $("#package td[data-ghc-version='" + ghcVersionName + "'][data-package-version='" + versionName + "']");
        if (!td[0]) {
          console.warn("Could not find cell for " + cellHash(ghcVersionName, packageName, versionName));
        }
        td.removeClass("fail-unknown");
        var r;
        if (res.ok) {
          td.text("OK")
            .addClass("pass-build");
        } else if (res.noIp) {
          td.text("OK (no-ip)")
            .addClass("pass-no-ip");
        } else if (r = res.fail) {
          (function (r) {
            td.text("FAIL (pkg)")
              .addClass("fail-build")
              .click(function (e) {
                var ghcVersion = $(e.target).attr("data-ghc-version");
                var packageVersion = $(e.target).attr("data-package-version");
                setHash(cellHash(ghcVersion, pkgName, packageVersion));
                setupFailTabs(ghcVersion, pkgName, packageVersion, r);
              });
          })(r);
        } else if (r = res.failDeps) {
          (function () {
            td.text("FAIL (" + r.length + " deps)")
              .addClass("fail-dep-build")
              .click(function (e) {
                var ghcVersion = $(e.target).attr("data-ghc-version");
                var packageVersion = $(e.target).attr("data-package-version");
                setHash(cellHash(ghcVersion, pkgName, packageVersion));
                setupFailDepsTabs(ghcVersion, r);
              });
          })(r);
        } else if (res.nop) {
          td.text("OK (boot)")
            .addClass("pass-no-op");
        } else {
          console.warn("unhandled result: ", res);
          td.addClass("fail-unknown");
        }
      });
    });

    function setupFailTabs (ghcVersion, pkgName, packageVersion, r) {
      setupTabs
        ( [{ label    : cellHash(ghcVersion, pkgName, packageVersion)
           , contents : $("<pre>").addClass("log-entry").text(r)
          }]
        );
    }

    function setupFailDepsTabs (ghcVersion, r) {
      setupTabs
        ( r.map(function (v, i) {
            return { label    : cellHash(ghcVersion, v.packageName, v.packageVersion)
                   , contents : $("<div>").append
                                  ( packageLink(v.packageName, ghcVersion, v.packageVersion).text("Go to this package")
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
        var ghcVer = report.results.filter(function (v) { return v.ghcVersion === ghcVersion; })[0];
        if (!ghcVer) {
          console.warn("Could not find ghc version: GHC-" + ghcVersion);
          return;
        }
        var res = ghcVer.resultsA.filter(function (v) { return v.packageVersion === packageVersion; })[0];
        if (!res) {
          console.warn("Could not find ghc/package version: GHC-" + ghcVersion + "/" + pkgName + "-" + packageVersion);
          return;
        }
        if (res.result.fail) {
          setupFailTabs(ghcVersion, pkgName, packageVersion, res.result.fail);
        }
        else if (res.result.failDeps) {
          setupFailDepsTabs(ghcVersion, res.result.failDeps);
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

  function hdiffUrl (pkgName, versionName) {
    return "http://hdiff.luite.com/cgit/" + pkgName + "/commit?id=" + versionName;
  }
  function hackageUrl (pkgName, versionName) {
    return "https://hackage.haskell.org/package/" + pkgName + "-" + versionName + "/" + pkgName + ".cabal/edit";
  }
  function revisionsUrl (pkgName, versionName) {
    return "https://hackage.haskell.org/package/" + pkgName + "-" + versionName + "/revisions";
  }
  function cellHash (ghcVersion, pkgName, pkgVersion) {
    return "GHC-" + ghcVersion + "/" + pkgName + "-" + pkgVersion;
  }
  function packageUri (pkgName, ghcVersion, pkgVersion) {
    var u = new Uri("/package/" + pkgName);
    if (ghcVersion && pkgVersion) {
      u.anchor(cellHash(ghcVersion, pkgName, pkgVersion));
    }
    return u;
  }
  function packageLink (pkgName, ghcVersion, pkgVersion) {
    return $("<a>").attr("href", packageUri(pkgName, ghcVersion, pkgVersion).toString()).text(pkgName);
  }

})();
