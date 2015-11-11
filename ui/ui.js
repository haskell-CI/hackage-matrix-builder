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
    var title = null;
    var pkgName;

    if (uri.path() === "/") {
      renderHome();
    } else if (pkgName = getPackageName(uri)) {
      title = pkgName + " - package";
      selectedPackage(pkgName);
    } else if (uri.path() === "/latest") {
      title = "latest"
      renderLatest();
    } else if (/^\/user\/([^\/]+)/.test(uri.path())) {
      var name = RegExp.$1;
      title = name + "- users";
      renderUser(name);
    } else if (uri.path() === "/packages") {
      title = "packages";
      renderPackages();
    } else {
      title = "404'd!";
      renderNotFound();
    }

    title = (title ? title + " - " : "") + "Hackage Matrix Builder";
    if (!isPopping) {
      window.history.pushState(null, title, uri.toString());
      window.document.title = title;
    } else {
      window.document.title = title;
      window.history.replaceState(null, title, uri.toString());
    }
  }

  function hidePages () {
    $(".page").hide();
  }

  function renderUser (name) {
    hidePages();
    var page = $("#page-user");
    var content = page.find(".content");
    var pkgList = page.find(".packages").html("");
    var onlyReports = page.find(".user-only-reports");
    var usernameSubtext = page.find(".main-header-subtext");
    page.find(".main-header").text(name);
    api.User.byName(name).get(gotUser, userNotFound);
    function userNotFound () {
      usernameSubtext.addClass("error").text("The user could not be found.");
      content.hide();
      page.show();
    }
    function gotUser (u) {
      usernameSubtext.removeClass("error").text("Displaying packages maintained by this user.");
      function renderPackageList () {
        pkgList.html("");
        var showOnlyReports = onlyReports.is(":checked");
        pkgList.html("");
        pkgList.append
          ( u.packages.filter(function (v) {
              return !showOnlyReports || (window.allPackagesMore[v] && window.allPackagesMore[v].report);
            }).map(function (v) {
              var date = (function () {
              if (!window.allPackagesMore[v]) {
                console.warn("packages.json might be out of date, could not find package: " + v);
                return;
              } else {
                return window.allPackagesMore[v].report;
              }
              })();
              return $("<li>").append
                ( packageLink(v)
                , date && $("<small>").text(" - last built: " + formatDate(date))
                );
            })
          );
      }
      onlyReports.change(renderPackageList);
      renderPackageList();
      content.show();
      page.show();
    }
  }

  function renderPackages () {
    hidePages();
    var page           = $("#page-packages");
    var tags           = page.find("tag-filter").html("");
    var headers        = page.find(".headers").html("");
    var pkgList        = page.find(".packages").html("");
    var tagFilters     = [];
    var onlyReports    = page.find(".packages-only-reports");
    var headers        = [];
    var selectedPrefix = "A";
    for (var i = 65; i <= 90; i++) {
      headers.push(String.fromCharCode(i));
    }
    tags.removeClass("active");
    page.find(".tag-filter").html("").append
      ( window.allTags
          .filter(function (t) { return t.packages.length; })
          .map(function (t) {
            return renderTag(t.name).click(function (e) {
              var tagName = $(e.target).attr("data-tag-name");
              var tagIndex = tagFilters.indexOf(tagName);
              if (tagIndex >= 0) {
                tagFilters.splice(tagIndex, 1);
                $(e.target).removeClass("active");
              } else {
                tagFilters.push(tagName);
                $(e.target).addClass("active");
              }
              showPrefix();
            })
          })
      );
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
      var filterByTags = !!tagFilters.length;
      pkgList.html("");
      pkgList.append
        ( window.allPackages.filter(
          function (v) {
            return (
              filterByTags
                ? (window.allPackagesMore[v].tags.filter(function (t) { return tagFilters.indexOf(t) >= 0; }).length > 0)
                : v[0].toUpperCase() === selectedPrefix
              ) && (!showOnlyReports || window.allPackagesMore[v].report);
          }).map(function (v) {
            var date = window.allPackagesMore[v].report;
            return $("<li>").append
              ( packageLink(v)
              , window.allPackagesMore[v].tags.map(renderTag)
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

    api.Package.listLatestReports(renderReports, fail("Package.listLatestReports"), { count : 100 });
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
    window.allPackages     = [];
    window.allPackagesMore = {};
    window.allTags         = [];
    var responses = 0;
    api.Tag.list(function (l) {
      window.allTags = l.items;
      responses++;
      checkDone();
    }, function () {
      fail("Tag.list").apply(null, arguments);
      responses++;
      checkDone();
    }, { count : 1000 });
    for (var i = 0; i < 10; i++) {
      (function (i) {
        api.Package.list(function (l) {
          l.items.forEach(function (v) {
            window.allPackages.push(v.name);
            window.allPackagesMore[v.name] = { name : v.name, report : v.report, tags : v.tags };
          });
          responses++;
          checkDone();
        }, function () {
          fail("Package.list: " + i).apply(null, arguments);
          responses++;
          checkDone();
        }, { count : 1000, offset : i*1000 });
      })(i);
    }
    function checkDone () {
      if (responses < 11) {
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
    $("#page-package .main-header").text(pkgName);
    $("#package").html("");
    if (pkg && report) {
      renderTable(pkgName, pkg, window.ghcVersions);
      $("#page-package .main-header-subtext.last-build").text("Last build: " + formatDate(report.modified));
      renderSingleVersionMatrix(pkgName, pkg, report, window.ghcVersions);
      $(".package-header").show();
      $(".logs-header").show();
      $("#package-not-built").hide();
    } else if (pkg) {
      $(".package-header").hide();
      $(".logs-header").hide();
      renderTable(pkgName, pkg, window.ghcVersions);
      $("#package-not-built").show();
    }
    setupBuildQueuer(pkgName);
    setupTagger(pkgName);
    cleanupTabs();
    $("#buildreport").show();
    $("#page-package").show();
  }

  function setupBuildQueuer (pkgName) {
    cleanupBuildQueuer();
    api.Queue.byName(pkgName).get(function (q) {
      $("#queueing .already-queued").show();
    }, function (r) {
      if (r && r.responseJSON && r.responseJSON.notFound) return;
      fail("Queue.byName(" + pkgName + ").get").apply(null, arguments);
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

  function setupTagger (pkgName) {
    cleanupTagger();

    function removeTag (tagName, pkgName)
    {
      api.Tag.byName(tagName).remove(pkgName, function () {
        setupTagger(pkgName);
      }, function () {
        fail("api.Tag.removeByName(" + tagName + ", " + pkgName + ")").apply(null, arguments);
        $("#tagging .error").show();
      });
    }

    api.Package.byName(pkgName).tags(function (tags) {
      $("#tagging .tags").append(
        tags.map(function (t) {
          return $("<li>").append
            ( $("<span>").text(t)
            ,  $("<a>").addClass("remove").text("╳").click(removeTag.bind(null, t, pkgName))
            );
        })
      );

    }, fail("Package.byName(" + pkgName + ").tags()"));

    $("#tagging").delegate("a", "click", function (e) {
      if (e.altKey || e.ctrlKey || e.metaKey || e.shiftKey || e.which != 1) {
        return;
      }
      e.stopPropagation()
      e.preventDefault();


    });
    $("#tagging .action").click(function () {
      var tagName = $("#tagging .tag-name").val();
      api.Tag.saveByName(tagName, pkgName, function () {
        $("#tagging .tag-name").val("");
        setupTagger(pkgName);
      }, function () {
        $("#tagging .error").show();
      });
    });
  }

  function cleanupTagger () {
    $("#tagging .action").off("click");
    $("#tagging .error").hide();
    $("#tagging .tags").html("");
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
      var prevVersionNameMay = pkg.versions[row-1] && pkg.versions[row-1].version;

      var th = $("<th>")
        .addClass("pkgv")
        .append
          ( $("<a>").text("Δ").attr("href", hdiffUrl(pkgName, versionName, prevVersionNameMay))
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
      ghcResult.ghcResult.forEach(function (versionResult, j) {
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
          console.warn("Could not find cell for " + cellHash(ghcVersionName, pkgName, versionName));
        }
        td.removeClass("fail-unknown");
        var r;
        if (res.ok) {
          td.text("OK")
            .addClass("pass-build");
        } else if (res.noIp) {
          td.text("OK (no-ip)")
            .addClass("pass-no-ip");
        } else if (r = res.noIpBjLimit) {
          td.text("FAIL (BJ " + r + ")")
            .addClass("fail-bj");
        } else if (r = res.noIpFail) {
          (function (r) {
            td.text("FAIL (no-ip)")
              .addClass("fail-no-ip")
              .click(function (e) {
                var ghcVersion = $(e.target).attr("data-ghc-version");
                var packageVersion = $(e.target).attr("data-package-version");
                setHash(cellHash(ghcVersion, pkgName, packageVersion));
                setupFailTabs(ghcVersion, pkgName, packageVersion, r.err + "\n" + r.out);
              });
          })(r);
        } else if (r = res.fail) {
          (function (r) {
            td.text("FAIL (pkg)")
              .addClass("fail-build")
              .click(function (e) {
                var ghcVersion = $(e.target).attr("data-ghc-version");
                var packageVersion = $(e.target).attr("data-package-version");
                var ident = ghcVersion + "-" + packageVersion;
                api.Package.byName(pkgName).Report.latest().Cell.byId(ident)
                   .get( function success (s) { setupFailTabs(ghcVersion, pkgName, packageVersion, s.resultA.result.fail); }

                       , function fail    (f) { console.warn("Loading cell data failed for " + ident, arguments) }
                       );
                setHash(cellHash(ghcVersion, pkgName, packageVersion));
              });
          })(r);
        } else if (r = res.failDeps) {
          (function () {
            td.text("FAIL (" + r + " deps)")
              .addClass("fail-dep-build")
              .click(function (e) {
                var ghcVersion = $(e.target).attr("data-ghc-version");
                var packageVersion = $(e.target).attr("data-package-version");
                var ident = ghcVersion + "-" + packageVersion;
                api.Package.byName(pkgName).Report.latest().Cell.byId(ident)
                   .get( function success (s) { setupFailDepsTabs(ghcVersion, s.resultA.result.failDeps); }

                       , function fail    (f) { console.warn("Loading cell data failed for " + ident, arguments) }
                       );
                setHash(cellHash(ghcVersion, pkgName, packageVersion));
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
        var ident = ghcVer.ghcVersion + "-" + packageVersion;
        if (!ghcVer) {
          console.warn("Could not find ghc version: GHC-" + ghcVersion);
          return;
        }

        var res = ghcVer.ghcResult.filter(function (v) { return v.packageVersion === packageVersion; })[0];
        if (!res) {
          console.warn("Could not find ghc/package version: GHC-" + ghcVersion + "/" + pkgName + "-" + packageVersion);
          return;
        }
        if (res.result.fail) {
          api.Package.byName(pkgName).Report.latest().Cell.byId(ident).get
            ( function success (r) { setupFailTabs(ghcVer, pkgName, packageVersion, r.resultA.result.fail); }
            , function fail () { console.warn("Couldn't find cell data for " + ident, arguments); }
            );
        }
        else if (res.result.failDeps) {
          api.Package.byName(pkgName).Report.latest().Cell.byId(ident)
             .get( function success (s) { setupFailDepsTabs(ghcVersion, s.resultA.result.failDeps); }
                 , function fail    (f) { console.warn("Couldn't load cell data failed for " + ident, arguments) }
                 );
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

  function hdiffUrl (pkgName, versionName, prevVersionName) {
    if (prevVersionName) {
      return "http://hdiff.luite.com/cgit/" + pkgName + "/diff"
        + "?id=" + versionName
        + "&id2=" + prevVersionName;
    } else {
      return "http://hdiff.luite.com/cgit/" + pkgName + "/commit?id=" + versionName
    }
    return "http://hdiff.luite.com/cgit/" + pkgName + "/diff?id=" + versionName
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

  function renderTag (tagName) {
    return $("<a>").addClass("tag-item").attr("data-tag-name", tagName).text(tagName);
  }

})();
