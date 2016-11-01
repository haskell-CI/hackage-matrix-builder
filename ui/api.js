(function (window) {

// in nodejs, global is an object
// in normal browser environment, global is undefined
// in webpack, global is set to window
var isNodeJs   = typeof global !== "undefined" && global.window === undefined;
var isCommonJs = typeof module === "object" && module && typeof module.exports === "object";

// require, that the nodejs will handle, but will remain not processed by webpack and the like
var obfuscatedRequire = function (moduleName)
{
  return module["r" + "equire"](moduleName);
}

function MatrixApi (url, secureUrl, modifyRequest)
{
  var MatrixApi =
    function (url, secureUrl, modifyRequest)
    {
      var self = this;
      var postfix          = '/v' + this.version + '/';
      var contextUrl       = url + postfix;
      var secureContextUrl = (secureUrl || url.replace(/^http:/, "https:")) + postfix;

      this.cookieJar = isNodeJs ? obfuscatedRequire('request').jar() : undefined;

      if(!modifyRequest) modifyRequest = function (req) { return req; };

      var finalModifyRequest = function (req)
      {
        if (isNodeJs) req.jar = self.cookieJar;
        return modifyRequest(req);
      }

      MatrixApi.setContext(this, contextUrl, secureContextUrl, finalModifyRequest);
    };

  var jqFun;
  if (isNodeJs)
  {
    MatrixApi.ajaxCall = nodeRequest;
  }
  else
  {
    if (isCommonJs) {
      jqFun = function () { return require("jquery"); };
    } else if (typeof define === "function" && define.amd) {
      jqFun = function () { return window.$; };
    } else {
      jqFun = function () { return window.$; };
    }

    MatrixApi.ajaxCall = jQueryRequest;
  }

  MatrixApi.addObject = function (obj1, obj2)
  {
    for (var fld in obj2)
      obj1[fld] = obj2[fld];
  };

  MatrixApi.defaultAjaxOptions = {};
  MatrixApi.defaultHeaders = {};

  function jQueryRequest (method, url, params, success, error, contentType, acceptHeader, data, callOpts, modifyRequest)
  {
    var jq = jqFun();

    var headers = jq.extend(true, {}, MatrixApi.defaultHeaders);
    MatrixApi.addObject(headers, { Accept : acceptHeader });

    var callData =
      { type        : method
      , url         : url + (params ? '?' + jq.param(params) : '')
      , cache       : false
      , success     : success || function () {}
      , error       : error || function () {}
      , contentType : contentType
      , headers     : headers
      , xhrFields   : { withCredentials: true }
      , data        : data || []
      };

    callData = modifyRequest(callData);

    MatrixApi.addObject(callData, MatrixApi.defaultAjaxOptions);
    MatrixApi.addObject(callData, callOpts);

    return jq.ajax(callData);
  }

  function nodeRequest (method, url, params, onSuccess, onError, contentType, acceptHeader, data, callOpts, modifyRequest)
  {
    var allParams = {};
    MatrixApi.addObject(allParams, params);

    if (method === "GET" || method === "HEAD")
      // Avoid cached API responses.
      allParams._ = Date.now();

    var headers = { "Content-type" : contentType
                  , "Accept"       : acceptHeader
                  };

    MatrixApi.addObject(headers, MatrixApi.defaultHeaders);

    var callData =
      { url     : url
      , qs      : allParams
      , method  : method
      , headers : headers
      };

    if (data) callData.body = data;

    callData = modifyRequest(callData);

    MatrixApi.addObject(callData, MatrixApi.defaultAjaxOptions);
    MatrixApi.addObject(callData, callOpts);

    return require("q").Promise(function (resolve, reject)
    {
      obfuscatedRequire("request")(callData, callback);

      function callback (error, message, body)
      {
        if (message && message.statusCode >= 200 && message.statusCode < 300)
        {
          var parsedResponse = parse(body, message.headers);
          onSuccess && onSuccess(parsedResponse, message);
          resolve(parsedResponse)
        }
        else
        {
          if (!error)
          {
            error = new Error("HTTP request error");
            error.statusCode = message && message.statusCode;
            error.responseBody = body;
          }

          error.response = parse(body, message ? message.headers : {});

          if (onError)
            onError(error);

          reject(error);
        }
      }
    });

    function parse (response, headers)
    {
      if (headers["content-type"] && headers["content-type"].split(";").indexOf("application/json") >= 0)
      {
        var r = response;
        try
        {
          r = JSON.parse(response);
        }
        catch (e)
        {
          return r;
        }
        return r;
      }
      else return response;
    }
  }

  MatrixApi.setContext =
    function (obj, url, secureUrl, modifyRequest)
    {
      obj.contextUrl = url;
      obj.secureContextUrl = secureUrl;
      obj.modifyRequest = modifyRequest;
      for (var fld in obj)
      {
        if (obj[fld] != undefined && obj[fld].apiObjectType != undefined && obj[fld].apiObjectType == 'resourceDir')
        {
          var postfix = fld.replace(/([a-z0-9])([A-Z])/g, '$1-$2').toLowerCase() + '/';
          MatrixApi.setContext(obj[fld], url + postfix, secureUrl + postfix, modifyRequest);
        }
      }
    };
MatrixApi.prototype.version = "1.0.0";
MatrixApi.prototype.Package =
  function Package (url, secureUrl, modifyRequest)
  {
    if (this instanceof Package)
    {
      MatrixApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return Package.access(url, secureUrl, modifyRequest);
    }
  };
MatrixApi.prototype.Package.apiObjectType = "resourceDir";
MatrixApi.prototype.Package.byName =
  function (string)
  {
    var postfix = 'name/' + encodeURIComponent(string) + '/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix, this.modifyRequest);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return MatrixApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
      };
    return accessor;
  };
MatrixApi.prototype.Package.list =
  function (success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("POST", this.contextUrl + 'list/', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Package.listLatestReports =
  function (success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("POST", this.contextUrl + 'list-latest-reports/', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Package.prototype.tags =
  function (success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("POST", this.contextUrl + 'tags/', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Package.prototype.Report =
  function Report (url, secureUrl, modifyRequest)
  {
    if (this instanceof Report)
    {
      MatrixApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return Report.access(url, secureUrl, modifyRequest);
    }
  };
MatrixApi.prototype.Package.prototype.Report.apiObjectType = "resourceDir";
MatrixApi.prototype.Package.prototype.Report.latest =
  function ()
  {
    var postfix = 'latest/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix, this.modifyRequest);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return MatrixApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
      };
    return accessor;
  };
MatrixApi.prototype.Package.prototype.Report.prototype.Cell =
  function Cell (url, secureUrl, modifyRequest)
  {
    if (this instanceof Cell)
    {
      MatrixApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return Cell.access(url, secureUrl, modifyRequest);
    }
  };
MatrixApi.prototype.Package.prototype.Report.prototype.Cell.apiObjectType = "resourceDir";
MatrixApi.prototype.Package.prototype.Report.prototype.Cell.byId =
  function (string)
  {
    var postfix = 'id/' + encodeURIComponent(string) + '/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix, this.modifyRequest);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return MatrixApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
      };
    return accessor;
  };
MatrixApi.prototype.Queue =
  function Queue (url, secureUrl, modifyRequest)
  {
    if (this instanceof Queue)
    {
      MatrixApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return Queue.access(url, secureUrl, modifyRequest);
    }
  };
MatrixApi.prototype.Queue.apiObjectType = "resourceDir";
MatrixApi.prototype.Queue.byName =
  function (string)
  {
    var postfix = 'name/' + encodeURIComponent(string) + '/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix, this.modifyRequest);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return MatrixApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
      };
    return accessor;
  };
MatrixApi.prototype.Queue.saveByName =
  function (string, json, success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("PUT", this.contextUrl + 'name/' + encodeURIComponent(string) + '/', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Queue.saveManyByName =
  function (json, success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("PUT", this.contextUrl + 'name/', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Queue.removeManyByName =
  function (json, success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("DELETE", this.contextUrl + 'name/', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Queue.list =
  function (success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("POST", this.contextUrl + 'list/', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Queue.create =
  function (json, success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("POST", this.contextUrl + '', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Queue.prototype.remove =
  function (success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("DELETE", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Tag =
  function Tag (url, secureUrl, modifyRequest)
  {
    if (this instanceof Tag)
    {
      MatrixApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return Tag.access(url, secureUrl, modifyRequest);
    }
  };
MatrixApi.prototype.Tag.apiObjectType = "resourceDir";
MatrixApi.prototype.Tag.byName =
  function (string)
  {
    var postfix = 'name/' + encodeURIComponent(string) + '/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix, this.modifyRequest);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return MatrixApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
      };
    return accessor;
  };
MatrixApi.prototype.Tag.saveByName =
  function (string, json, success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("PUT", this.contextUrl + 'name/' + encodeURIComponent(string) + '/', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Tag.saveManyByName =
  function (json, success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("PUT", this.contextUrl + 'name/', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Tag.removeManyByName =
  function (json, success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("DELETE", this.contextUrl + 'name/', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Tag.list =
  function (success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("POST", this.contextUrl + 'list/', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
  };
MatrixApi.prototype.Tag.prototype.remove =
  function (json, success, error, params, callOpts)
  {
    return MatrixApi.ajaxCall("DELETE", this.contextUrl + '', params, success, error, "text/json", "text/json", JSON.stringify(json), callOpts, this.modifyRequest);
  };
MatrixApi.prototype.User =
  function User (url, secureUrl, modifyRequest)
  {
    if (this instanceof User)
    {
      MatrixApi.setContext(this, url, secureUrl, modifyRequest);
    }
    else
    {
      return User.access(url, secureUrl, modifyRequest);
    }
  };
MatrixApi.prototype.User.apiObjectType = "resourceDir";
MatrixApi.prototype.User.byName =
  function (string)
  {
    var postfix = 'name/' + encodeURIComponent(string) + '/';
    var accessor = new this(this.contextUrl + postfix, this.secureContextUrl + postfix, this.modifyRequest);
    accessor.get =
      function (success, error, params, callOpts)
      {
        return MatrixApi.ajaxCall("GET", this.contextUrl + '', params, success, error, "text/plain", "text/json", undefined, callOpts, this.modifyRequest);
      };
    return accessor;
  };  return new MatrixApi (url, secureUrl, modifyRequest);
}

var jqFun;
if (isNodeJs)
{
  // Export as Node module.
  module.exports = MatrixApi;
}
else
{
  if (isCommonJs) {
    // Export as CommonJs
    module.exports = MatrixApi;
  } else if (typeof define === "function" && define.amd) {
    // Export as AMD.
    define("MatrixApi", [], function () { return MatrixApi; });
  } else {
    // Export as global.
    window.MatrixApi = MatrixApi;
  }
}

})(this);