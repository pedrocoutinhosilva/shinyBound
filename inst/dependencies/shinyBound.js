if (typeof String.prototype.parseFunction != 'function') {
  String.prototype.parseFunction = function () {
    var funcReg = /function *\(([^()]*)\)[ \n\t]*{(.*)}/gmi;
    var match = funcReg.exec(this.replace(/\n/g, ' '));

    if(match) {
      return new Function(match[1].split(','), match[2]);
    }

    return null;
  };
}

if (typeof shinyBoundScopedScript != 'function') {
  let shinyBoundScopedScript = function(message) {
    if (!!document.getElementById(message.id) && !!document.getElementById(message.id).scopedCallback) {
      document.getElementById(message.id).scopedCallback(message.callback)
    }
  }

  Shiny.addCustomMessageHandler('shinyBoundScopedScript', shinyBoundScopedScript)
}

var ShinyBound = {
  version: null
}
