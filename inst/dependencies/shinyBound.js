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

let trackDependency = function(object) {
  // object.getRootNode().host.dataset.loadedDependencies = parseInt(object.getRootNode().host.dataset.loadedDependencies) + 1
  // object.getRootNode().host.isFinishedLoading();
}

if (typeof registerCustomComponent != 'function') {
  let registerCustomComponent = function(message) {
    let classtag = document.createElement('script')

    Object.entries(message.class.attribs).forEach(([key, value]) => {
      classtag[[key]] = value
    })

    let bindingtag = document.createElement('script')

    Object.entries(message.binding.attribs).forEach(([key, value]) => {
      bindingtag[[key]] = value
    })

    document.head.appendChild(classtag)
    document.head.appendChild(bindingtag)
  }

  Shiny.addCustomMessageHandler('registerCustomComponent', registerCustomComponent)
}