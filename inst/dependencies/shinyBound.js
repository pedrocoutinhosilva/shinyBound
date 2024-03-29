// Global object to store information about the package
var ShinyBound = {
  version: null
}

// Allows a string to be parsed into a function
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

// Custom message to register a custom component from the server
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

class shinyBoundWcBase extends HTMLElement {
  // Prefixes used to identify HTML attributes in the component in order
  // to create all necessary binds and state updates.
  prefixes = {
    fromShiny: "fromShiny",
    toShiny: "toShiny",
    fromShinyAttribute: "from-shiny",
    toShinyAttribute: "to-shiny",
  };

  // Query selectors for template tags that should be automatically be treated as
  // slotIn elements (For compatibility with existing shiny components)
  autoSlots = [
    ".shiny-html-output",
    ".shiny-input-container",
    ".html-widget-output",
    ".shinywc-component"
  ]

  // Current state of the component, updated via the update function
  state = {};

  // Callbacks to run for removing any secondary input values in shiny.
  // Used when the component is removed by shiny.
  clearInputsCallbacks = {};

  // Tracks how many times times custom registered events get triggered
  // custom triggers force an update on the shiny side, but their value is
  // only passed to the shiny input variable if requested in the bindings
  eventCounters = {};

  // Used to skip the first update trigger, to avoid init triggers on the shiny side
  loadComplete = true;

  // Templates that are repeated based on a state value
  repeaterTemplates = {};

  constructor() {
    super();

    // HTML content for the web component. Runs in open JS mode to allow
    // external javascript code (including shiny code) to access the shadow DOM
    const shadowRoot = this.attachShadow({mode: 'open'});
    shadowRoot.innerHTML = `<div class = "shinywc-root"></div>`;
  }

  connectedCallback() {
    let rootElement = this.shadowRoot.querySelector(".shinywc-root");

    if (document.body.querySelector("#shinyBoundDeps") == null) {
      let depContainer = document.createElement("div")
      depContainer.id = "shinyBoundDeps"

      document.body.append(depContainer)
    }

    if (this.parentNode.querySelector(`#${this.className}`) != null) {
      document.body.querySelector("#shinyBoundDeps")
        .append(this.parentNode.querySelector(`#${this.className}`).cloneNode(true))
      this.parentNode.querySelector(`#${this.className}`).remove()
    }

    this.parentNode.querySelectorAll("[package=shinyBound]").forEach(dep => {
      document.body.querySelector("#shinyBoundDeps").append(dep.cloneNode(true))
      dep.remove()
    })

    this.shadowRoot.querySelector(".shinywc-root")
      .appendChild(document.querySelector(`#${this.className}`).content.cloneNode(true))

    // Automatically slots ui outputs from shiny
    this.autoSlots.map((selector) => [ ...this.selectAll(selector)])
      .flat()
      .forEach(node => {
        let slot = document.createElement('slot');
        let randomName = `slot${Math.random().toString().substring(2)}${new Date().getTime()}`
        slot.setAttribute('name', randomName)

        node.setAttribute('slot', randomName)
        $(node).replaceWith(slot)

        this.append(node)
      })

    // Parse repeater nodes
    this.repeaterTemplates = [...this.selectAll("[data-from-shiny-repeater]")].map(node => {
      node.setAttribute("templateId", `${Math.random().toString(36).replace('0.','sbtemplate')}${Date.now()}`)

      let template = {
        parent: node.parentElement,
        node: node
      }

      node.remove()
      return(template)
    })

    // Custom event that signals a update of the component state. Used by
    // shiny to know when the component related inputs need updating.
    // see getState() and inst/shiny.bindings.js for more info.
    this.updatedEvent = new CustomEvent("updated", {
      bubbles: true,
      cancelable: false,
      composed: true
    });

    // Locates and binds javascript events defined in the HTML of the web
    // component. Events trigger an update to shiny (if the state changed).
    // The amount of each triggered event is stored in this.eventCounters.
    [...this.selectAll(`[data-${this.prefixes.toShinyAttribute}-event]`)].map(element => {
      element.dataset.toShinyEvent
        .split("|")
        .forEach(event => {
          element.addEventListener(`${event.includes(':') ? event.split(':').shift() : event}`, function (event) {
            this.eventCounters[event.type] = this.eventCounters.hasOwnProperty(event.type)
              ? this.eventCounters[event.type] + 1
              : 1
            if (this.loadComplete) {
              // console.log("this.loadComplete")
              this.dispatchEvent(this.updatedEvent);
              event.stopPropagation();
            }
          }.bind(this), false);
        })
    })

    // Sets the inital state of the component based on the given initialState
    this.setState(this.initialState);

    // if (this.dataset.numberDependencies != 0) {
    //   this.classList.add("shinywc-shine");
    //   rootElement.style.opacity = 0;
    // }

    this.onRenderCallbacks
      .filter(isNaN)
      .map((callbackName) => {
        let temp = document.createElement("script")


        Object.entries(JSON.parse(callbackName)).forEach(entry => {
          if (entry[0] == 0) {
            temp.setAttribute("src", entry[1])
          } else {
            temp.setAttribute(entry[0], entry[1])
          }
        })

        temp.async = false

        document.body.querySelector("#shinyBoundDeps").appendChild(temp)
      })

    this.dispatchEvent(this.updatedEvent);
  }

  // Triggered when the element is removed to the page
  disconnectedCallback() {
    //console.log('Custom element removed from page.');
  }

  // Update all component values based on a given object
  setState(newState) {
    this.newState = {
      state: newState,
      timestamp: Date.now()
    };

    Object.entries(newState)
      .forEach(([key, value]) => {
        this.state[key] = this.isObject(this.state[key]) && this.isObject(value)
          ? {...this.state[key], ...value}
          : value;

        const bindKey = this.isObject(value) ? this.getBindKey(key, value) : key;
        const bindKeys = this.isArray(bindKey) ? bindKey : [bindKey];

        bindKeys.forEach(key => this.updateBindings(key, value));
    });

    if (this.loadComplete) {
      // console.log("dispatch")
      this.dispatchEvent(this.updatedEvent);
    }

    // $( document ).ready(function() {
    //   if (!!Shiny) {
    //     Shiny?.bindAll();
    //   }
    // });


  }

  // Parses and returns a single object with values representing the web component
  // state based on all the bindings defined in the component HTML
  // this object is sent to the corresponding shiny input and will be accessible
  // under input${inputId}. In addition, for each key in the return object, a
  // secondary shiny input is also updated and can be observed in shiny as a
  // reactive value under input${inputId}_{key}
  getState() {
    let returnState = {}

    // Find all elements in the web component that have binding attributes for
    // each of the binding types
    const bindings = ["property", "attribute", "style", "class", "event"]
      .reduce((elementList, attribute) => {
        elementList[attribute] = this.getToShinyElements(attribute);
        return elementList;
      }, {});

    // Read values for property bindings
    bindings.property.map(component => {
      component.dataset[`toShinyProperty`].split("|").forEach(function(dataProp) {
        let {bindProp, bindValue} = this.getPropBinds(dataProp);

        returnState[bindValue] = component[bindProp]
      }, this)
    })

    // Read values for attribute bindings
    bindings.attribute.map(component => {
      component.dataset[`toShinyAttribute`].split("|").forEach(function(dataProp) {
        let {bindProp, bindValue} = this.getPropBinds(dataProp);

        returnState[bindValue] = component.getAttribute(bindProp);
      }, this)
    })

    // Read values for style bindings
    bindings.style.map(component => {
      component.dataset[`toShinyStyle`].split("|").forEach(function(dataProp) {
        let {bindProp, bindValue} = this.getPropBinds(dataProp);

        returnState[bindValue] = component.style[bindProp];
      }, this)
    })

    // Read values for class bindings
    bindings.class.map(component => {
      component.dataset[`toShinyClass`].split("|").forEach(function(dataProp) {
        let {bindProp, bindValue} = this.getPropBinds(dataProp);

        returnState[bindValue] = component.classList.contains(bindProp);
      }, this)
    })

    // Read values for event bindings
    bindings.event.map(component => {
      component.dataset[`toShinyEvent`].split("|").forEach(function(dataProp) {
        // Events usually only trigger state updates. If a return key was not
        // specified for a given event, it is not added to the return values.
        if (!dataProp.includes(':')) return false;

        let {bindProp, bindValue} = this.getPropBinds(dataProp);

        // For events never triggered, the value is set to null to keep in line
        // with the expected reactive behavior of shiny inputs
        returnState[bindValue] = this.eventCounters.hasOwnProperty(bindProp)
          ? this.eventCounters[bindProp]
          : null
      }, this)
    })

    // Updates secondary shiny input values for each binding.
    Object.keys(returnState).forEach(key => {
      if (!!returnState[key]) {
        setTimeout(function(){
          Shiny.setInputValue(`${this.id}_${key}`, returnState[key]);
        }.bind(this, key, returnState), 0);

        // If a specific secondary binding destruction callback is not registered,
        // register it. These callbacks are used to remove all inputs if the element
        // is removed from the UI by shiny.
        if (!this.clearInputsCallbacks[key]) this.clearInputsCallbacks[key] = function() {
          Shiny.setInputValue(`${this.id}_${key}`, null);
        }
      }
    })

    // If only one value is returned, the return object is simplified to a single value
    if (Object.keys(returnState).length == 1)
      return Object.values(returnState)

    return(returnState)
  }

  // Gets all HTML elements that need to send values back to shiny, based on a
  // given type (property, attribute, style, class, event)
  getToShinyElements(type) {
    return [...this.selectAll(`[data-${this.prefixes.toShinyAttribute}-${type}]`)]
  }

  // Helper function that parses a bind string into a set of values.
  // It expects a string in either {prop}:{value} or {prop} format.
  getPropBinds(dataProp) {
    return {
      bindProp: dataProp.includes(':') ? dataProp.split(':').shift() : dataProp,
      bindValue: dataProp.includes(':') ? dataProp.split(':')[1] : dataProp
    }
  }

  // Updates all HTML elements that read information for a specific state prop
  updateBindings(prop, value = '') {
    let repeaters = this.repeaterTemplates.filter((single) => {
      return(single.node.getAttribute("data-from-shiny-repeater") == prop)
    })

    // console.log(this.newState)

    let newState = this.newState

    repeaters.forEach(function(single) {
      let id = single.node.getAttribute("templateId")

      single.parent.querySelectorAll(`[templateId='${id}']`).forEach(node => {node.remove()})

      value.forEach(function(val, index) {
        let tempnode = single.node
        tempnode.setAttribute("data-repeater-index", index);


        let repeaterNodes = ["property", "attribute", "style", "class"]
          .reduce((elementList, attribute) => {
            let queryWrapper = document.createElement("div");
            queryWrapper.appendChild(tempnode)

            elementList[attribute] = queryWrapper.querySelectorAll(`[data-from-shiny-${attribute}*="repeater"]`);

            [...elementList[attribute]].map(node => {
              [...node.getAttribute(`data-from-shiny-${attribute}`).split("|").flat()]
                  .filter(binding => {
                      return binding.includes("repeater")
                  }, this)
                  .forEach(binding => {
                      let singlePropKey = binding.includes(':') ? binding.split(':').shift() : binding;

                      switch(attribute) {
                        case "property":

                          if (singlePropKey == 'innerHTML') {
                            val = val.toString()
                            .replace(/&lt;/g , "<")
                            .replace(/&gt;/g , ">")
                            .replace(/&quot;/g , "\"")
                            .replace(/&#39;/g , "\'")
                            .replace(/&amp;/g , "&")
                          }

                          node[singlePropKey] = val;
                          break;
                        case "attribute":
                          node.setAttribute(singlePropKey, val.toString());
                          break;
                      }
                  }, this)
              return node
          }, this)

          return elementList;
        }, {})

        single.parent.appendChild(tempnode.cloneNode(true))
      }, this)
    }, this)

    const bindings = ["property", "attribute", "style", "class"]
      .reduce((elementList, attribute) => {
        elementList[attribute] = this.getBoundElements(attribute, prop);
        return elementList;
      }, {})

    Object.entries(bindings).map(([type, elements]) => {
      elements.map(node => {
        const dataProp = node.dataset[`fromShiny${this.capitalize(type)}`]
          .split("|")
          .filter(partial => partial.endsWith(prop));

        for (const index in dataProp) {
          let singleProp = dataProp[index];

          const bindProp = singleProp.includes(':') ? singleProp.split(':').shift() : singleProp;
          let bindValue = singleProp.includes(':') ? singleProp.split('.').slice(1).reduce((obj, p) => obj[p], value) : value;
          const target = [...this.selectAll(node.tagName)].find(el => el === node);

          const isStateUpdate = singleProp.includes(':') && this.isCustomElement(target);

          // console.log([bindProp, bindValue, target])
          //console.log([type == "attribute", bindProp == "disabled", !bindValue])

          if (type == "attribute" && bindProp == "disabled" && !bindValue) {
            bindValue == "";

            node.removeAttribute(bindProp);

            break;
          }
          if (type == "attribute" && bindProp == "selected") {
            if (!!bindValue) {
              if (bindValue.includes(node.value)) {
                node.setAttribute("selected", true)
              }
            } else {
              bindValue == "";
              node.removeAttribute(bindProp);
            }
            break;
          }
          if (type == "attribute" && bindProp == "hidden" && !bindValue) {
            bindValue == "";

            node.removeAttribute(bindProp);

            break;
          }

          switch(type) {
            case "property":
              if (bindProp == "innerHTML") {
                let dom = document.createElement('div')
                // let temp = document.createElement('div')

                // console.log(bindValue)



                bindValue = bindValue.toString()
                  .replace(/&lt;/g , "<")
                  .replace(/&gt;/g , ">")
                  .replace(/&quot;/g , "\"")
                  .replace(/&#39;/g , "\'")
                  .replace(/&amp;/g , "&")

                dom.innerHTML = bindValue

                Object.values(dom.querySelectorAll("template")).forEach((template) => {
                  document.body.querySelector("#shinyBoundDeps").appendChild(template.cloneNode(true))
                  template.remove()
                })

                let scriptTags = document.createElement("div")
                Object.values(dom.querySelectorAll("script")).forEach((script) => {
                  let scriptNode = document.createElement("script")
                  scriptNode.src = script.src

                  // console.log(scriptNode)
                  document.body.querySelector("#shinyBoundDeps").appendChild(scriptNode)
                  script.remove()
                })

                bindValue = dom.innerHTML

                // console.log(bindValue)
                // console.log(dom)
              }

              isStateUpdate ? target.setState({[`${bindProp}`]: bindValue}) :
                this.isArray(bindValue)
                  ? target[bindProp] = bindValue
                  : node[bindProp] = bindValue;
              // Automatically slots ui outputs from shiny
              this.autoSlots.map((selector) => [ ...this.selectAll(selector)])
                .flat()
                .forEach(node => {
                  let slot = document.createElement('slot');
                  let randomName = `slot${Math.random().toString().substring(2)}${new Date().getTime()}`
                  slot.setAttribute('name', randomName)

                  node.setAttribute('slot', randomName)
                  $(node).replaceWith(slot)

                  this.append(node)
                })
              break;
            case "attribute":
              isStateUpdate ? target.setState({[`${bindProp}`]: bindValue}) :
                this.isArray(bindValue)
                  ? target[bindProp] = bindValue
                  : node.setAttribute(bindProp, bindValue.toString());
              break;
            case "style":
              isStateUpdate ? target.setState({[`${bindProp}`]: bindValue}) :
                this.isArray(bindValue)
                  ? target[bindProp] = bindValue
                  : node.style[bindProp] = bindValue.toString();
              break;
            case "class":
              if (typeof node.defaultClasses == "undefined") {
                node.defaultClasses = node.classList
              }

              if (node.currentTimestap != this.newState.timestamp) {
                node.currentTimestap = this.newState.timestamp

                node.stepClassList = node.defaultClasses;
              }

              node.stepClassList.add((singleProp.includes(':') ? bindProp : `{${prop}}`).replace(`{${prop}}`, bindValue));

              node.classList = node.stepClassList;
              break;
          }
        }
      })
    })

    this.autoSlots.map((selector) => [ ...this.selectAll(selector)])
    .flat()
    .forEach(node => {

      console.log(node)
      let slot = document.createElement('slot');
      let randomName = `slot${Math.random().toString().substring(2)}${new Date().getTime()}`
      slot.setAttribute('name', randomName)

      node.setAttribute('slot', randomName)
      $(node).replaceWith(slot)

      this.append(node)
    })

  }

  // Finds HTML elements that have can be updated from shiny, for a given binding type and prop.
  getBoundElements(type, prop) {
    const elementSelector = `[data-${this.prefixes.fromShinyAttribute}-${type}*="${prop}"]`;
    const elements = [...this.selectAll(elementSelector)]

    const filtered = elements.lenght > 0
      ? elements.filter(node => node.dataset[`${this.prefixes.fromShiny}${this.capitalize(type)}`]
        .split("|").filter(single => single
          .split(":").includes(prop)))
      : elements

    return(filtered)
  }

  // Iterates through all clearInputsCallbacks callbacks. These callbacks are
  // expected to clear all secondary input values in shiny. This function is
  // invoked from the web component correspondent Shiny.InputBinding's unsubscribe
  // event, called when the component is removed from the page as part of the clean up.
  cleanSecondaryInputs() {
    for (var i in this.clearInputsCallbacks) this.clearInputsCallbacks[i]();
    this.clearInputsCallbacks = {};
  }


  // If a binding is an object instead of a value, return the correct key
  getBindKey(key, obj) {
    return Object.keys(obj).map(k => this.isObject(obj[k]) ? `${key}.${this.getBindKey(k, obj[k])}` : `${key}.${k}`);
  }

  // Check if a given object type is an array
  isArray(arr) {
    return Array.isArray(arr);
  }

  // Check if a given object type is an object
  isObject(obj) {
    return Object.prototype.toString.call(obj) === '[object Object]';
  }

  // Similar to the native querySelectorAll, but searches for elements
  // only in the web component shadow DOM
  selectAll(selector) {
    return this.shadowRoot ? this.shadowRoot.querySelectorAll(selector) : this.querySelectorAll(selector);
  }

  // Checks if a given element is registered as a web component
  isCustomElement(element) {
    let customElement = customElements.get(element.tagName.toLowerCase())
    return typeof(customElement) === "undefined" ? false : Object.getPrototypeOf(customElements.get(element.tagName.toLowerCase())).name === this.className;
  }

  // Returns a given string with the first letter uppercased
  capitalize(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
  }

  // Runs a given function inside the component shadow dom
  scopedCallback(callback) {
    callback.parseFunction().bind(this.shadowRoot)()
  }
}