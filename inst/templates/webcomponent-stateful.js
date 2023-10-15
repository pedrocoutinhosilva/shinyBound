class {{className}} extends HTMLElement {
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
    ".html-widget-output"
  ]

  // Default state for the element defined when defining the component
  initialState = {{initialState}};

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
  loadComplete = false;

  constructor() {
    super();

    this.dataset.numberDependencies = {{numberDependencies}};
    this.dataset.loadedDependencies = 0;

    this.isFinishedLoading = function() {

      let rootElement = this.shadowRoot.querySelector(".shinywc-root");

      if (parseInt(this.dataset.numberDependencies) == parseInt(this.dataset.loadedDependencies)) {
        this.classList.remove("shinywc-shine");
        rootElement.style.opacity = 1;
      } else {
        this.classList.add("shinywc-shine");
        rootElement.style.opacity = 0;
      }
    }

    // HTML content for the web component. Runs in open JS mode to allow
    // external javascript code (including shiny code) to access the shadow DOM
    const shadowRoot = this.attachShadow({mode: 'open'});
    shadowRoot.innerHTML = `<div class = "shinywc-root">{{innerHTML}}</div>`;

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
          element[`on${event.includes(':') ? event.split(':').shift() : event}`] = event => {
            this.eventCounters[event.type] = this.eventCounters.hasOwnProperty(event.type)
              ? this.eventCounters[event.type] + 1
              : 1
            if (this.loadComplete) {
              this.dispatchEvent(this.updatedEvent);
            }
          };
        })
    })

    // Sets the inital state of the component based on the given initialState
    this.setState(this.initialState);
  }

  // Triggered when the element is added to the page
  connectedCallback() {
    //console.log('Custom element added to page.');

    let rootElement = this.shadowRoot.querySelector(".shinywc-root");

    if (this.dataset.numberDependencies != 0) {
      this.classList.add("shinywc-shine");
      rootElement.style.opacity = 0;
    }
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
      this.dispatchEvent(this.updatedEvent);
    }
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

  ///////////////////
  // Helper functions
  ///////////////////

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
    const bindings = ["property", "attribute", "style", "class"]
      .reduce((elementList, attribute) => {
        elementList[attribute] = this.getBoundElements(attribute, prop);
        return elementList;
      }, {});

    Object.entries(bindings).map(([type, elements]) => {
      elements.map(node => {
        const dataProp = node.dataset[`fromShiny${this.capitalize(type)}`]
          .split("|")
          .filter(partial => partial.endsWith(prop));

        for (const index in dataProp) {
          let singleProp = dataProp[index];

          const bindProp = singleProp.includes(':') ? singleProp.split(':').shift() : singleProp;
          const bindValue = singleProp.includes('.') ? singleProp.split('.').slice(1).reduce((obj, p) => obj[p], value) : value;
          const target = [...this.selectAll(node.tagName)].find(el => el === node);

          const isStateUpdate = singleProp.includes(':') && this.isCustomElement(target);

          //console.log([bindProp, bindValue, target])
          //console.log([type == "attribute", bindProp == "disabled", !bindValue])

          if (type == "attribute" && bindProp == "disabled" && !bindValue) {
            bindValue == "";

            node.removeAttribute(bindProp);

            break;
          }

          switch(type) {
            case "property":
              isStateUpdate ? target.setState({[`${bindProp}`]: bindValue}) :
                this.isArray(bindValue)
                  ? target[bindProp] = bindValue
                  : node[bindProp] = bindValue;
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
              console.log(node.currentTimestap)

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
    return typeof(customElement) === "undefined" ? false : Object.getPrototypeOf(customElements.get(element.tagName.toLowerCase())).name === '{{className}}';
  }

  // Returns a given string with the first letter uppercased
  capitalize(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
  }

  // Returns a given string with the first letter uppercased
  scopedCallback(callback) {
    callback.parseFunction().bind(this.shadowRoot)()
  }
}

if (!customElements.get('{{tagName}}')) {
  customElements.define('{{tagName}}', {{className}});
}
