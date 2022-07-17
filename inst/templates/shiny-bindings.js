var {{className}}InputBinding = new Shiny.InputBinding();
$.extend({{className}}InputBinding, {
  find: function(scope) {
    return $(scope).find("{{htmlWCTagName}}");
  },
  getValue: function(el) {
    return !!el.getState ? el.getState() : null;
  },
  setValue: function(el, value) {
    return !!el.setState ? el.setState(value) : null;
  },
  receiveMessage: function(el, data) {
    return !!el.setState ? el.setState(data) : null;
  },
  subscribe: function(el, callback) {
    el.addEventListener("updated", (event) => {callback(); })
  },
  unsubscribe: function(el) {
    el.cleanSecondaryInputs();
    $(el).off('.{{className}}InputBinding');
  }
});
Shiny.inputBindings.register({{className}}InputBinding, 'shiny.{{className}}Input');
