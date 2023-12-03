class {{className}} extends shinyBoundWcBase {
  // Default state for the element defined when defining the component
  initialState = {{initialState}};

  onRenderCallbacks = {{onRenderCallbacks}};

  className = "{{className}}";
}

if (!customElements.get('{{tagName}}')) {
  customElements.define('{{tagName}}', {{className}});
}