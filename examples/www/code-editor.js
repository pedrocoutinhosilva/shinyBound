[...document.querySelectorAll(".code-editor")].map((node) => {
  let defaultCode = node.innerHTML
    .replace(/&lt;/g , "<")
    .replace(/&gt;/g , ">")
    .replace(/&quot;/g , "\"")
    .replace(/&#39;/g , "\'")
    .replace(/&amp;/g , "&");
  node.innerHTML = "";
  node.style.height = "fit-content";

  let runButton = document.createElement("button");
  let outputDiv = document.createElement("div");
  let editorDiv = document.createElement("div");

  node.appendChild(editorDiv);
  node.appendChild(runButton);
  node.appendChild(outputDiv);

  runButton.innerHTML = "Regenerate Widget";

  var myModeSpec = {
    name: "htmlmixed",
    tags: {
      style: [["type", /^text\/(x-)?scss$/, "text/x-scss"],
              [null, null, "css"]],
      custom: [[null, null, "customMode"]]
    }
  }

  const editor = CodeMirror((elt) => {
      elt.style.border = "1px solid";
      elt.style.height = "calc(50vh - 150px)";
      editorDiv.append(elt);
    }, {
      value: defaultCode,
      lineNumbers: true,
      mode: myModeSpec,
      theme: "light default",
      viewportMargin: Infinity,
    });

  var totalLines = editor.lineCount();

  Shiny.addCustomMessageHandler("code", function(message) {
    editor.setValue(message);
    //editor.autoFormatRange({line:0, ch:0}, {line:totalLines});
    runButton.click();
  });

  runButton.onclick = async () => {
    Shiny.setInputValue("code", editor.getValue())
  }
})