import * as Ed from "./editor";

export let globals: {
  editor: Ed.Editor;
};

window.addEventListener("load", () => {
  console.log("loading window");
  const showRcvrTypesButton = document.getElementById(
    "showReceiverTypes"
  ) as HTMLInputElement | null;
  // Keybindings should provide more than VIM or nothing so this should be a dropdown.
  const vimKeybindingToggle = document.getElementById(
    "vimKeybindings"
  ) as HTMLInputElement | null;
  const editorElement = document.querySelector(
    ".static-editor"
  ) as HTMLElement | null;

  if (
    showRcvrTypesButton == null ||
    editorElement == null ||
    vimKeybindingToggle == null
  ) {
    throw new Error(
      "document elements cannot be null (TODO there must be a better way to handle this)"
    );
  }

  globals = {
    editor: new Ed.Editor(editorElement, [
      Ed.receiverPermissionsField.stateField,
    ]),
  };

  let toggleVim = (_: any) => {
    globals.editor.toggleVim(vimKeybindingToggle?.checked);
  };

  // Set the initial state of the VIM bindings
  toggleVim(0);

  vimKeybindingToggle.addEventListener("click", toggleVim);

  showRcvrTypesButton.addEventListener("click", _ => {
    return globals.editor.computeReceiverPermissions();
  });
});
