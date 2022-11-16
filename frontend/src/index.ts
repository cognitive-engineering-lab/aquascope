import * as Ed from "./editor";

export let globals: {
  editor: Ed.Editor;
};

window.addEventListener("load", () => {
  console.log("loading window");
  const show_rcvr_types_toggle = document.getElementById(
    "show_receiver_types"
  ) as HTMLInputElement | null;
  // Keybindings should provide more than VIM or nothing so this should be a dropdown.
  const vim_keybinding_toggle = document.getElementById(
    "vim_keybindings"
  ) as HTMLInputElement | null;
  const editor_element = document.querySelector(
    ".static-editor"
  ) as HTMLElement | null;

  if (
    show_rcvr_types_toggle == null ||
    editor_element == null ||
    vim_keybinding_toggle == null
  ) {
    throw new Error(
      "document elements cannot be null (TODO there must be a better way to handle this)"
    );
  }

  globals = {
    editor: new Ed.Editor(editor_element, [
      Ed.receiverPermissionsField.stateField,
    ]),
  };

  vim_keybinding_toggle.addEventListener("click", _ => {
    globals.editor.toggleVim(vim_keybinding_toggle?.checked);
  });

  show_rcvr_types_toggle.addEventListener("click", _ => {
    return globals.editor.computeReceiverPermissions();
  });
});
