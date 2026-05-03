*** Settings ***
Documentation    Real-browser smoke test: execCommand('insertText') preserves
...              the native Ctrl+Z undo stack in current Chromium. This is the
...              one thing jsdom can't validate, and the entire `replaceRange`
...              wrapper relies on it.
Library          Browser

Suite Setup      Setup Browser And Page
Suite Teardown   Close Browser

*** Variables ***
${BASE_URL}      http://localhost:8001
${TENSION_PATH}  /tension/f6/0x35e205

*** Keywords ***
Setup Browser And Page
    New Browser    chromium    headless=True
    New Page       ${BASE_URL}${TENSION_PATH}
    Wait For Elements State    css=body    visible    timeout=5s

Inject Textarea
    [Documentation]    Inject a textarea into the page and focus it.
    Evaluate JavaScript    *
    ...    const ta = document.createElement('textarea');
    ...    ta.id = 'undoTestTa';
    ...    ta.value = '';
    ...    document.body.appendChild(ta);
    ...    ta.focus();

*** Test Cases ***
ExecCommand InsertText Preserves Native Undo Stack
    [Documentation]    Type text via execCommand, mutate via execCommand again,
    ...    then issue undo. The buffer must roll back to the prior state — that
    ...    proves Ctrl+Z still walks the same stack our `replaceRange` writes to.

    Inject Textarea

    # Step 1: type "hello" via execCommand. This is the same primitive replaceRange uses.
    Evaluate JavaScript    *
    ...    const ta = document.getElementById('undoTestTa');
    ...    ta.focus();
    ...    document.execCommand('insertText', false, 'hello');

    ${after_type}=    Evaluate JavaScript    *    document.getElementById('undoTestTa').value
    Should Be Equal    ${after_type}    hello

    # Step 2: append " world" via execCommand. Two distinct edits in the undo stack.
    Evaluate JavaScript    *
    ...    const ta = document.getElementById('undoTestTa');
    ...    ta.focus();
    ...    ta.setSelectionRange(ta.value.length, ta.value.length);
    ...    document.execCommand('insertText', false, ' world');

    ${after_append}=    Evaluate JavaScript    *    document.getElementById('undoTestTa').value
    Should Be Equal    ${after_append}    hello world

    # Step 3: undo. Must revert the last edit (" world"), leaving "hello".
    Evaluate JavaScript    *    document.getElementById('undoTestTa').focus()
    Keyboard Key    press    Control+z

    ${after_undo}=    Evaluate JavaScript    *    document.getElementById('undoTestTa').value
    Should Be Equal    ${after_undo}    hello
