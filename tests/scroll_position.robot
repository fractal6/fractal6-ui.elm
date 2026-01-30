*** Settings ***
Documentation     Test scroll-to-top button functionality on tension page
Library           Browser

Suite Setup       Setup Browser And Page
Suite Teardown    Close Browser

*** Variables ***
${BASE_URL}       http://localhost:8001
${TENSION_PATH}   /tension/f6/0x35e205

*** Keywords ***
Setup Browser And Page
    New Browser    chromium    headless=True
    New Page       ${BASE_URL}${TENSION_PATH}

*** Test Cases ***
Scroll To Top Button Returns Page To Top
    [Documentation]    Verify clicking scroll-to-top button scrolls page back to top
    # Wait for page to load
    Wait For Elements State    css=.tensionTitle    visible    timeout=5s

    # Scroll down 500px
    Evaluate JavaScript    *    window.scrollTo(0, 500)
    Sleep    1s

    # Verify we scrolled down
    ${scroll_y}=    Evaluate JavaScript    *    window.scrollY
    Should Be True    ${scroll_y} > 0    Page should have scrolled down

    # Verify required elements exist
    Wait For Elements State    css=.scrollToTop    visible

    # Click the 1th scrollToTop button (nth-child selector)
    Click    css=.scrollToTop >> nth=0

    # Wait 1 second for scroll animation
    Sleep    1s

    # Verify page is back at top
    ${final_scroll}=    Evaluate JavaScript    *    window.scrollY
    Should Be Equal As Numbers    ${final_scroll}    0    Page should be at top after clicking scroll button
