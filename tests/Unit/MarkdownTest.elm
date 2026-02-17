module Unit.MarkdownTest exposing (..)

import Expect
import Markdown exposing (escapeAmpersandsInHtmlBlocks, frac6Parser, parseMarkdown, processOutsideCodeBlocks)
import Test exposing (..)


{-| Helper: check that parseMarkdown succeeds (no parse error)
-}
expectParseOk : String -> String -> Test
expectParseOk label input =
    test label <|
        \_ ->
            case parseMarkdown input of
                Ok _ ->
                    Expect.pass

                Err err ->
                    Expect.fail ("Parse failed: " ++ err)



-- ────────────────────────────────────────────────────
-- 1. escapeAmpersandsInHtmlBlocks (unit tests)
-- ────────────────────────────────────────────────────


escapeAmpersandsTests : Test
escapeAmpersandsTests =
    describe "escapeAmpersandsInHtmlBlocks"
        [ test "bare & inside <details> is escaped" <|
            \_ ->
                escapeAmpersandsInHtmlBlocks "<details>\n<summary>T</summary>\nhttps://x.com?a=1&b=2\n</details>"
                    |> Expect.equal "<details>\n<summary>T</summary>\nhttps://x.com?a=1&amp;b=2\n</details>"
        , test "bare & outside <details> is NOT escaped" <|
            \_ ->
                escapeAmpersandsInHtmlBlocks "hello &world"
                    |> Expect.equal "hello &world"
        , test "already-escaped &amp; is NOT double-escaped" <|
            \_ ->
                escapeAmpersandsInHtmlBlocks "<details>\n&amp; &lt; &#123;\n</details>"
                    |> Expect.equal "<details>\n&amp; &lt; &#123;\n</details>"
        , test "& inside code fence within <details> IS escaped" <|
            \_ ->
                escapeAmpersandsInHtmlBlocks "<details>\n<summary>Code</summary>\n\n```\na=1&b=2\n```\n\n</details>"
                    |> Expect.equal "<details>\n<summary>Code</summary>\n\n```\na=1&amp;b=2\n```\n\n</details>"
        , test "HTML tags in code fence within <details> are escaped" <|
            \_ ->
                escapeAmpersandsInHtmlBlocks "<details>\n<summary>T</summary>\n\n```\n<details> <summary>x</summary> </details>\n```\n\n</details>"
                    |> Expect.equal "<details>\n<summary>T</summary>\n\n```\n&lt;details> &lt;summary>x&lt;/summary> &lt;/details>\n```\n\n</details>"
        , test "<details> inside a code fence does NOT change depth" <|
            \_ ->
                escapeAmpersandsInHtmlBlocks "```\n<details>\n&foo\n</details>\n```\n&bar"
                    |> Expect.equal "```\n<details>\n&foo\n</details>\n```\n&bar"
        , test "nested <details> blocks" <|
            \_ ->
                escapeAmpersandsInHtmlBlocks "<details>\nouter &x\n<details>\ninner &y\n</details>\nstill &z\n</details>\noutside &w"
                    |> Expect.equal "<details>\nouter &amp;x\n<details>\ninner &amp;y\n</details>\nstill &amp;z\n</details>\noutside &w"
        , test "& inside <div> is escaped" <|
            \_ ->
                escapeAmpersandsInHtmlBlocks "<div>\n&foo\n</div>"
                    |> Expect.equal "<div>\n&amp;foo\n</div>"
        , test "single-line <div> with &" <|
            \_ ->
                escapeAmpersandsInHtmlBlocks "<div>&test</div>"
                    |> Expect.equal "<div>&amp;test</div>"
        ]



-- ────────────────────────────────────────────────────
-- 2. processOutsideCodeBlocks (unit tests)
-- ────────────────────────────────────────────────────


processOutsideCodeBlocksTests : Test
processOutsideCodeBlocksTests =
    describe "processOutsideCodeBlocks"
        [ test "transforms content outside code fences" <|
            \_ ->
                processOutsideCodeBlocks String.toUpper "hello\n```\ncode\n```\nworld"
                    |> Expect.equal "HELLO\n```\ncode\n```\nWORLD"
        , test "code fence content is unchanged" <|
            \_ ->
                processOutsideCodeBlocks (\s -> String.replace "_" "\\_" s) "text_here\n```\ncode_here\n```"
                    |> Expect.equal "text\\_here\n```\ncode_here\n```"
        , test "tilde fences work too" <|
            \_ ->
                processOutsideCodeBlocks String.toUpper "before\n~~~\ncode\n~~~\nafter"
                    |> Expect.equal "BEFORE\n~~~\ncode\n~~~\nAFTER"
        , test "no code blocks → entire content transformed" <|
            \_ ->
                processOutsideCodeBlocks String.toUpper "hello world"
                    |> Expect.equal "HELLO WORLD"
        , test "only code block → nothing transformed" <|
            \_ ->
                processOutsideCodeBlocks String.toUpper "```\nhello\n```"
                    |> Expect.equal "```\nhello\n```"
        , test "multiple code blocks" <|
            \_ ->
                processOutsideCodeBlocks String.toUpper "a\n```\nb\n```\nc\n```\nd\n```\ne"
                    |> Expect.equal "A\n```\nb\n```\nC\n```\nd\n```\nE"
        ]



-- ────────────────────────────────────────────────────
-- 3. frac6Parser (unit tests)
-- ────────────────────────────────────────────────────


frac6ParserTests : Test
frac6ParserTests =
    describe "frac6Parser"
        [ test "escapes & inside <details>" <|
            \_ ->
                frac6Parser "<details>\n<summary>T</summary>\n\nhttps://x.com?a=1&b=2\n\n</details>"
                    |> String.contains "&amp;b=2"
                    |> Expect.equal True
        , test "does NOT escape & outside <details>" <|
            \_ ->
                frac6Parser "hello &world"
                    |> String.contains "&amp;"
                    |> Expect.equal False
        , test "does not modify content inside code fences" <|
            \_ ->
                let
                    result =
                        frac6Parser "```\nsome_url http://a.com\n```"
                in
                -- escapeLinks should NOT have escaped the _ inside the fence
                Expect.equal False (String.contains "\\_" result)
        ]



-- ────────────────────────────────────────────────────
-- 4. Full parseMarkdown pipeline (integration tests)
--    These verify the entire chain doesn't crash.
-- ────────────────────────────────────────────────────


parseMarkdownTests : Test
parseMarkdownTests =
    describe "parseMarkdown (integration — should not crash)"
        [ -- Basic details
          expectParseOk "basic details block"
            "<details>\n<summary>Title</summary>\n\nContent here.\n\n</details>"

        -- URL with & inside details
        , expectParseOk "URL with & inside details"
            "<details>\n<summary>Links</summary>\n\nhttps://example.com?a=1&bar=2&baz=3\n\n</details>"

        -- Markdown link with & inside details
        , expectParseOk "markdown link with & inside details"
            "<details>\n<summary>Links</summary>\n\n[Google](https://www.google.com/search?q=hello&lang=en)\n\n</details>"

        -- Already escaped entities (no double-escape crash)
        , expectParseOk "already escaped entities inside details"
            "<details>\n<summary>Entities</summary>\n\n&amp; and &lt;tag&gt; and &#123;\n\n</details>"

        -- Code block inside details
        , expectParseOk "code block inside details"
            "<details>\n<summary>Code</summary>\n\n```python\nurl = \"https://example.com?a=1&b=2\"\nhtml = \"<summary>not real</summary>\"\nif x & y:\n    print(\"test\")\n```\n\n</details>"

        -- Fenced code block with details tags (outside details)
        , expectParseOk "fenced code with <details> tags outside details"
            "```html\n<details>\n<summary>Inside code</summary>\n&foo &bar\n</details>\n```"

        -- Inline backtick with tags
        , expectParseOk "inline backtick with tags"
            "Use `<details>` and `<summary>` tags."

        -- Nested details
        , expectParseOk "nested details"
            "<details>\n<summary>Outer</summary>\n\nhttps://example.com?x=1&y=2\n\n<details>\n<summary>Inner</summary>\n\nhttps://example.com?a=1&nested=true\n\n</details>\n\n</details>"

        -- Details with div inside
        , expectParseOk "details with div inside"
            "<details>\n<summary>Mixed</summary>\n\n<div>\n\nhttps://example.com?div=1&test=2\n\n</div>\n\n</details>"

        -- Bare & outside HTML blocks (should still work)
        , expectParseOk "bare & outside HTML blocks"
            "Normal text with https://example.com?a=1&b=2\n\nAT&T and R&D."

        -- Details with blockquote
        , expectParseOk "details with blockquote"
            "<details>\n<summary>Quote</summary>\n\n> Quote with https://example.com?q=1&lang=en\n\n</details>"

        -- Single-line div with &
        , expectParseOk "single line div"
            "<div>&test https://example.com?a=1&b=2</div>"

        -- Code fence with ~~~ inside details
        , expectParseOk "tilde code fence inside details"
            "<details>\n<summary>Tilde</summary>\n\n~~~js\nconst url = \"https://example.com?a=1&b=2\";\n~~~\n\n</details>"

        -- Multiple details blocks in sequence
        , expectParseOk "multiple details in sequence"
            "<details>\n<summary>First</summary>\n\nhttps://one.com?a=1&b=2\n\n</details>\n\n<details>\n<summary>Second</summary>\n\nhttps://two.com?c=3&d=4\n\n</details>"

        -- Empty details
        , expectParseOk "empty details"
            "<details>\n<summary>Empty</summary>\n\n</details>"

        -- Uppercase hex entity inside details (elm-markdown rejects uppercase)
        , expectParseOk "uppercase hex entity inside details"
            "<details>\n<summary>Hex</summary>\n\n&#x1F4A9;\n\n</details>"

        -- Case 4 from original test doc: code block with HTML tags and & inside details
        , expectParseOk "case4: code block with details tags and & inside details"
            (String.join "\n"
                [ "<details>"
                , "<summary>Code example</summary>"
                , ""
                , "```python"
                , "# This <details> tag should NOT be parsed"
                , "url = \"https://example.com?a=1&b=2\""
                , "html = \"<summary>not a real tag</summary>\""
                , "if x & y:"
                , "    print(\"ampersand in code\")"
                , "```"
                , ""
                , "</details>"
                ]
            )

        -- Case 4 variants: isolate what breaks
        , expectParseOk "case4-minimal: code block with just & inside details"
            (String.join "\n"
                [ "<details>"
                , "<summary>Code</summary>"
                , ""
                , "```"
                , "a=1&b=2"
                , "```"
                , ""
                , "</details>"
                ]
            )
        , expectParseOk "case4-no-html-in-code: code block without HTML tags inside details"
            (String.join "\n"
                [ "<details>"
                , "<summary>Code</summary>"
                , ""
                , "```python"
                , "url = \"https://example.com?a=1&b=2\""
                , "if x & y:"
                , "    print(\"test\")"
                , "```"
                , ""
                , "</details>"
                ]
            )
        , expectParseOk "case4-with-details-tag-in-code: <details> tag in code fence inside details"
            (String.join "\n"
                [ "<details>"
                , "<summary>Code</summary>"
                , ""
                , "```"
                , "# This <details> tag should NOT be parsed"
                , "```"
                , ""
                , "</details>"
                ]
            )
        , expectParseOk "case4-with-summary-tag-in-code: <summary> tag in code fence inside details"
            (String.join "\n"
                [ "<details>"
                , "<summary>Code</summary>"
                , ""
                , "```"
                , "html = \"<summary>not real</summary>\""
                , "```"
                , ""
                , "</details>"
                ]
            )
        , expectParseOk "case4-with-closing-details-in-code: </details> in code fence inside details"
            (String.join "\n"
                [ "<details>"
                , "<summary>Code</summary>"
                , ""
                , "```"
                , "text </details> text"
                , "```"
                , ""
                , "</details>"
                ]
            )

        -- Full test document (all cases combined)
        , expectParseOk "full test document"
            (String.join "\n"
                [ "# Markdown Details/Summary Test Cases"
                , ""
                , "## 1. Basic details block"
                , ""
                , "<details>"
                , "<summary>Click to expand</summary>"
                , ""
                , "This is basic content inside a details block."
                , ""
                , "</details>"
                , ""
                , "## 2. URL with & inside details"
                , ""
                , "<details>"
                , "<summary>Links with ampersands</summary>"
                , ""
                , "Check this URL: https://example.com?a=1&bar=2&baz=3"
                , ""
                , "And a markdown link: [Google search](https://www.google.com/search?q=hello&lang=en&safe=off)"
                , ""
                , "Bare URL with multiple &: https://api.example.com/data?format=json&limit=10&offset=20&sort=name"
                , ""
                , "</details>"
                , ""
                , "## 3. Already escaped entities inside details"
                , ""
                , "<details>"
                , "<summary>Entities</summary>"
                , ""
                , "This has &amp; already escaped, and &lt;tag&gt; too."
                , "And a numeric entity: &#123; and hex: &#x1f4a9;"
                , ""
                , "</details>"
                , ""
                , "## 4. Code block inside details"
                , ""
                , "<details>"
                , "<summary>Code example</summary>"
                , ""
                , "```python"
                , "# This <details> tag should NOT be parsed"
                , "url = \"https://example.com?a=1&b=2\""
                , "html = \"<summary>not a real tag</summary>\""
                , "if x & y:"
                , "    print(\"ampersand in code\")"
                , "```"
                , ""
                , "</details>"
                , ""
                , "## 5. Fenced code block with details/summary tags (outside details)"
                , ""
                , "```html"
                , "<details>"
                , "<summary>This is inside a code fence</summary>"
                , "<p>Should render as code, not as a collapsible block</p>"
                , "&foo &bar &baz"
                , "</details>"
                , "```"
                , ""
                , "## 6. Inline backtick with tags (outside details)"
                , ""
                , "Use `<details>` and `<summary>` tags for collapsible sections."
                , ""
                , "The entity `&amp;` represents an ampersand."
                , ""
                , "## 7. Nested details blocks"
                , ""
                , "<details>"
                , "<summary>Outer block</summary>"
                , ""
                , "Some outer content with https://example.com?x=1&y=2"
                , ""
                , "<details>"
                , "<summary>Inner block</summary>"
                , ""
                , "Inner content with https://example.com?a=1&nested=true&deep=yes"
                , ""
                , "</details>"
                , ""
                , "Back to outer content."
                , ""
                , "</details>"
                , ""
                , "## 8. Details with div inside"
                , ""
                , "<details>"
                , "<summary>Mixed HTML blocks</summary>"
                , ""
                , "<div>"
                , ""
                , "Content inside a div with URL https://example.com?div=1&test=2"
                , ""
                , "</div>"
                , ""
                , "</details>"
                , ""
                , "## 9. Bare & outside any HTML block (should work as before)"
                , ""
                , "Normal paragraph with https://example.com?a=1&b=2 in it."
                , ""
                , "And a bare & by itself, or AT&T, or R&D."
                , ""
                , "## 10. Details with blockquote content"
                , ""
                , "<details>"
                , "<summary>Blockquote inside</summary>"
                , ""
                , "> This is a quote with a URL https://example.com?q=1&lang=en"
                , "> And a second line with &stuff"
                , ""
                , "</details>"
                , ""
                , "## 11. Single-line HTML (edge case)"
                , ""
                , "<div>Quick &test with https://example.com?a=1&b=2</div>"
                , ""
                , "## 12. Code fence inside details with ~~~ syntax"
                , ""
                , "<details>"
                , "<summary>Tilde code fence</summary>"
                , ""
                , "~~~js"
                , "const url = \"https://example.com?a=1&b=2\";"
                , "// <details><summary>not parsed</summary></details>"
                , "~~~"
                , ""
                , "</details>"
                , ""
                , "## 13. Multiple details blocks in sequence"
                , ""
                , "<details>"
                , "<summary>First block</summary>"
                , ""
                , "https://one.com?a=1&b=2"
                , ""
                , "</details>"
                , ""
                , "<details>"
                , "<summary>Second block</summary>"
                , ""
                , "https://two.com?c=3&d=4"
                , ""
                , "</details>"
                , ""
                , "## 14. Details with autolinked mentions and tensions"
                , ""
                , "<details>"
                , "<summary>Fractale references</summary>"
                , ""
                , "Mention @someuser and tension 0x1a2b3c with URL https://fractale.co/path?ref=1&type=tension"
                , ""
                , "</details>"
                , ""
                , "## 15. Empty details"
                , ""
                , "<details>"
                , "<summary>Empty</summary>"
                , ""
                , "</details>"
                ]
            )
        ]
