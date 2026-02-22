#!/usr/bin/env python

"""i18n file generator

Usage:
    i18n ls
    i18n gen [--lang LANG] [-w]

Commands:
    ls          List i18n items
    gen         generate an elm file in the given lang.

Options:
    -w, --write        save/replace in file.
    -l, --lang         lang to use (DEFAULT: en).

Examples:
    i18n.py ls
    i18n.py gen -w -l fr
"""

import re
import sys
import tomllib

from docopt import docopt

elm_header = """\
module Text exposing (..)

import Dict exposing (Dict)
import Maybe exposing (withDefault)
import String.Format

{-
    Auto-generated i18n.
    DO NOT EDIT
-}

"""

elm_entry_template = """\
{k} : String
{k} =
    {v}
"""

elm_entry_dict_template = """\
{k} : Dict String String -> String
{k} lexicon =
    {v}
"""

# Lexicon pattern convention:
# In i18n.toml, translatable terms that depend on per-org lexicon are written as {{_term_}}
# (e.g. {{_tension_}}, {{_Mandate_}}). At Elm generation time, these become
# String.Format.namedValue calls that look up the term in a Dict at runtime.
LEXICON_PATTERN = re.compile(r"{{_(\w+)_}}")


def lexicon_elm_key(term):
    """Convert a lexicon term key to a valid Elm identifier with _ suffix.

    Lowercase terms become term_ (e.g. "tension" -> "tension_").
    Uppercase-starting terms get Up_ suffix to avoid conflicts (e.g. "Tension" -> "tensionUp_").
    """
    if term[0].isupper():
        return term[0].lower() + term[1:] + "Up_"
    return term + "_"


DEFAULT_LEXICON = {
    "en": {
        "tension": "tension",
        "Tension": "Tension",
        "mandate": "mandate",
        "Mandate": "Mandate",
        "void": "",
    },
    "fr": {
        "tension": "tension",
        "Tension": "Tension",
        "mandate": "mandat",
        "Mandate": "Mandat",
        "void": "",
    },
}


def to_elm_string(value):
    """Convert a Python string to an Elm string literal.

    Single-line values become "...", multiline values become triple-quoted \"\"\"...\"\"\" strings.
    Backslashes and (for single-line) double quotes are escaped for Elm.
    """
    if "\n" in value:
        # Strip leading whitespace from each line to match TOML file indentation cleanup
        lines = [line.strip() for line in value.split("\n")]
        stripped = "\n".join(lines)
        escaped = stripped.replace("\\", "\\\\")
        return f'"""{escaped}"""'
    else:
        escaped = value.replace("\\", "\\\\").replace('"', '\\"')
        return f'"{escaped}"'


class I18N:
    i18n_input = "i18n/i18n.toml"
    i18n_output = "src/Text.elm"
    default_lang = "en"

    def __init__(self, conf):
        self.conf = conf

    def run(self):
        q = self.conf
        if q["ls"]:
            self.list_items()
        elif q["gen"]:
            lang = q.get("LANG") or self.default_lang
            data = self.gen(lang)
            if q["--write"]:
                self.write_elm(data, lang)
                print(f"{self.i18n_output} written")
            else:
                print(data)

    def gen(self, lang):
        """Parse the TOML input and return an ordered dict of entries for the given lang.

        Each entry maps to {"text": <elm_string>, "lexicon_terms": [<term>, ...]}.
        Falls back to the default language when the requested lang is missing for an entry.
        """
        try:
            with open(self.i18n_input, "rb") as f:
                toml_data = tomllib.load(f)
        except FileNotFoundError:
            print(f"Error: input file '{self.i18n_input}' not found.", file=sys.stderr)
            sys.exit(1)
        except tomllib.TOMLDecodeError as e:
            print(f"Error: failed to parse '{self.i18n_input}': {e}", file=sys.stderr)
            sys.exit(1)

        data = {}
        for entry, translations in toml_data.items():
            if not isinstance(translations, dict):
                print(f"Warning: skipping '{entry}' (expected a table, got {type(translations).__name__})", file=sys.stderr)
                continue

            # Use requested lang if available, otherwise fall back to default lang
            if lang in translations:
                value = translations[lang]
            elif self.default_lang in translations:
                value = translations[self.default_lang]
            else:
                print(f"Warning: entry '{entry}' has no '{lang}' or '{self.default_lang}' translation, skipping.", file=sys.stderr)
                continue

            elm_value = to_elm_string(value)
            lexicon_terms = LEXICON_PATTERN.findall(value)

            data[entry] = {"text": elm_value, "lexicon_terms": lexicon_terms}

        return data

    def write_elm(self, data, lang):
        """Write the generated entries to the Elm output file."""
        # Resolve lexicon defaults: use target lang, fall back to default lang
        lexicon = DEFAULT_LEXICON.get(lang, DEFAULT_LEXICON.get(self.default_lang, {}))

        with open(self.i18n_output, "w") as f:
            f.write(elm_header)
            for entry, d in data.items():
                elm_value = d["text"]
                lexicon_terms = d["lexicon_terms"]

                if lexicon_terms:
                    f.write(elm_entry_dict_template.format(k=entry, v=elm_value))
                else:
                    f.write(elm_entry_template.format(k=entry, v=elm_value))

                for term in lexicon_terms:
                    if term not in lexicon:
                        print(f"Warning: unknown lexicon term '{{{{_{term}_}}}}' in entry '{entry}'. "
                              f"Known terms: {', '.join(lexicon.keys())}", file=sys.stderr)
                        default = ""
                    else:
                        default = lexicon[term]
                    word = f'withDefault "{default}" (Dict.get "{term}" lexicon)'
                    f.write(" " * 8 + f'|> String.Format.namedValue "_{term}_" ({word})\n')

                f.write("\n\n")

            # Write default lexicon entries as simple Elm string constants
            f.write("\n-- Default lexicon entries\n\n")
            for term, value in lexicon.items():
                if not value:  # skip empty entries like "void"
                    continue
                elm_key = lexicon_elm_key(term)
                f.write(elm_entry_template.format(k=elm_key, v=f'"{value}"'))
                f.write("\n\n")

    def list_items(self):
        """List the number of i18n entries and per-language counts."""
        try:
            with open(self.i18n_input, "rb") as f:
                toml_data = tomllib.load(f)
        except FileNotFoundError:
            print(f"Error: input file '{self.i18n_input}' not found.", file=sys.stderr)
            sys.exit(1)
        except tomllib.TOMLDecodeError as e:
            print(f"Error: failed to parse '{self.i18n_input}': {e}", file=sys.stderr)
            sys.exit(1)

        n_entries = len(toml_data)
        lang_counts = {}
        for translations in toml_data.values():
            if isinstance(translations, dict):
                for lang in translations:
                    lang_counts[lang] = lang_counts.get(lang, 0) + 1

        print(f"Number of entries: {n_entries}")
        for lang, count in sorted(lang_counts.items()):
            print(f"  {lang}: {count}")


if __name__ == "__main__":
    args = docopt(__doc__, version="i18n v0")
    p = I18N(args)
    p.run()
