#!/usr/bin/env python

"""i18n file generator

Usage:
    i18n ls
    i18n bootstrap [-w]
    i18n gen [--lang LANG] [-w]

Commands:
    ls          List i18n items
    bootstrap   Initialize .toml file from a elm file containing traductions
    gen         generate an elm file in the given lang.

Options:
    -w, --write        save/replace in file.
    -l, --lang         lang to use (DEFAULT: en).

Examples:
    i18n.py ls
    i18n.py gen -w -l fr
"""

import os
import re
from string import Template

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

default_lexicon = {
    "tension": "tension",
    "Tension": "Tension",
    "mandate": "mandate",
    "Mandate": "Mandate",
    "void": "",
}


class I18N(object):
    src_path = "src/"
    i18n_input = "i18n/i18n.toml"
    i18n_output = "src/Text.elm"
    default_lang = "en"

    def __init__(self, conf):
        self.conf = conf

    def run(self):
        q = self.conf
        if q["ls"]:
            self.list_items()
        elif q["bootstrap"]:
            data = self.bootstrap()
            if q["--write"]:
                with open(self.i18n_input, "w") as _f:
                    for ll in data:
                        k = ll[0]
                        v = ll[1]
                        _f.write("[%s]\n" % k)
                        _f.write("  en=%s\n" % v)
                        _f.write("\n")
            else:
                print(data)

        elif q["gen"]:
            lang = q.get("LANG") or self.default_lang
            data = self.gen(lang)
            if q["--write"]:
                with open(self.i18n_output, "w") as _f:
                    _f.write(elm_header)
                    for k, d in data.items():
                        v = d["text"]
                        lexicon_terms = d["lexicon_terms"]
                        if lexicon_terms:
                            _f.write(elm_entry_dict_template.format(k=k, v=v))
                        else:
                            _f.write(elm_entry_template.format(k=k, v=v))

                        for term in lexicon_terms:
                            default = default_lexicon[term]
                            word = f'withDefault "{default}" (Dict.get "{term}" lexicon)'
                            _f.write(" "*8 + f'|> String.Format.namedValue "_{term}_" ({word})\n')
                        _f.write("\n\n")

                print("%s written" % self.i18n_output)
            else:
                print(data)

    def gen(self, lang) -> dict:
        with open(self.i18n_input) as _f:
            lines = _f.readlines()

        multiline = False
        multiline_content = ""
        in_entry = False  # not used
        entry = ""
        current_lang = ""  # current lang
        data = {}
        for line in lines:
            line = line.strip()
            if line.startswith("[") and line.endswith("]"):
                # Got an entry
                entry = line[1:-1]
                continue
            elif line.startswith("#"):
                continue

            if multiline:
                # Multiline entry
                multiline_content += line
                if line.endswith('"""') or line.endswith("'''"):
                    multiline = False
                    self._append_or_replace_entry(current_lang, entry, multiline_content, data)
                else:
                    multiline_content += "\n"
            else:
                # Single line entry
                ll = line.split("=")
                if len(ll) < 2:
                    continue
                current_lang = ll[0].strip()
                v = "=".join(ll[1:]).strip()
                if current_lang not in [lang, self.default_lang]:
                    continue
                if v.startswith('"""') or v.startswith("'''"):
                    multiline = True
                    multiline_content = v + "\n"
                    continue

                self._append_or_replace_entry(current_lang, entry, v, data)

        return data

    def _append_or_replace_entry(self, lang, entry, v, data):
        # extract lexicon pattern...
        lexicon_terms = [x[3:-3] for x in re.findall(r"{{_\w+_}}", v)]

        if entry not in data or lang != self.default_lang:
            data[entry] = {"lang": lang, "text": v, "lexicon_terms": lexicon_terms}


    def bootstrap(self):
        with open("src/temp.elm") as _f:
            lines = _f.readlines()

        multiline = False
        last_k = ""
        content = ""
        data = []
        for l in lines:
            l = l.rstrip()

            if multiline:
                content += l
                if l.endswith('"""') or l.endswith("'''"):
                    multiline = False
                    data.append([last_k, content])
                else:
                    content += "\n"
            else:
                ll = l.split("=")
                if len(ll) < 2:
                    continue
                k = ll[0].strip()
                v = "=".join(ll[1:]).strip()
                if v.startswith('"""') or v.startswith("'''"):
                    multiline = True
                    last_k = k
                    content = v + "\n"
                    continue

                data.append([k, v])

        return data

    def list_items(self):
        with open(self.i18n_input) as _f:
            lines = _f.readlines()

        n_entries = 0
        for l in lines:
            l = l.strip()
            if l.startswith("[") and l.endswith("]"):
                n_entries += 1

        print("Number of entries: %d" % n_entries)


# Number of entrie per lang
# grep "en=" i18n/i18n.toml  | wc -l


if __name__ == "__main__":
    args = docopt(__doc__, version="i18n v0")
    p = I18N(args)
    p.run()
