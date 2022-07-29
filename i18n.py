#!/bin/python

'''i18n file generator

Usage:
    i18n ls
    i18n bootstrap [-w]
    i18n gen [--lang LANG] [-w]

Commands:
    ls     List i18n items
    bootstrap Initialiaz .toml file from a elm file containent traductions
    gen generate an elm file in the given lang.

Options:
    -w, --write        save/replace in file.
    -l, --lang         lang to use (DEFAULT: en).

Examples:
    i18n.py ls
'''

import os
import re
from string import Template
from docopt import docopt
from loguru import logger

elm_header = """\
module Text exposing (..)

{-
    Auto-generated i18n
-}

"""

elm_entry_template = """\
{k} : String
{k} =
    {v}
"""

class I18N(object):

    src_path = "src/"
    i18n_input = "i18n/i18n.toml"
    i18n_output = "src/Text.elm"

    def __init__(self, conf):
        self.conf = conf

    def run(self):
        q = self.conf
        if q["ls"]:
            self._list_items()
        elif q["bootstrap"]:
            data = self._bootstrap()
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
            lang = q.get("LANG") or "en"
            data = self._gen(lang)
            if q["--write"]:
                with open(self.i18n_output, "w") as _f:
                    _f.write(elm_header)
                    for ll in data:
                        k = ll[0]
                        v = ll[1]
                        _f.write(elm_entry_template.format(k=k, v=v))
                        _f.write("\n\n")

                print("%s written" % self.i18n_output)
            else:
                print(data)

    def _gen(self, lang):
        with open(self.i18n_input) as _f:
            lines = _f.readlines()

        multiline = False
        in_entry = False
        entry = ""
        # data is a tuple of (#keyword identifier, #text content, #list of named arguments)
        data = []
        for l in lines:
            l = l.strip()
            if not in_entry or (l.startswith("[") and l.endswith("]")):
                # Got an entry
                in_entry = True
                entry = l[1:-1]
                continue

            if not in_entry:
                # Ignore everithing else
                continue

            if multiline:
                # Multiline entry
                content += l
                if l.endswith('"""') or l.endswith("'''"):
                    multiline = False
                    in_entry = False
                    data.append([entry, content])
                else:
                    content += "\n"
            else:
                # single line entry
                ll = l.split("=")
                if len(ll) < 2: continue
                k = ll[0].strip()
                v = "=".join(ll[1:]).strip()
                if k != lang:
                    continue
                if v.startswith('"""') or v.startswith("'''"):
                    multiline = True
                    content = v + "\n"
                    continue

                data.append([entry, v])

        return data

    def _bootstrap(self):
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
                if len(ll) < 2: continue
                k = ll[0].strip()
                v = "=".join(ll[1:]).strip()
                if v.startswith('"""') or v.startswith("'''"):
                    multiline = True
                    last_k = k
                    content = v + "\n"
                    continue

                data.append([k, v])

        return data

    def _list_items(self):
        with open(self.i18n_input) as _f:
            lines = _f.readlines()

        n_entries = 0
        for l in lines:
            l = l.strip()
            if l.startswith("[") and l.endswith("]"):
                n_entries +=1

        print("Number of entries: %d" % n_entries)
		# Number of entrie per lang
		#grep "en=" i18n/i18n.toml  | wc -l


if __name__ == "__main__":
    args = docopt(__doc__, version="i18n v0")
    p = I18N(args)
    p.run()


