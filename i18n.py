#!/usr/bin/env python

'''i18n file generator

Usage:
    i18n ls
    i18n bootstrap [-w]
    i18n gen [--lang LANG] [-w]

Commands:
    ls          List i18n items
    bootstrap   Initialiaz .toml file from a elm file containing traductions
    gen         generate an elm file in the given lang.

Options:
    -w, --write        save/replace in file.
    -l, --lang         lang to use (DEFAULT: en).

Examples:
    i18n.py ls
    i18n.py gen -w -l fr
'''

import os
import re
from string import Template

from docopt import docopt

elm_header = """\
module Text exposing (..)

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
                    for ll in data:
                        k = ll[1]
                        v = ll[2]
                        _f.write(elm_entry_template.format(k=k, v=v))
                        _f.write("\n\n")

                print("%s written" % self.i18n_output)
            else:
                print(data)

    def gen(self, lang):
        with open(self.i18n_input) as _f:
            lines = _f.readlines()

        multiline = False
        in_entry = False # not used
        entry = ""
        l = "" # current lang
        # data is a tuple of (#keyword identifier, #text content, #list of named arguments)
        data = []
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
                content += line
                if line.endswith('"""') or line.endswith("'''"):
                    multiline = False
                    self._append_or_replace_entry(l, entry, content, data)
                else:
                    content += "\n"
            else:
                # Single line entry
                ll = line.split("=")
                if len(ll) < 2: continue
                l = ll[0].strip()
                v = "=".join(ll[1:]).strip()
                if l not in [lang, self.default_lang]:
                    continue
                if v.startswith('"""') or v.startswith("'''"):
                    multiline = True
                    content = v + "\n"
                    continue

                self._append_or_replace_entry(l, entry, v, data)

        return data

    def _append_or_replace_entry(self, l, entry, v, data):
        if len(data) == 0:
            # Empty data
            data.append([l, entry, v])
            return

        last_lang =  data[-1][0]
        last_entry = data[-1][1]

        if entry != last_entry:
            # Normal append
            data.append([l, entry, v])
            return

        if l == self.default_lang:
            # Do not replace default lang entry if entry already exists
            return

        # replace defualt lang by current
        data[-1] = [l, entry, v]

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

    def list_items(self):
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


