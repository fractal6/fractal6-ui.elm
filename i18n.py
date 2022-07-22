#!/bin/python

'''i18n file generator

Usage:
    i18n ls
    i18n bootstrap [-w]

Commands:
    ls     List i18n items

Options:
    -w, --write         save/replace in file.

Examples:
    i18n.py ls
'''

import os
import re
from string import Template
from docopt import docopt
from loguru import logger

src_path = "src/"

class I18N(object):

    i18n_output = "i18n/i18n.toml"

    def __init__(self, conf):
        self.conf = conf

    def run(self):
        q = self.conf
        if q["ls"]:
            self._list_items()
        elif q["bootstrap"]:
            data = self._bootstrap()

            if q["--write"]:
                with open(self.i18n_output, "w") as _f:
                    for ll in data:
                        k = ll[0]
                        v = ll[1]
                        _f.write("[%s]\n" % k)
                        _f.write("  en=%s\n" % v)
                        _f.write("\n")


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
                if len(ll) != 2: continue
                k = ll[0].strip()
                v = ll[1].strip()
                if v.startswith('"""') or v.startswith("'''"):
                    multiline = True
                    last_k = k
                    content = v + "\n"
                    continue

                data.append([k, v])

        return data

    def _list_items(self):
        with open(self.i18n_output) as _f:
            lines = _f.readlines()

        n_entries = 0
        for l in lines:
            l = l.strip()
            if l.startswith("[") and l.endswith("]"):
                n_entries +=1

        print("Number of entries: %d" % n_entries)


if __name__ == "__main__":
    args = docopt(__doc__, version="i18n v0")
    p = I18N(args)
    p.run()


