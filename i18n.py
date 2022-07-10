#!/bin/python

'''i18n file generator

Usage:
    i18n ls [-w]

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

    def __init__(self, conf):
        self.conf = conf

    def run(self):
        q = self.conf
        if q["ls"]:
            self._list_items()

    def _list_items(self):
        pass


if __name__ == "__main__":
    args = docopt(__doc__, version="i18n v0")
    p = I18N(args)
    p.run()


