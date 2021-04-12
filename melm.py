#!/bin/python

'''Elm spa parser/generator.

Usage:
    melm add MODULE_NAME...
    melm push MODULE_NAME...

Commands:
    add     Add a new sub-component.
    push    Add a sub-component in a existing component.
'''

import os
from string import Template
from docopt import docopt


class ElmSpa():

    # An Elm module name. CamelCase, dot separated.
    component_path = "src/"
    templates = {
        "simple": "tests/component-simple.template.elm", # @todo
        "modal": "tests/component-modal.template.elm",
    }
    default_template = "modal"

    def __init__(self, conf):
        self.conf = conf
        self.checks()

        model = self.default_template
        self.template = Template(open(self.templates[model]).read())

    def checks(self):
        # Check module name
        # check case and special character
        pass

    def run(self):
        q = self.conf
        if q["add"]:
            self.add_subcomponent(q["MODULE_NAME"][0])
        elif q["push"]:
            self.push_subcomponent(*q["MODULE_NAME"])


    def add_subcomponent(self, module_name):
        modules = module_name.split(".")
        d = dict(
            module_name = module_name,
            module_basename = modules[-1]
        )
        s = self.template.substitute(d)
        fn = os.path.join(self.component_path, *modules) + ".elm"
        if os.path.exists(fn):
            raise ValueError("Path already exists: %s" % fn)
        with open(fn, "w") as f:
            f.write(s)
        print("file %s written" % fn)

    def push_subcomponent(self, module_name_source, module_name_target):
        pass

if __name__ == "__main__":
    args = docopt(__doc__, version="melm 0")
    p = ElmSpa(args)
    p.run()

