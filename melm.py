#!/bin/python

'''Elm spa parser/generator.

Usage:
    melm add [-w] MODULE_NAME...
    melm push [-w] MODULE_NAME...

Commands:
    add     Add a new sub-component.
    push    Add a sub-component in a existing component.

Options:
    -w, --write  save/replace in file

Examples:
    melm.py add Components.MoveTension
    melm.py push Components.MoveTension Org.Tension
'''

import os
from string import Template
from docopt import docopt
import re
from loguru import logger


class ElmSpa(object):

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

    def get_module_map(self, module_name):
        modules = module_name.split(".")
        return dict(
            module_name = module_name,
            module_basename = modules[-1],
            module_basename_lower1 = modules[-1][0].lower() + modules[-1][1:],
            fn = os.path.join(self.component_path, *modules) + ".elm",
        )

    def add_subcomponent(self, module_name):
        d = self.get_module_map(module_name)
        s = self.template.substitute(d)

        if self.conf['--write']:
            if os.path.exists(d["fn"]):
                raise ValueError("Path already exists: %s" % d["fn"])
            with open(d["fn"], "w") as f:
                f.write(s)
            print("file %s written" % d["fn"])
        else:
            print(s)

    def push_subcomponent(self, module_name_source, module_name_target):

        import_spec = [
            dict(
                reg =  r"^import ",
                pos = -1,
                t = "import ${module_name} as ${module_basename}"
            ),
        ]
        model_spec = [
            dict(
                reg =  r"^type alias Model\s*=\s*{.*?}",
                pos = 0,
            ),
            dict(
                reg =  r"\n",
                pos = -1,
                t = ", ${module_basename_lower1} : ${module_basename}.State"
            )
        ]
        init_spec = [
            dict(
                reg =  r"^init .*?=\s*let.*? in ",
                pos = 0,
            ),
            dict(
                reg =  r"^\s*model\s*=\s*{.*?}",
                pos = 0,
            ),
            dict(
                reg =  r"\n",
                pos = -1,
                t = ", ${module_basename_lower1} = ${module_basename}.init global.session.user"
            )
        ]
        msg_spec = [
            dict(
                reg =  r"^type Msg\s*=.*?\n\n\n",
                pos = 0,
            ),
            dict(
                reg =  r"\n",
                pos = -3,
                t = "| ${module_basename}Msg ${module_basename}.Msg"
            )
        ]
        update_spec = [
            dict(
                reg =  r"^update .*?=.*?\n\n\n",
                pos = 0,
            ),
            dict(
                reg =  r"\n",
                pos = -3,
                t = '''
                ${module_basename}Msg msg ->
                    let
                        ( data, out ) = ${module_basename}.update apis msg model.${module_basename_lower1}

                        (res1, res2) = out.result

                        ( cmds, gcmds ) = mapGlobalOutcmds out.gcmds
                    in
                    ( {model | ${module_basename_lower1} = data}, out.cmds |> List.map (\m -> Cmd.map ${module_basename}Msg m) |> List.append cmds |> Cmd.batch, Cmd.batch gcmds )
                '''
            ),
        ]
        subscriptions_spec = [
            dict(
                reg =  r"^subscriptions .*?=.*?\n\n\n",
                pos = 0,
                t = "|> Sub.batch"
            ),
            dict(
                reg =  r"\n",
                pos = -4,
                t = "++ (${module_basename}.subscriptions |> List.map (\s -> Sub.map ${module_basename}Msg s))"
            )
        ]
        view_spec = [
            dict(
                reg =  r"^view .*?=.*?\n\n\n",
                pos = 0,
            ),
            dict(
                reg =  r"\s+body\s*=\s*\[.*?\]",
                pos = 0,
            ),
            dict(
                reg =  r"\n",
                pos = -1,
                t = ", ${module_basename}.view {} model.${module_basename_lower1} |> Html.map ${module_basename}Msg"
            )
        ]

        all_specs = [
            import_spec,
            model_spec,
            init_spec,
            msg_spec,
            update_spec,
            subscriptions_spec,
            view_spec,
        ]

        s_map = self.get_module_map(module_name_source)
        t_map = self.get_module_map(module_name_target)
        content = open(t_map["fn"]).read()

        for spec in all_specs:
            try:
                _, content = self.rmatch(spec, content, mapping=s_map)
            except TypeError as e:
                logger.warning("Potentially not found regex: %s" % spec )
                continue

        if self.conf['--write']:
            with open(t_map["fn"], "w") as f:
                f.write(content)
            print("file %s written" % t_map["fn"])
        else:
            print(content)


    def rmatch(self, regs, content, mapping=None):
        '''Match a sequence of regexp recursively use to concentrate the last regexp of the sequence.
           Returns the line of the final match.

           REG FORMAT : {
            reg: regexp,
            pos: int (0 for the first match and -1 for the last match)
            t: template to add
           }

        '''

        if len(regs) == 0:
            return

        #print(content)
        #print('-'*29)

        reg = regs[0]
        lines = []
        for m in re.finditer(reg["reg"], content, re.MULTILINE|re.DOTALL):
            start, end = m.start(), m.end()
            line_start, line_end = content[0:start].count("\n"), content[0:end].count("\n")
            # recompute start/end after/before the next/previous newline.
            offset = content[start:end].find('\n')
            start = (start if offset < 0 else start + offset)+1
            offset = content[:end][::-1].find('\n')
            end = (end if offset < 0 else end - offset)-1
            lines.append((line_start, line_end, start, end, m.group()))

        if len(lines) == 0:
            return

        _, line, start, end, matching = lines[reg["pos"]]
        if reg.get("t"):
            if mapping:
                n = Template(reg["t"]).substitute(mapping)
            else:
                n = reg["t"]

            temp = content.split("\n")
            # Get the offset with the last line of content that is not empty
            offset = [i for i, x in enumerate(temp[:line+1][::-1]) if x.strip() != ""][0]
            # if the patch is not already here, add it
            if temp[line - offset].strip() != n.strip():
                # Get the indent of the last line of content that is not empty
                indent = len(temp[line-offset]) - len(temp[line-offset].lstrip())
                # indent the patch
                n = list(map(lambda x: " "*indent + x.lstrip(), n.split("\n")))
                extension = temp[:line+1] + n + temp[line+1:]
                content = "\n".join(extension)
                end = end + len(n)

        r = self.rmatch(regs[1:], content[start:end], mapping=mapping)
        if r:
            l, c = r
            return line+l, content[:start] + c + content[end:]

        else:
            return line, content


if __name__ == "__main__":
    args = docopt(__doc__, version="melm 0")
    p = ElmSpa(args)
    p.run()

