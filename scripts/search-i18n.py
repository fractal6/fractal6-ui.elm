#!/usr/bin/env python3
"""Extract top-level keys in i18n/i18n.toml whose values contain '{{_tension_}}'."""

from pathlib import Path

import tomllib

NEEDLE = "{{_tension_}}"
TOML_PATH = Path(__file__).parent / "i18n" / "i18n.toml"


def contains_needle(value) -> bool:
    if isinstance(value, str):
        return NEEDLE in value
    if isinstance(value, dict):
        return any(contains_needle(v) for v in value.values())
    if isinstance(value, list):
        return any(contains_needle(v) for v in value)
    return False


def main() -> None:
    with TOML_PATH.open("rb") as f:
        data = tomllib.load(f)
    for key, value in data.items():
        if contains_needle(value):
            print(key)


if __name__ == "__main__":
    main()
