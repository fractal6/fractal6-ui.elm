#!/bin/bash


# Magical command that will output the missing objects
#
# node_modules/.bin/elm make src/Main.elm --output=/dev/null 2>&1 \
#     | command grep 'cannot find a `.*`' \
#     | sed 's/.*`\([^`]*\)`.*/\1/' \
#     | sort \
#     | uniq \
#     | paste -sd "," \
#     | sed "s/,/, /g"

# Add missing import from the given module inside the given file.
# @DEBUG: Enum type like will be added although they must be imported by a syntak like so MyType(..)
#
# Usage: ./debugit.sh FILENAME
#
if [ "$1" == "-d" -o -z "$1" ]; then
    node_modules/.bin/elm make src/Main.elm --output=/dev/null
elif [ ! -f $1 ]; then
    echo "Error: file unknwown"
    exit
fi

PATTERN='cannot find a `.*`'
TARGET="$1"
MODULE="ModelSchema"
TERMS=$(node_modules/.bin/elm make src/Main.elm --output=/dev/null  2>&1  | command grep "$PATTERN" | sed 's/.*`\([^`]*\)`.*/\1/' | sort | uniq | paste -sd "," | sed "s/,/, /g")

if [ -z "$TERMS" ]; then
    echo "No terms matched"
    exit
fi

grep -q "^import ${MODULE}$" "$TARGET"

if [ $? -eq 0 ]; then
    sed -i "s/^import $MODULE$/import $MODULE exposing ($TERMS)/" "$TARGET"
else
    sed -i "s/^import $MODULE exposing (\(.*\))$/import $MODULE exposing (\1, $TERMS)/" "$TARGET"
fi
