#!/bin/bash

# Elm Debug Helper Script
# Helps fix missing imports and analyze compilation errors

set -e

# Configuration
ELM_BIN="node_modules/.bin/elm"
MAIN_FILE="src/Main.elm"
DEFAULT_MODULE="ModelSchema"

# Functions
show_usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS] [FILE]

Options:
  -l    List files with errors and error count
  -h    Show this help message

Without options: Run elm compiler (default)
With FILE: Add missing imports from $DEFAULT_MODULE to FILE
EOF
}

# Run elm compiler and capture output
run_elm_compiler() {
    "$ELM_BIN" make "$MAIN_FILE" --output=/dev/null 2>&1 | sed "s/Compiling ...//g" || true
}

# List files with errors and their count
list_error_files() {
    local output
    output=$(run_elm_compiler)

    # Extract file paths from error headers like "-- TYPE MISMATCH ---- src/Components/NodeDoc.elm"
    # The error format has multiple dashes before and after the error type
    echo "$output" | grep -E "^-- " | \
        sed -E 's/^-- [A-Za-z0-9 ]+ --* //' | \
        sort | uniq -c | \
        awk '{printf "%-50s %d error(s)\n", $2, $1}'
}

# Extract missing terms from compiler output
get_missing_terms() {
    run_elm_compiler | \
        grep 'cannot find a `.*`' | \
        sed 's/.*`\([^`]*\)`.*/\1/' | \
        sort | uniq | \
        paste -sd "," | \
        sed "s/,/, /g"
}

# Add missing imports to target file
add_missing_imports() {
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
    local target_file="$1"
    local module="${2:-$DEFAULT_MODULE}"

    # Check if file exists
    if [[ ! -f "$target_file" ]]; then
        echo "Error: File not found: $target_file" >&2
        exit 1
    fi

    # Get missing terms
    local terms
    terms=$(get_missing_terms)

    if [[ -z "$terms" ]]; then
        echo "No missing terms found"
        return 0
    fi

    echo "Adding missing imports: $terms"

    # Check if module import exists
    if grep -q "^import ${module}$" "$target_file"; then
        # Convert simple import to exposing import
        sed -i "s/^import $module$/import $module exposing ($terms)/" "$target_file"
    elif grep -q "^import $module exposing (" "$target_file"; then
        # Add to existing exposing list
        sed -i "s/^import $module exposing (\(.*\))$/import $module exposing (\1, $terms)/" "$target_file"
    else
        echo "Warning: Module $module not imported in $target_file"
    fi
}

# Main logic
main() {
    case "$1" in
        -h|--help)
            show_usage
            exit 0
            ;;
        -l|--list)
            list_error_files
            exit 0
            ;;
        "")
            # Default: run compiler
            run_elm_compiler
            ;;
        *)
            # File provided: add missing imports
            add_missing_imports "$1"
            ;;
    esac
}

main "$@"
