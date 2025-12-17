#!/bin/bash
# Wrapper script to run Fortran program with user-provided potential expression

if [ $# -lt 1 ]; then
    echo "Usage: $0 <potential_expression_file>"
    echo "Example: $0 my_potential.inc"
    exit 1
fi

POTENTIAL_FILE="$1"

if [ ! -f "$POTENTIAL_FILE" ]; then
    echo "Error: File '$POTENTIAL_FILE' not found!"
    exit 1
fi

echo "Using potential expression from: $POTENTIAL_FILE"
cp "$POTENTIAL_FILE" potential_expression.inc
echo "Copied to potential_expression.inc"

echo "Compiling..."
gfortran multifix.f -o m.exe
if [ $? -ne 0 ]; then
    echo "Compilation failed!"
    exit 1
fi

echo "Running program..."
./m.exe

