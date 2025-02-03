#!/bin/bash

if [ $# -ne 2 ]; then
    echo "Usage: $0 <year> <day>"
    exit 1
fi

YEAR=$1
DAY=$2

curl --cookie "session=$(cat .token)" "https://adventofcode.com/$YEAR/day/$DAY/input" -o "${YEAR}/$(printf "%02d" $DAY)_input.txt"
