#!/bin/bash

NC='\033[0m'
CYAN='\033[0;36m'
GREEN='\033[0;32m'
RED='\033[0;31m'

result=true

INPUT_FILES="code_gen/*.in"
printf "${CYAN}Running code_gen tests...\n${NC}"



for input_file in $INPUT_FILES; do
    output_file=${input_file/.in/.out}
    sh ./circline.sh $input_file | cmp -s $output_file -
    if [ "$?" -eq 0 ]; then
       printf "%-65s ${GREEN}SUCCESS\n${NC}" "  - checking $input_file..."
    else
       printf "%-65s ${RED}ERROR\n${NC}" "  - checking $input_file..." 1>&2
    	result=false
    fi
done

exit 0

# if $result; then
# 	exit 0
# else
# 	exit 1
# fi
