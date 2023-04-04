#!/bin/bash

# Author: Zakaria Kebairia
# Email: 4.kebairia@gmail.com
#
# Description: 
# later
for file in $(\ls ./content/*.org | awk -F "/" '{ print $NF }' | grep -v index.org) ; do
    _date=$( echo "$file" | cut -d"-" -f1-3 )
     touch -t $(date -d $_date +"%Y%m%d%H%M") ./content/$file
done

