#!/bin/bash

# Author: Zakaria Kebairia
# Email: 4.kebairia@gmail.com
#
# Description: 
# This script updates the file modification time of all files in the ./content/ directory
# based on their file name prefix (YYYY-MM-DD).

# Define the content directory path
content_dir="./content/"

# Loop through each file in the content directory except for index.org
for file in "${content_dir}"*.org; do
    if [ "$file" != "${content_dir}index.org" ]; then

        # Extract the date from the file name prefix
        file_date="$(basename "$file" | cut -d'-' -f1-3)"

        # Convert the date to the format required by the touch command
        touch_date="$(date -d "${file_date}" +"%Y%m%d%H%M")"

        # Update the file modification time using the touch command
        touch -t "${touch_date}" "${file}"
    fi
done

