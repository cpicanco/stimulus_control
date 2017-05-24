#!/bin/bash

for file in linux*.txt
do
    from_file=$file 
    to_file=$(echo $file | cut -f 1 -d '.')
    # todos unix2dos $file "$name_win.txt"
    echo $from_file
    unix2dos -b -u $from_file
done

exit