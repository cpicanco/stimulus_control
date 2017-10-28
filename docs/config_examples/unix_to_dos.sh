#!/bin/bash

for file in windows*.txt
do
    rm $file
done

for file in linux*.txt
do
    echo $file
    unix2dos -b -u $file
done

for from_file in linux*.txt
do
    to_file="${from_file/linux/windows}"
    mv $from_file $to_file
    echo $to_file
done

for from_file in linux*.bak
do
    to_file=$(echo $from_file | cut -f 1 -d '.')
    to_file+=".txt"
    mv $from_file $to_file
done

exit