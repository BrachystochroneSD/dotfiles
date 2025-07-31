#!/bin/sh

mkdir "$PWD"/convertdir

all_file=$(find "$PWD" -maxdepth 1 -type f | grep -ic ".jpg\|.jpeg\|.png\|.bmp")
i=0

for file in *.jpg *.jpeg *.png *.bmp; do
    [ -e "$file" ] || [ -L "$file" ] || continue
    percent=$((i*100 / all_file))
    printf "Converting percent : $percent%%\r"
    convert "$PWD"/"$file" -resize 1000x1554 -quality 75 "$PWD"/convertdir/"$file"
    : $((i=i+1))
done

printf "\nDone\n"
