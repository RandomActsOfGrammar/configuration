#!/bin/bash

#whether to use a single image (1) or randomize from a directory (0)
USE_SINGLE=0
#what picture to use for a single image
SINGLE_IMAGE=/home/tux/Pictures/Grandpa90Birthday.jpg
#the directory to pull images from--must end with a slash
IMAGE_DIRECTORY=/home/tux/OneDrive/Pictures/Tux/Christmas/
#how long (seconds) to wait between changing the background picture
SLEEP_TIME=60

#check if it is already running
#kill the others so you can pick up changes easily
for pid in $(pidof -x background.sh); do
    if [ $pid != $$ ]; then
        kill $pid
    fi
done


#set the background
if [ "$USE_SINGLE" -gt "0" ]; then
    feh --bg-max $SINGLE_IMAGE
else
    while true; do
        #feh --bg-max --randomize $IMAGE_DIRECTORY*.jpg $IMAGE_DIRECTORY*.png $IMAGE_DIRECTORY*.bmp
        #set IFS to newline to iterate over ls
        IFS='
'
        files=$(ls $IMAGE_DIRECTORY*.jpg $IMAGE_DIRECTORY*.png $IMAGE_DIRECTORY*.bmp $IMAGE_DIRECTORY*.jpeg)
        for image in $files; do
            feh --bg-max $image
            sleep $SLEEP_TIME
        done
    done
fi

