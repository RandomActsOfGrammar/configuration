#!/bin/bash

#get the full list line for the touchpad
touchpad_line=$(xinput --list --short | grep -i "touchpad")
#remove [...] from the end
touchpad_line_start=${touchpad_line%\[*\]}
#remove name and 'id=' from the beginning, leaving just the id number
touchpad_id=${touchpad_line_start##*id=}

#get the line line for the tapping property
#(needs '(' at the end to get the one that can be set by a script rather
#   than the one to enable it by default)
prop_line=$(xinput --list-props $touchpad_id | grep -i "tapping enabled (")
prop_line_start=${prop_line%%\)*}
prop_num=${prop_line_start##*\(}

xinput set-prop $touchpad_id $prop_num 1
