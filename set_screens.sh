#!/bin/bash

main () {
    #check whether there are arguments telling which configuration to use
    argument_found=false

    #check that there is an argument
    while [ ! -z $1 ]; do
        case $1 in
            home | h )
                top_long
                argument_found=true
                ;;
            laptop | l )
                just_laptop
                argument_found=true
                ;;
            apartment | apt | a )
                right_tall
                argument_found=true
                ;;
            -h | --help )
                show_help
                exit #leave once the help is displayed
                ;;
            * )
                echo "Unknown argument $1"
        esac
        shift
    done

    #no arguments; decide based on connections
    if ! $argument_found; then
        if xrandr -q | grep "HDMI1" | grep "disconnected"; then
            just_laptop
        else
            right_tall
        fi
    fi
}


#show the valid options
show_help () {
    echo "Usage: set_screens.sh [CONFIGURATION] [-h]"
    echo ""
    echo "Set the screen configuration using xrandr to a configuration " \
         "determined by an argument or the connections available."
    echo ""
    echo "Available configuration options:"
    echo "   laptop, l:  Turn HDMI off so only the laptop screen is turned on"
    echo "   apartment, apt, a:  Set HDMI tall and to the right"
    echo "   home, h:  Set HDMI above laptop screen"
    echo ""
    echo "If no configuration is given:"
    echo "   HDMI connected:  Set HDMI tall and to the right"
    echo "   HDMI disconnected:  Turn HDMI off so only the laptop screen is turned on"
}


#functions for doing the actual setting of the screens

#just the laptop screen, nothing else
just_laptop () {
    xrandr --fb 1366x768 --output eDP1 --pos 0x0 --output HDMI1 --off
}

#sideways screen on the right, like at my apartment
right_tall () {
    xrandr --fb 2446x1920 \
           --output HDMI1 --mode 1920x1080 --rotate left --pos 1366x0 \
           --output eDP1 --pos 0x1152
}

#long one above, like at home
top_long () {
    xrandr --fb 1920x1848 \
       --output HDMI1 --mode 1920x1080 --rotate normal --pos 0x0 \
       --output eDP1 --pos 0x1080
}


#actually call it
main $@
