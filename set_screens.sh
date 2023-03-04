#!/bin/bash

main () {
    #check whether there are arguments telling which configuration to use
    argument_found=false

    #check that there is an argument
    while [ ! -z $1 ]; do
        case $1 in
            home | h )
                left_long
                argument_found=true
                ;;
            laptop | l )
                just_laptop
                argument_found=true
                ;;
            apartment | apt | a )
                right_tall_mid
                argument_found=true
                ;;
            keller | k )
                keller6_212
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
        if xrandr -q | grep "HDMI-1" | grep "disconnected"; then
            just_laptop
        else
            right_tall_mid
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
    echo "   apartment, apt, a:  Set HDMI tall and to the left"
    echo "   home, h:  Set HDMI to the left of the laptop screen"
    echo ""
    echo "If no configuration is given:"
    echo "   HDMI connected:  Set HDMI tall and to the right"
    echo "   HDMI disconnected:  Turn HDMI off so only the laptop screen is turned on"
}


#functions for doing the actual setting of the screens

#just the laptop screen, nothing else
just_laptop () {
    xrandr --output eDP-1 --pos 0x0 --primary --output HDMI-1 --off
}

#sideways screen on the right
right_tall () {
    xrandr --output eDP-1 --mode 1920x1080 --pos 0x840 --rotate normal \
           --output HDMI-1 --primary --mode 1920x1080 --pos 1920x0 --rotate left
}

#sideways screen on the right with laptop halfway up
right_tall_mid () {
    xrandr --output eDP-1 --mode 1920x1080 --pos 0x536 --rotate normal \
           --output HDMI-1 --primary --mode 1920x1080 --pos 1920x0 --rotate left
}

#long one on the left
left_long () {
    xrandr --output eDP-1 --primary --pos 1920x509 --rotate normal \
           --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate normal
}

#slightly wider and placed above
keller6_212 () {
    xrandr --output eDP-1 --primary --mode 1366x768 --pos 0x720 --rotate normal \
           --output HDMI-1 --mode 1280x720 --pos 0x0 --rotate normal
}


#actually call it
main $@
