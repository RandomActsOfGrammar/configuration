#!/bin/bash

main () {
    #check whether there are arguments telling which configuration to use
    argument_found=false

    #check that there is an argument
    while [ ! -z $1 ]; do
        case $1 in
            hdmi | HDMI | h )
                audio_hdmi
                argument_found=true
                ;;
            speaker | speakers | s )
                audio_speakers
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
            audio_speakers
        else
            audio_hdmi
        fi
    fi
}


#show the valid options
show_help () {
    echo "Usage: set_audio.sh [CONFIGURATION] [-h]"
    echo ""
    echo "Set where the sound comes out " \
         "determined by an argument or the connections available."
    echo ""
    echo "Available configuration options:"
    echo "   hdmi, HDMI, h:  Set sound to come out the HDMI connection"
    echo "   speaker, speakers, s:  Set sound to come out the laptop speakers"
    echo ""
    echo "If no configuration is given:"
    echo "   HDMI connected:  Set sound to come out the HDMI connection"
    echo "   HDMI disconnected:  Set sound to come out the laptop speakers"
}


#functions for doing the actual setting of the audio

#just the laptop speakers (or headphones if they're plugged in)
audio_speakers () {
    pacmd set-card-profile 0 output:analog-stereo
}

#HDMI speakers
audio_hdmi () {
    pacmd set-card-profile 0 output:hdmi-stereo
}


#actually call it
main $@

