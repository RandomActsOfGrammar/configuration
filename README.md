# configuration
This contains configuration files and shellscripts I do not want to
lose if anything happens to my computer.  I place this Git repository
at `~/configuration`.

## settitle
This script takes whatever arguments it is given and changes the title
of the terminal to them.  Since it takes all the arguments and places
them in the title, it is not necessary to use quotation marks for
arguments, that is, 'settitle "two words"' is the same as 'settitle
two words', with the result being a title of 'two words'.

## PS1_rotater
This script, along with the last section of the .bashrc file, changes
the color and text style of the prompt each time it is displayed.  The
PS1_TEXT variable can be changed to any prompt desired; this includes
standard PS1 escape sequences (Note that this is only included at the
beginning of a terminal session; changing PS1_TEXT in the middle of a
session will not change what is displayed in the prompt).  The prompts
go through several colors and text styles.  It has no useful purpose;
it is only intended for fun.

## set_screens.sh
Set the display screens, sizes, and orientations based on arguments if
given, or whether there is an HDMI connection if no arguments are
given.

## set_audio.sh
Set the audio to the internal speakers or the HDMI condition based on
arguments if given, or whether there is an HDMI connection if no
arguments are given.

## wifi_add.sh
This takes care of the boilerplate of adding a wifi connection by
walking you through selecting an available network and entering the
password.

## background.sh
Set the background image using `feh`.  The file includes options for
using either a single image or images in the directory.

## enable_tapping.sh
Identify the touchpad and the property for enabling tapping on it to
set it to allow tapping.  Uses `xinput` for identifying and setting.

## bashrc.sh
Configuration file for Bash.  This can be loaded without any links or
copying by putting a file including `source ~/configuration/bashrc.sh`
in the regular Bash configuration file location.

## emacs.el
Configuration file for Emacs.  This can be loaded without any links or
copying by putting a file including
`(load "~/configuration/emacs.el")` in the regular Emacs configuration
file location.

## i3_config
Configuration file for the i3 window manager.

## i3status_config
Configuration file for i3status.

## termite_config
Configuration file for the termite terminal emulator.

## silver-mode.el
An Emacs mode for Silver grammars that I created before I realized
that there was a mode already written.  It has some issues.
