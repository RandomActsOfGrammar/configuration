# configuration
Configuration files and shellscripts I do not want to lose if anything
happens to my computer

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
