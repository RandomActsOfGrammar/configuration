#!/bin/bash

#This is probably not a super robust script.  If nothing else, though,
#it can serve as an example of how to add a network by hand.

#the following should be the default, but do it anyway
nmcli r wifi on

#show all the possibilities
nmcli d wifi list
#choose a network from the list
echo -n "Nework SSID (from list):  "
read network

#get the password for the network
echo -n "Network password:  "
read password

#actually connect
nmcli d wifi connect "$network" password "$password"
