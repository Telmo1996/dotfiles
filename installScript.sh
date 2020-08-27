#!/bin/sh
sudo apt-get install rofi i3blocks lxappearance compton feh vim
#para la bateria: acpi

#para playerctl en ubuntu (i3blocks/mediaplayer)
wget http://ftp.nl.debian.org/debian/pool/main/p/playerctl/libplayerctl2_2.0.1-1_amd64.deb
wget http://ftp.nl.debian.org/debian/pool/main/p/playerctl/playerctl_2.0.1-1_amd64.deb
sudo dpkg -i libplayerctl2_2.0.1-1_amd64.deb playerctl_2.0.1-1_amd64.deb

sudo apt-get install alacritty

#scripts wapos pa flexear colores
git clone https://github.com/stark/color-scripts

#para cambiar el editor
sudo update-alternatives --config editor
