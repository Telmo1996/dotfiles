#!/bin/sh
sudo apt-get install rofi i3blocks lxappearance compton feh vim
#para la bateria: acpi

#para playerctl en ubuntu (i3blocks/mediaplayer)
wget http://ftp.nl.debian.org/debian/pool/main/p/playerctl/libplayerctl2_2.0.1-1_amd64.deb
wget http://ftp.nl.debian.org/debian/pool/main/p/playerctl/playerctl_2.0.1-1_amd64.deb
sudo dpkg -i libplayerctl2_2.0.1-1_amd64.deb playerctl_2.0.1-1_amd64.deb

#para instalar alacritty:
#en VM pantalla -> contolador grafico -> VBoxVGA
#https://www.tecmint.com/alacritty-fastest-terminal-emulator-for-linux/

#scripts wapos pa flexear colores
git clone https://github.com/stark/color-scripts

#para cambiar el editor
sudo update-alternatives --config editor

# para brillo portatil
# https://unix.stackexchange.com/questions/526653/control-screen-brightness-in-i3

#Añadir al final de ~/.bashrc
#Scripts Directory
PATH=$HOME/scripts:$PATH

#crear /etc/sudoers.d/brillo y editarlo con visudo
telmo ALL=(ALL:ALL) NOPASSWD:/usr/bin/tee /sys/class/backlight/amdgpu_bl0/brightness
