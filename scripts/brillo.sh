#!/bin/sh

if [ -z "$1" ]; then
    cat /sys/class/backlight/amdgpu_bl0/brightness
else
    echo $1 | sudo tee /sys/class/backlight/amdgpu_bl0/brightness
fi
