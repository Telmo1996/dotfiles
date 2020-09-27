#!/bin/sh

echo $1 | sudo tee /sys/class/backlight/amdgpu_bl0/brightness
