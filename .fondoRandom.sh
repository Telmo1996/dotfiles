#!/bin/sh
fondo=`find fondosPantalla -type f | shuf -n 1`
feh --bg-scale $HOME/$fondo
