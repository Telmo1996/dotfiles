#!/bin/bash

parentData=`ps -e | grep $PPID`
IFS=' '
read -ra arr <<< "$parentData"
parentName=${arr[3]}


if [ "$parentName" = "ranger" ];then
	echo saliendo
else
	$SHELL
fi
