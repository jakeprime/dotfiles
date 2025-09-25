#!/bin/sh

keyboard=$1

pushd ~/.config/hypr/custom/input
ln -fs keyboard.${keyboard}.conf keyboard.conf

notify-send "Changed keyhboard to $keyboard"
hyprctl reload
