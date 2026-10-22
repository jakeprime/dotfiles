#!/bin/sh

keyboard=$1

pushd ~/.config/hypr/custom/input
ln -fs keyboard.${keyboard}.lua keyboard.lua

notify-send "Changed keyboard to $keyboard"
hyprctl reload
