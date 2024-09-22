#!/bin/sh

keyboard=$1

ln -fs ~/.config/hypr/custom/keyboard.${keyboard}.conf ~/.config/hypr/custom/keyboard.conf
hyprctl reload
