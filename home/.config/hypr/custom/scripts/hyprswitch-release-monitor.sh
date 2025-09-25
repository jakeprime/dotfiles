#!/bin/bash

for dev in /dev/input/by-path/*-event-kbd; do
  [ -e "$dev" ] || continue
  stdbuf -oL -eL evtest "$dev" 2>/dev/null | \
    while read -r line; do
      if [[ $line == *KEY_LEFTMETA* || $line == *KEY_RIGHTMETA* ]]; then
        if [[ $line == *"value 0"* ]]; then
          hyprswitch close
          hyprctl dispatch submap reset
        fi
      fi
    done &
done

wait
