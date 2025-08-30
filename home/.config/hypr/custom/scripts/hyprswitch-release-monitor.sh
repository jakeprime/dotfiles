#!/bin/bash

evtest /dev/input/event3 |
  while read -r line; do
    if [[ "$line" =~ "KEY_LEFTMETA" || "$line" =~ "KEY_RIGHTMETA" ]]; then
      if [[ "$line" =~ "value 0" ]]; then
        hyprswitch close
        hyprctl dispatch submap reset
      fi
    fi
  done
