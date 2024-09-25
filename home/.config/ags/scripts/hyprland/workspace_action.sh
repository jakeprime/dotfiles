#!/usr/bin/env bash
hyprctl dispatch alterzorder top
hyprctl dispatch "$1" $(((($(hyprctl activeworkspace -j | jq -r .id) - 1)  / 10) * 10 + $2))
