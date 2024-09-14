#!/bin/sh

position=$1

params() {
    local x=$(echo "scale=0; ($1 + 0.5) / 1" | bc)
    local y=$(echo "scale=0; ($2 + 0.5) / 1" | bc)
    echo "exact $x $y"
}

math() {
    local result=$(echo "scale=3; $1" | bc)
    echo $result
}

screen_width=$(hyprctl -j monitors | jq -r ".[0].width")
screen_height=$(hyprctl -j monitors | jq -r ".[0].height")
taskbar_height=$(hyprctl -j monitors | jq -r ".[0].reserved[1]")
scale=$(hyprctl -j monitors | jq -r ".[0].scale")

gaps_out=$(hyprctl -j getoption "general:gaps_out" | jq -r ".custom | split(\" \") | .[0]")
gaps_in=$(hyprctl -j getoption "general:gaps_in" | jq -r ".custom | split(\" \") | .[0]")
border_size=$(hyprctl -j getoption "general:border_size" | jq -r ".int")

origin_x=$gaps_out
origin_y=$(math "$taskbar_height + $gaps_out + $border_size")
usable_width=$(math "($screen_width / $scale) - (2 * $gaps_out)")
usable_height=$(math "$screen_height / $scale - $origin_y - $gaps_out - $border_size")


case $position in
    "left-half"|"right-half")
        window_width=$(math "($usable_width / 2) - ($gaps_in + $border_size)")
        ;;
    "left-third"|"center-third"|"right-third")
        window_width=$(math "($usable_width / 3) - (2 * ($gaps_in + $border_size))")
        ;;
    "fullscreen")
        window_width=$usable_width
        ;;
    *)
esac
resize_params=$(params $window_width $usable_height)

case $position in
    "left-half"|"left-third"|"fullscreen")
        window_x=$(math "$origin_x + $gaps_in")
        ;;
    "center-third")
        window_x=$(math "$origin_x + ($usable_width / 3) + (1 * ($gaps_in + $border_size))")
        ;;
    "right-third")
        window_x=$(math "$origin_x + 2 * ($usable_width / 3) + (1 * ($gaps_in + $border_size))")
        move_params="exact $window_x $origin_y"
        ;;
    "right-half")
        window_x=$(math "$origin_x + ($usable_width / 2) + (1 * ($gaps_in + $border_size))")
        move_params="exact $window_x $origin_y"
        ;;
    *)
esac
move_params=$(params $window_x $origin_y)

hyprctl dispatch setfloating
hyprctl dispatch resizeactive $resize_params
hyprctl dispatch moveactive $move_params
