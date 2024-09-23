#! /bin/sh

direction=$1

if [ $direction == "cycle" ]; then
    # gets the window with the oldest focus history
    pid=$(hyprctl -j clients | jq -r "sort_by(.focusHistoryID) | reverse | .[0] | .pid")
else
    # gets the most recently focussed other window
    pid=$(hyprctl -j clients | jq -r "sort_by(.focusHistoryID) | .[1] | .pid")
fi

hyprctl dispatch focuswindow pid:${pid}
# can only bring window to the front if it is floating
hyprctl dispatch setfloating pid:${pid}
# focussing does not automatically bring a window to the front
hyprctl dispatch alterzorder top,pid:${pid}
