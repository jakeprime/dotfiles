#! /bin/sh

# gets the window with the oldest focus history
pid=$(hyprctl -j clients | jq -r "sort_by(.focusHistoryID) | reverse | .[0] | .pid")

hyprctl dispatch focuswindow pid:${pid}
