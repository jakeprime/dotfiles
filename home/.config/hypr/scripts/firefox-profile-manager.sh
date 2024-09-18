#!/bin/sh

profile=$1

# is there already an active session?
running=$(hyprctl -j clients | jq -r "first(.[] | select(.class == \"firefox\") | select(.tags[] | contains(\"$profile\")) | .pid)")

if [[ $running != "" ]]
then
	 hyprctl dispatch focuswindow pid:${running}
else
    firefox -P $profile &
    app_pid=$!
    sleep 1

    hyprctl dispatch tagwindow "$profile pid:$app_pid"
fi
