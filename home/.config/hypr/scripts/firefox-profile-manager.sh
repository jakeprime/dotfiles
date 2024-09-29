#!/bin/sh

profile=$1

# is there already an active session?
running=$(hyprctl -j clients | jq -r "first(.[] | select(.class == \"firefox\") | select(.tags[] | contains(\"$profile\")) | .pid)")

if [[ $running != "" ]]
then
	 hyprctl dispatch focuswindow pid:${running}
	 hyprctl dispatch alterzorder top,pid:${running}
else
    firefox -P $profile &
    app_pid=$!

    while [[ -z "$(hyprctl -j clients | jq -r ".[] | select(.pid == $app_pid)")" ]]
    do
          sleep 0.1
    done

    hyprctl dispatch tagwindow "$profile pid:$app_pid"
fi
