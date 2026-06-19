#!/bin/sh

for x in $(hyprctl clients -j | jq -r '.[] | .address'); do
  hyprctl dispatch settiled address:$x
done
