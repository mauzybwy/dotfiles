#!/bin/zsh

source "$CONFIG_DIR/colors.sh"

# The $SELECTED variable is available for space components and indicates if
# the space invoking this script (with name: $NAME) is currently selected:
# https://felixkratz.github.io/SketchyBar/config/components#space----associate-mission-control-spaces-with-an-item

# sketchybar --set "$NAME" background.drawing="$SELECTED"

WIN=$(yabai -m query --spaces --space $SID | jq '.windows[0]')
SHOW_SPACE="false"
if [[ "$WIN" != "null" ]]; then
  SHOW_SPACE="true"
elif [[ "$SELECTED" = "true"  ]]; then
  SHOW_SPACE="true"
  sketchybar --set $NAME label="--"
else
  sketchybar --set $NAME label="--"
fi

sketchybar --set $NAME \
    background.drawing=$SELECTED \
    drawing=$SHOW_SPACE
