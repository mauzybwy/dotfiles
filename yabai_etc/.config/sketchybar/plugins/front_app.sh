#!/bin/zsh

# Some events send additional information specific to the event in the $INFO
# variable. E.g. the front_app_switched event sends the name of the newly
# focused application in the $INFO variable:
# https://felixkratz.github.io/SketchyBar/config/events#events-and-scripting

source "$CONFIG_DIR/icons.sh"

Emacs="Doooom"

if [ "$SENDER" = "front_app_switched" ]; then
  APP="${(P)INFO}"
  if [ ! "$APP" ]; then
    APP="$INFO"
  fi
  sketchybar --set "$NAME" label="$APP"

  SID=$(yabai -m query --spaces | jq '.[] | select(."is-visible" == true) | .index')
  $CONFIG_DIR/init_spaces.sh $SID $INFO
fi
