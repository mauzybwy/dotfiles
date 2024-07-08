#!/bin/sh

# Some events send additional information specific to the event in the $INFO
# variable. E.g. the front_app_switched event sends the name of the newly
# focused application in the $INFO variable:
# https://felixkratz.github.io/SketchyBar/config/events#events-and-scripting

Emacs="dooom "

if [ "$SENDER" = "front_app_switched" ]; then
  APP="${!INFO}"
  if [ ! "$APP" ]; then
    APP="$INFO"
  fi
  sketchybar --set "$NAME" label="$APP"
fi
