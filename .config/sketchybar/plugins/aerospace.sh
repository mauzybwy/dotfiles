#!/usr/bin/env zsh

# make sure it's executable with:
# chmod +x ~/.config/sketchybar/plugins/aerospace.sh

source "$CONFIG_DIR/icons.sh"

sketchybar --set debug label="HERE"

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME background.drawing=on
else
    sketchybar --set $NAME background.drawing=off
fi

WIN=$(aerospace list-windows --workspace $1 | awk 'BEGIN {FS="|"}; {print $2; exit}')
# sketchybar --set $NAME label="$WIN"

WINDOWS=$(aerospace list-windows --workspace $1)

ICONS=""
echo $WINDOWS | while IFS='|' read -r PID APP_NAME TITLE; do
    TRIMMED=$(echo $APP_NAME | xargs)
    APP_ICON=${apps[$TRIMMED]}
    if [[ ! "$APP_ICON" ]]; then
        APP_ICON="$TRIMMED"
    fi

    GAP=""
    if [[ "$ICONS" ]]; then
        GAP=" "
    fi

    ICONS="${ICONS}$GAP${APP_ICON}"
    # sketchybar --set $NAME label="ASDF"
done

if [[ ! "$ICONS" ]]; then
    # sketchybar --set $NAME drawing=off
fi

sketchybar --set $NAME label="$ICONS"
