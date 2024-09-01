#!/usr/bin/env zsh

# make sure it's executable with:
# chmod +x ~/.config/sketchybar/plugins/aerospace.sh

WORKSPACE_ID=$1
MONITOR_ID=$2

source "$CONFIG_DIR/icons.sh"

if [ "$WORKSPACE_ID" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set $NAME background.drawing=on
else
    sketchybar --set $NAME background.drawing=off
fi

WINDOWS=$(aerospace list-windows --workspace $WORKSPACE_ID)

if [[ ! "$WINDOWS" && "$WORKSPACE_ID" != "$FOCUSED_WORKSPACE" ]]; then
    sketchybar --set $NAME drawing=off
else
    sketchybar --set $NAME drawing=on
fi


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

sketchybar --set $NAME label="$ICONS" display="$MONITOR_ID"
