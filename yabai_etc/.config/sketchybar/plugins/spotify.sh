#!/usr/bin/env bash

source "$CONFIG_DIR/colors.sh"


PLAYING=1
if [ "$(echo "$INFO" | jq -r '.["Player State"]')" = "Playing" ]; then
    PLAYING=0
    TRACK="$(echo "$INFO" | jq -r .Name)"
    ARTIST="$(echo "$INFO" | jq -r .Artist)"
    ALBUM="$(echo "$INFO" | jq -r .Album)"
fi

ICON="󰓇"
COLOR="$GREEN"
DRAWING="off"
if [ $PLAYING -eq 0 ]; then
    DRAWING="on"
    if [ "$ARTIST" == "" ]; then
        LABEL="$ALBUM - $TRACK"
    else
        LABEL="$ARTIST - $TRACK"
    fi
fi

sketchybar --set music icon="$ICON" drawing="$DRAWING" icon.color="$COLOR" label="$LABEL" scroll_texts="true" label.max_chars=20
