#!/bin/bash

source "$CONFIG_DIR/colors.sh"

PERCENTAGE=$(df -H /System/Volumes/Data | awk 'END {print $5}' | sed 's/%//')
AVAIL=$(df -H /System/Volumes/Data | awk 'END {print $4}')

case ${PERCENTAGE} in
    9[8-9] | 100)
        ICON="󰪥"
        COLOR=$RED
        ;;
    8[8-9] | 9[0-7])
        ICON="󰪤"
        COLOR=$ORANGE
        ;;
    7[6-9] | 8[0-7])
        ICON="󰪣"
        COLOR=$YELLOW
        ;;
    6[4-9] | 7[0-5])
        ICON="󰪢"
        ;;
    5[2-9] | 6[0-3])
        ICON="󰪡"
        ;;
    4[0-9] | 5[0-1])
        ICON="󰪠"
        ;;
    2[8-9] | 3[0-9])
        ICON="󰪟"
        ;;
    1[6-9] | 2[0-7])
        ICON="󰪞"
        ;;
    [0-9] | 1[0-5])
        ICON="󰝦"
        ;;
    *)
        # Handle other cases if needed
        ICON="󰅚"
        COLOR=$HIGHLIGHT
        ;;
esac

sketchybar --set "$NAME" icon="$ICON" icon.color="$COLOR" label="$AVAIL"
