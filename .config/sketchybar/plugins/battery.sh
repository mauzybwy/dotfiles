#!/bin/sh

source "$CONFIG_DIR/colors.sh"

PERCENTAGE="$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)"
CHARGING="$(pmset -g batt | grep 'AC Power')"

if [ "$PERCENTAGE" = "" ]; then
exit 0
fi

case "${PERCENTAGE}" in
  9[0-9]|100)
    COLOR=$GREEN
    ICON=""
    ;;
  [6-8][0-9])
    COLOR=$GREEN
    ICON=""
    ;;
  [3-5][0-9])
    COLOR=$GREEN
    ICON=""
    ;;
  1[6-9] | 2[0-9])
    COLOR=$YELLOW
    ICON=""
    ;;
  [0-9] | 1[0-5])
    ICON=""
    COLOR=$RED
    ;;
  *)
    ICON="󰅚"
    COLOR=$HIGHLIGHT
    ;;
esac

if [[ "$CHARGING" != "" ]]; then
COLOR=$YELLOW
ICON=""
fi

# The item invoking this script (name $NAME) will get its icon and label
# updated with the current battery status
sketchybar --set "$NAME" icon="$ICON" icon.color="$COLOR" label="${PERCENTAGE}%"
