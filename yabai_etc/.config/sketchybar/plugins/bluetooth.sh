#!/bin/bash

source "$CONFIG_DIR/colors.sh"

STATE="$(system_profiler SPBluetoothDataType -json | jq -r '.SPBluetoothDataType | .[0] | .controller_properties | .controller_state')"
DEVICE="$(system_profiler SPBluetoothDataType -json | jq -r '.SPBluetoothDataType | .[0] | .device_connected | .[0] | keys | .[0]')"

case "$STATE" in
    "attrib_on")
        ICON="󰂯"
        COLOR=$BLUE
        ;;
    "attrib_off")
        ICON="󰂲"
        COLOR=$RED
        DEVICE="[off]"
        ;;
    *)
        ICON=""
        COLOR=$RED
        ;;
esac

if [[ "$DEVICE" && "$DEVICE" != "[off]" ]]; then
    COLOR=$GREEN
    ICON="󰂱"
elif [[ ! "$DEVICE" || "$DEVICE" = "null" ]]; then
    COLOR=$BLUE
    DEVICE="[none]"
fi

sketchybar --set "$NAME" icon="$ICON" icon.color="$COLOR" icon.padding_right=4 icon.padding_left=5 label.drawing="off" label="$DEVICE" label.max_chars=auto scroll_texts="true" click_script="open 'x-apple.systempreferences:com.apple.preferences.Bluetooth'"
