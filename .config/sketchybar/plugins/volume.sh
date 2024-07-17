#!/bin/sh

# The volume_change event supplies a $INFO variable in which the current volume
# percentage is passed to the script.

source "$CONFIG_DIR/colors.sh"

VOLUME="$INFO"

STATE="$(system_profiler SPBluetoothDataType -json | jq -r '.SPBluetoothDataType | .[0] | .controller_properties | .controller_state')"
DEVICE="$(system_profiler SPBluetoothDataType -json | jq -r '.SPBluetoothDataType | .[0] | .device_connected | .[0] | keys | .[0]')"

case "$STATE" in
    "attrib_on")
        BTICON="󰂯"
        COLOR=$BLUE
        ;;
    "attrib_off")
        BTICON="󰂲"
        COLOR=$RED
        DEVICE="[off]"
        ;;
    *)
        BTICON=""
        COLOR=$RED
        ;;
esac

if [[ "$DEVICE" && "$DEVICE" != "[off]" ]]; then
    COLOR=$GREEN
    BTICON="󰂱"
elif [[ ! "$DEVICE" || "$DEVICE" = "null" ]]; then
    COLOR=$BLUE
    DEVICE="[none]"
fi

if [ "$SENDER" = "volume_change" ]; then
  VOLUME="$INFO"

  case "$VOLUME" in
    [6-9][0-9]|100) ICON="󰕾"
    ;;
    [3-5][0-9]) ICON="󰖀"
    ;;
    [1-9]|[1-2][0-9]) ICON="󰕿"
    ;;
    *) ICON="󰖁"
  esac

  sketchybar --set "$NAME" icon="$BTICON" icon.color="$COLOR" label="$VOLUME% $ICON " click_script="open 'x-apple.systempreferences:com.apple.preferences.Bluetooth'"
fi
