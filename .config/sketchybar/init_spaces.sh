#!/bin/zsh

source "$CONFIG_DIR/icons.sh"

WINDOWS=$(yabai -m query --windows | jq -r 'sort_by(.id) | .[] | "\(.space)|\(.app)|\(."has-focus")"')

declare -A screens
echo $WINDOWS | while IFS='|' read -r SID APP_NAME FOCUSED; do
    APP_ICON=${apps[$APP_NAME]}
    if [[ ! "$APP_ICON" ]]; then
        APP_ICON=$APP_NAME
    fi

    GAP=""
    if [[ "$screens[$SID]" ]]; then
        GAP=" "
    fi

    screens[$SID]="${screens[$SID]}$GAP$APP_ICON"
done

for key val in ${(@kv)screens[@]}; do
    if [[ "$val" = "" ]]; then
        val="--"
    fi
    sketchybar --set space.$key label="$val"
done
