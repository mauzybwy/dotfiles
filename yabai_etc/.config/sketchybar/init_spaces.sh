#!/bin/zsh

SID=$1
APP_NAME=$2

declare -A apps
apps[Emacs]=""
apps[iTerm2]=""
apps[Kitty]=""
apps[Spotify]=""
apps[Arc]="󰖟"
apps[Firefox]=""
apps[Slack]=""
apps[Finder]=""
apps[Preview]=""
apps[System]=""
a="System Settings"
apps[$a]=""
apps[Simulator]=""

APP_ICON=${apps[$APP_NAME]}
if [[ ! "$APP_ICON" ]]; then
    APP_ICON=$APP_NAME
fi

sketchybar --set space.$SID label=$APP_ICON
