#!/usr/bin/env zsh

#--------------------------------------------------------------------------------
# App Icons
#--------------------------------------------------------------------------------
declare -A apps
apps[Emacs]=""
apps[iTerm2]=""
apps[kitty]=""
apps[Spotify]=""
apps[Arc]="󰖟"
apps[Firefox]=""
apps[Slack]=""
apps[Finder]=""
apps[Preview]=""
apps[System]=""
apps[Simulator]=""
apps[Discord]="󰙯"
apps[Krita]=""
apps[NoMachine]="󰢹"
apps[Thunderbird]="󰴃"

# Special cases
name="System Settings"; apps[$name]=""
name="Google Chrome"; apps[$name]=""
name="Spark Desktop"; apps[$name]="󰇮"
