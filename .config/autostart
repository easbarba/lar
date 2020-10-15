#!/usr/bin/env bash

# DESCRIPTION: Auto start software

APPS=(udiskie emacs unclutter mpd clipit nm-applet konsole)
# [[ ! $WM == "awesomewm" ]] apps +=(dunst )

not-found() { [[ ! -x $(command -v ${1}) ]]; }

running() { [[ $(pgrep -f ${1}) ]]; }

run() { ${@} & }

for app in ${APPS[@]}
do
    not-found ${app} && continue
    running ${app} && continue

    run ${app}
done
