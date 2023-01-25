#!/usr/bin/env sh

mkc() {
    mkdir "$1" && cd "$1" || echo "Could not create/enter folder!"
}
