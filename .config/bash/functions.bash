s_mkc() {
    mkdir "$1" && cd "$1" || echo "Could not create/enter folder!"
}

s_decimal_binary() {
    local decimal=$1
    local bin=""
    while [ $decimal -gt 0 ]; do
        bin=$(( decimal % 2 ))$bin
        decimal=$(( decimal / 2 ))
    done
    echo $bin
}
