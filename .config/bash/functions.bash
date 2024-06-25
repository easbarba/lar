s_mkc() {
    mkdir "$1" && cd "$1" || echo "Could not create/enter folder!"
}


s_binary_decimal() {
    # desc: from binary to decimal
    echo $((2#$1))
}


s_decimal_binary() {
    # desc: from to decimal to binary

    local decimal=$1
    local binary=""

    while [ "$decimal" -gt 0 ]; do
        binary=$(( decimal % 2 ))$binary
        decimal=$(( decimal / 2 ))
    done

    echo $binary
}
