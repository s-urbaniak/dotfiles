#!/bin/bash

goget=("go" "get" "-u" "-v")

for tool in \
    "github.com/uudashr/gopkgs/cmd/gopkgs" \
    "github.com/nsf/gocode" \
    "github.com/tpng/gopkgs" \
    "github.com/rogpeppe/godef" \
    "github.com/golang/dep/cmd/dep"; do
    "${goget[@]}" "${tool}"
done

for tool in "gomvpkg" "gorename" "guru" "goimports" "present" "godoc" "vet" "eg"; do
    "${goget[@]}" "golang.org/x/tools/cmd/$tool"
done

"${goget[@]}" github.com/alecthomas/gometalinter
gometalinter --install --update

