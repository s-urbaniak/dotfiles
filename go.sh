#!/bin/sh

goget="go get -u -v"

$goget github.com/rogpeppe/godef
$goget github.com/nsf/gocode

for tool in "gomvpkg" "gorename" "guru" "goimports" "present" "godoc" "vet" "eg"; do
    $goget "golang.org/x/tools/cmd/$tool"
done

$goget github.com/alecthomas/gometalinter
gometalinter --install --update

