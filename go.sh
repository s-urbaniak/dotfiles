#!/bin/sh

goget="go get -u -v"

$goget github.com/rogpeppe/godef
$goget github.com/nsf/gocode
$goget github.com/s-urbaniak/agoc
$goget github.com/s-urbaniak/acme/cmd/apl

for tool in "oracle" "goimports" "present" "godoc" "vet" "eg"; do
    $goget "golang.org/x/tools/cmd/$tool"
done

