#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)
cd $HERE

cabal new-build vty
cabal test vty-tests
