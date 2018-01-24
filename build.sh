#!/bin/bash

set -e

export DOTNETSOLUTION=WebSharper.sln
exec tools/WebSharper.Fake.sh "$@"
