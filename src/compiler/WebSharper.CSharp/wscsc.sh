#!/usr/bin/env bash

exec dotnet "$(dirname "$BASH_SOURCE")/wscsc.dll" "$@"
