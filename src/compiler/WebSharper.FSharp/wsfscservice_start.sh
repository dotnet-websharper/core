#!/usr/bin/env bash

nohup "$(dirname "$BASH_SOURCE")/wsfscservice" >/dev/null 2>&1 &
