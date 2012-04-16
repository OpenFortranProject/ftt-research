#!/usr/bin/env bash

HERE="$(cd "$(dirname "$0")" ; pwd)"

source $HERE/test-harness.sh

run_cases ./build_opencl
