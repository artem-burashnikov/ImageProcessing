#!/usr/bin/env bash

set -eu
set -o pipefail

FAKE_DETAILED_ERRORS=true dotnet test --verbosity normal
