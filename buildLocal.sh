#!/usr/bin/env bash

set -eu
set -o pipefail

echo "Restoring dotnet tools..."
dotnet tool restore
echo "Restoring project dependencies..."
dotnet restore

FAKE_DETAILED_ERRORS=true dotnet build --no-restore
