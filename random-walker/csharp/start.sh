#!/bin/sh
set -e

# Ensure project exists
if [ ! -f "RandomWalker.csproj" ]; then
  # Create a minimal project if missing
  dotnet new console -n RandomWalker -o . --force
fi

# Build in Release mode
dotnet build -c Release
dotnet publish -c Release -r linux-x64 --self-contained true
./bin/Release/net9.0/linux-x64/publish/RandomWalker
