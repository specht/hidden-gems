#!/bin/sh
set -e
javac -cp json.jar Bot.java
exec java -cp .:json.jar Bot
