#!/bin/sh

cd ../spictapp
R --vanilla < runapp.R

## close terminal after execution
osascript -e 'tell application "Terminal" to quit'
