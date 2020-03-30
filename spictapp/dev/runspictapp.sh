#!/bin/sh

R --vanilla < runapp.R

## close terminal after execution
osascript -e 'tell application "Terminal" to quit'
