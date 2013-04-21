#!/bin/bash

sed -n '
/^  <topic id/,/^  <\/topic>/ {
  /^  <topic/ {
    s:.*<topic id="\([^"]*\)".*:\1:
    h
  }
  /^\t<word/ {
    s:^\t<word weight="\([^"]*\)" count="\([^"]*\)">\(.*\)</word>$:\1, \2, "\3":
    G
    s:\(.*\)\n\(.*\):\2, \1:
    p
  }
}
'
