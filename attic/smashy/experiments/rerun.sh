#!/usr/bin/env bash

while true ; do
  echo start
  /usr/bin/time -f "USAGE: E %E K %S U %U P %P M %M kB O %O" $* 2>&1
  echo done: $$
  sleep 1
done
