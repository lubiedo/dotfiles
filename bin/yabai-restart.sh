#!/bin/bash
killall yabai 2>/dev/null
(
  cd /tmp && nohup yabai &
)
