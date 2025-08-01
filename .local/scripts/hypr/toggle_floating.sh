#!/bin/bash


active_is_floating() {
    hyprctl activewindow | grep -q "floating: 1"
}

hyprctl dispatch togglefloating

if active_is_floating; then
    hyprctl dispatch resizeactive exact 50% 50%
    hyprctl dispatch moveactive exact 25% 25%
fi
