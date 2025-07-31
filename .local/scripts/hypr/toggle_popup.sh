#!/bin/bash


active_is_floating() {
    hyprctl activewindow | grep -q "floating: 1"
}

hyprctl dispatch togglefloating

if ! active_is_floating; then
    exit 0
fi

hyprctl dispatch resizeactive exact 25% 25%
hyprctl dispatch moveactive exact 75% 75%
