#!/bin/bash


active_is_floating() {
    hyprctl activewindow | grep -q "floating: 1"
}

hyprctl dispatch togglefloating

if ! active_is_floating; then
    exit 0
fi


# Récupérer les dimensions de l'écran
screen_resolution=$(hyprctl monitors | grep -A 1 'Monitor eDP-1' | grep -oP '(\d\d+)x(\d\d+)')
screen_width=${screen_resolution%x*}
screen_height=${screen_resolution#*x}

[ -z "$screen_width" ] && echo "No screen width" && exit 1
[ -z "$screen_height" ] && echo "No screen height" && exit 1

# Calculer les nouvelles dimensions et positions
new_width=$((screen_width / 4))
new_height=$((screen_height / 4))
x_position=$((screen_width - new_width))
y_position=$((screen_height - new_height))

# Appliquer les changements à la fenêtre active
hyprctl dispatch setfloating true
hyprctl dispatch resizeactive exact 25% 25%
hyprctl dispatch moveactive exact 75% 75%
