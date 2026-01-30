. "${HOME}/.config/colors.sh"

NOTIFY_CMD="notify-send -e -i owl"
# MENU_CMD="wofi -d -M=fuzzy -i -p"
MENU_CMD="bemenu --fn FiraCode
--cb $color0
--cf $color15
--ab $color0
--af $color15
--tb $color0
--tf $color15
--fb $color0
--ff $color15
--nb $color0
--nf $color15
--hb $color3
--hf $color0
--sb $color3
--sf $color0"
# MENU_SECRET="wofi -d -P"
MENU_SECRET="$MENU_CMD --ff $color0"
TYPE_CMD="wtype -"
