[meta]
version = 1
name = "Jake"
description = "Jake theme"
variant = "dark"
icon = "icons/omarchy.png"

[colors.core]
background = "{{ color0 }}"               # UI background (darker than editor)
foreground = "{{ foreground }}"           # Main text
secondary_background = "{{ background }}" # Editor background (elevated)
border = "{{ color4 }}"                   # Line color
accent = "{{ accent }}"                   # Tag color

[colors.accents]
blue = "{{ color4 }}"                     # Entity
green = "{{ color2 }}"                    # String
magenta = "{{ color5 }}"                  # Constant
orange = "{{ color11 }}"                  # Keyword
purple = "{{ color12 }}"                  # Constant
red = "{{ color1 }}"                      # Error/VCS removed
yellow = "{{ color3 }}"                   # Function
cyan = "{{ color6 }}"                     # Regexp

[colors.list.item.selection]
background = "{{ color8 }}" # Slightly lighter than UI bg
secondary_background = "{{ color5 }}"     # Line color on secondary

[colors.list.item.hover]
background = "{{ color8 }}"               # Very subtle hover
