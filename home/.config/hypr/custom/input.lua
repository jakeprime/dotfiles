-- Control your input devices
-- See https://wiki.hypr.land/Configuring/Variables/#input

-- Scroll faster in the terminal
hl.window_rule({
    match = {
      class = "Alacritty",
    },
    scroll_touchpad = 1.5,
})

hl.config({
    input = {
      follow_mouse = 2,
      repeat_rate = 40,
      repeat_delay = 250,
      touchpad = {
        natural_scroll = true,
        scroll_factor = 0.4,
        tap_to_click = true,
        tap_and_drag = true,
        drag_lock = 2,
      },
    },
})

require("custom.input.keyboard")
