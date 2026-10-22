hl.window_rule({
    match = {
        class = ".*",
    },
    float = true,
})

hl.window_rule({
    match = {
        tag = "chromium-based-browser",
    },
    float = true,
})

hl.window_rule({
    match = {
        class = ".*",
    },
    opacity = "1 1",
})

-- layerrule = match:namespace gtk4-layer-shell, no_anim on
-- layerrule = match:namespace gtk4-layer-shell, blur on
-- layerrule = match:namespace gtk4-layer-shell, xray 0
-- layerrule = match:namespace gtk4-layer-shell, ignore_alpha 0
-- these are intended for Hyprswitch. It doesn't have it's own namespace, just
-- the gtk one, but I expect we'd want to the same for any other layer using the
-- same
hl.layer_rule({
    match = { namespace = "gtk4-layer-shell" },
    no_anim = true,
    blur = true,
    xray = false,
    ignore_alpha = 0.0,
})
