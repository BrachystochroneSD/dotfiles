static const char norm_fg[] = "#f6edd1";
static const char norm_bg[] = "#223322";
static const char norm_border[] = "#aca592";

static const char sel_fg[] = "#f6edd1";
static const char sel_bg[] = "#57A7B8";
static const char sel_border[] = "#f6edd1";

static const char urg_fg[] = "#f6edd1";
static const char urg_bg[] = "#817D9E";
static const char urg_border[] = "#817D9E";

static const char *colors[][3]      = {
    /*               fg           bg         border                         */
    [SchemeNorm] = { norm_fg,     norm_bg,   norm_border }, // unfocused wins
    [SchemeSel]  = { sel_fg,      sel_bg,    sel_border },  // the focused win
    [SchemeUrg] =  { urg_fg,      urg_bg,    urg_border },
};
