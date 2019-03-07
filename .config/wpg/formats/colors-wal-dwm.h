static const char norm_fg[] = "#dfe7c3";
static const char norm_bg[] = "#2d2d1e";
static const char norm_border[] = "#9ca188";

static const char sel_fg[] = "#dfe7c3";
static const char sel_bg[] = "#71BD82";
static const char sel_border[] = "#dfe7c3";

static const char urg_fg[] = "#dfe7c3";
static const char urg_bg[] = "#E1CD7C";
static const char urg_border[] = "#E1CD7C";

static const char *colors[][3]      = {
    /*               fg           bg         border                         */
    [SchemeNorm] = { norm_fg,     norm_bg,   norm_border }, // unfocused wins
    [SchemeSel]  = { sel_fg,      sel_bg,    sel_border },  // the focused win
    [SchemeUrg] =  { urg_fg,      urg_bg,    urg_border },
};
