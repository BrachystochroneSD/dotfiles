static const char norm_fg[] = "#dee9cc";
static const char norm_bg[] = "#2c2219";
static const char norm_border[] = "#9ba38e";

static const char sel_fg[] = "#dee9cc";
static const char sel_bg[] = "#69B791";
static const char sel_border[] = "#dee9cc";

static const char urg_fg[] = "#dee9cc";
static const char urg_bg[] = "#929968";
static const char urg_border[] = "#929968";

static const char *colors[][3]      = {
    /*               fg           bg         border                         */
    [SchemeNorm] = { norm_fg,     norm_bg,   norm_border }, // unfocused wins
    [SchemeSel]  = { sel_fg,      sel_bg,    sel_border },  // the focused win
    [SchemeUrg] =  { urg_fg,      urg_bg,    urg_border },
};
