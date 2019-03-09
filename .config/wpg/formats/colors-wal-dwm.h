static const char norm_fg[] = "#eadeca";
static const char norm_bg[] = "#362121";
static const char norm_border[] = "#a39b8d";

static const char sel_fg[] = "#eadeca";
static const char sel_bg[] = "#D4955E";
static const char sel_border[] = "#eadeca";

static const char urg_fg[] = "#eadeca";
static const char urg_bg[] = "#A88879";
static const char urg_border[] = "#A88879";

static const char *colors[][3]      = {
    /*               fg           bg         border                         */
    [SchemeNorm] = { norm_fg,     norm_bg,   norm_border }, // unfocused wins
    [SchemeSel]  = { sel_fg,      sel_bg,    sel_border },  // the focused win
    [SchemeUrg] =  { urg_fg,      urg_bg,    urg_border },
};
