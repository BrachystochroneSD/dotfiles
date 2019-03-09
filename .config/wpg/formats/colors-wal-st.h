const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#362121", /* black   */
  [1] = "#A88879", /* red     */
  [2] = "#D4955E", /* green   */
  [3] = "#DAD063", /* yellow  */
  [4] = "#E77C9B", /* blue    */
  [5] = "#B49597", /* magenta */
  [6] = "#E4AF99", /* cyan    */
  [7] = "#eadeca", /* white   */

  /* 8 bright colors */
  [8]  = "#a39b8d",  /* black   */
  [9]  = "#A88879",  /* red     */
  [10] = "#D4955E", /* green   */
  [11] = "#DAD063", /* yellow  */
  [12] = "#E77C9B", /* blue    */
  [13] = "#B49597", /* magenta */
  [14] = "#E4AF99", /* cyan    */
  [15] = "#eadeca", /* white   */

  /* special colors */
  [256] = "#362121", /* background */
  [257] = "#eadeca", /* foreground */
  [258] = "#eadeca",     /* cursor */
};

/* Default colors (colorname index)
 * foreground, background, cursor */
 unsigned int defaultbg = 0;
 unsigned int defaultfg = 257;
 unsigned int defaultcs = 258;
 unsigned int defaultrcs= 258;
