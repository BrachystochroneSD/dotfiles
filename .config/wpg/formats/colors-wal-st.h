const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#2c2219", /* black   */
  [1] = "#929968", /* red     */
  [2] = "#69B791", /* green   */
  [3] = "#99A28A", /* yellow  */
  [4] = "#B9BB85", /* blue    */
  [5] = "#B6C186", /* magenta */
  [6] = "#C4DFAA", /* cyan    */
  [7] = "#dee9cc", /* white   */

  /* 8 bright colors */
  [8]  = "#9ba38e",  /* black   */
  [9]  = "#929968",  /* red     */
  [10] = "#69B791", /* green   */
  [11] = "#99A28A", /* yellow  */
  [12] = "#B9BB85", /* blue    */
  [13] = "#B6C186", /* magenta */
  [14] = "#C4DFAA", /* cyan    */
  [15] = "#dee9cc", /* white   */

  /* special colors */
  [256] = "#2c2219", /* background */
  [257] = "#dee9cc", /* foreground */
  [258] = "#dee9cc",     /* cursor */
};

/* Default colors (colorname index)
 * foreground, background, cursor */
 unsigned int defaultbg = 0;
 unsigned int defaultfg = 257;
 unsigned int defaultcs = 258;
 unsigned int defaultrcs= 258;
