const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#092A10", /* black   */
  [1] = "#A29F58", /* red     */
  [2] = "#AFA95D", /* green   */
  [3] = "#FE8D72", /* yellow  */
  [4] = "#ECB076", /* blue    */
  [5] = "#F7BF78", /* magenta */
  [6] = "#EB9F8A", /* cyan    */
  [7] = "#f5ccbb", /* white   */

  /* 8 bright colors */
  [8]  = "#ab8e82",  /* black   */
  [9]  = "#A29F58",  /* red     */
  [10] = "#AFA95D", /* green   */
  [11] = "#FE8D72", /* yellow  */
  [12] = "#ECB076", /* blue    */
  [13] = "#F7BF78", /* magenta */
  [14] = "#EB9F8A", /* cyan    */
  [15] = "#f5ccbb", /* white   */

  /* special colors */
  [256] = "#092A10", /* background */
  [257] = "#f5ccbb", /* foreground */
  [258] = "#f5ccbb",     /* cursor */
};

/* Default colors (colorname index)
 * foreground, background, cursor */
 unsigned int defaultbg = 0;
 unsigned int defaultfg = 257;
 unsigned int defaultcs = 258;
 unsigned int defaultrcs= 258;
