const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#2d2d1e", /* black   */
  [1] = "#E1CD7C", /* red     */
  [2] = "#71BD82", /* green   */
  [3] = "#73C288", /* yellow  */
  [4] = "#90CE9A", /* blue    */
  [5] = "#BBD899", /* magenta */
  [6] = "#89D3A5", /* cyan    */
  [7] = "#dfe7c3", /* white   */

  /* 8 bright colors */
  [8]  = "#9ca188",  /* black   */
  [9]  = "#E1CD7C",  /* red     */
  [10] = "#71BD82", /* green   */
  [11] = "#73C288", /* yellow  */
  [12] = "#90CE9A", /* blue    */
  [13] = "#BBD899", /* magenta */
  [14] = "#89D3A5", /* cyan    */
  [15] = "#dfe7c3", /* white   */

  /* special colors */
  [256] = "#2d2d1e", /* background */
  [257] = "#dfe7c3", /* foreground */
  [258] = "#dfe7c3",     /* cursor */
};

/* Default colors (colorname index)
 * foreground, background, cursor */
 unsigned int defaultbg = 0;
 unsigned int defaultfg = 257;
 unsigned int defaultcs = 258;
 unsigned int defaultrcs= 258;
