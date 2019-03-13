const char *colorname[] = {

  /* 8 normal colors */
  [0] = "#223322", /* black   */
  [1] = "#817D9E", /* red     */
  [2] = "#57A7B8", /* green   */
  [3] = "#52A8D4", /* yellow  */
  [4] = "#63ABD9", /* blue    */
  [5] = "#E5CF99", /* magenta */
  [6] = "#ECD7A0", /* cyan    */
  [7] = "#f6edd1", /* white   */

  /* 8 bright colors */
  [8]  = "#aca592",  /* black   */
  [9]  = "#817D9E",  /* red     */
  [10] = "#57A7B8", /* green   */
  [11] = "#52A8D4", /* yellow  */
  [12] = "#63ABD9", /* blue    */
  [13] = "#E5CF99", /* magenta */
  [14] = "#ECD7A0", /* cyan    */
  [15] = "#f6edd1", /* white   */

  /* special colors */
  [256] = "#223322", /* background */
  [257] = "#f6edd1", /* foreground */
  [258] = "#f6edd1",     /* cursor */
};

/* Default colors (colorname index)
 * foreground, background, cursor */
 unsigned int defaultbg = 0;
 unsigned int defaultfg = 257;
 unsigned int defaultcs = 258;
 unsigned int defaultrcs= 258;
