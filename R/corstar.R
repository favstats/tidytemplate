#' A correlation table with stars
#'
#' This function allows you to create a correlation table with significance tables.
#' @param x dataframe
#' @keywords correlation table
#' @export
#' @examples
#' corstar(cars)

corstar <- function(x, y = NULL, use = "pairwise", method = "pearson", round = 3, row.labels, col.labels, ...) {
  
  ct <- corr.test(x, y, use, method)    # calculate correlation
  r <- ct$r                             # get correlation coefs
  p <- ct$p                             # get p-values
  
  stars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))) # generate significance stars
  
  m <- matrix(NA, nrow = nrow(r) * 2, ncol = ncol(r) + 1) # create empty matrix
  
  rlab <- if(missing(row.labels)) rownames(r) else row.labels # add row labels
  clab <- if(missing(col.labels)) {
    if(is.null(colnames(r)))
      deparse(substitute(y))
    else
      colnames(r)
  } else {
    col.labels # add column labels
  }
  
  rows <- 1:nrow(m)                     # row indices
  cols <- 2:ncol(m)                     # column indices
  
  odd <- rows %% 2 == 1                 # odd rows
  even <- rows %% 2 == 0                # even rows
  m[odd, 1] <- rlab                     # add variable names
  m[even, 1] <- rep("", sum(even))      # add blank
  
  m[odd, cols] <- paste(format(round(r, round), nsmall = round, ...), stars, sep = "")     # add r coefs
  m[even, cols] <- paste("(", format(round(p, round), nsmall = round, ...), ")", sep = "") # add p-values
  
  colnames(m) <- c(" ", clab)           # add colnames
  m                                     # return matrix
}