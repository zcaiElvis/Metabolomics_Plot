
#' Title
#'
#' @param analytes
#' @param X_sign
#' @param rep_num
#' @param analytes_baseline
#' @param mM_baseline
#' @param plt_x_axis
#'
#' @return
#' @export
#'
#' @examples
media_comp_plot <- function(data, analytes = c("GLC3B", "GLN2B", "GLU2B", "LAC2B"),
                    X_sign = c(TRUE, TRUE, FALSE, FALSE),
                    rep_num = 2,
                    analytes_baseline = c(1,1,1,1),
                    mM_baseline = c(1,1),
                    plt_x_axis = c("10mM_VC", "10mM_0.5", "10mM_1", "10mM_2.5",
                                   "1mM_VC", "1mM_0.5", "1mM_1", "1mM_2.5")) {

  for(analyte in analytes) {
    d <- data[data$CEDEX.BIO.HT == analyte,]
    result <- format_process(d)
    d <- result$d
    d_top <- result$d_top
    d <- value_process(d, rep_num, analyte, analytes, X_sign, d_top)

    # Change order
    d$mM_val <- factor(d$mM_val, levels = plt_x_axis)

    plt <- ggplot2::ggplot(data = d, ggplot2::aes(x = mM_val, y = X_mean))+
      ggplot2::geom_bar(ggplot2::aes(x= mM_val, y = X_mean, fill = mM_val),
                        stat = "identity") +
      ggplot2::geom_point(ggplot2::aes(x = mM_val, y = X), shape = 1)+
      ggplot2::theme_bw()+
      ggplot2::labs(x = "Exp", y = "Value", title = analyte)

    print(plt)
  }
}



#' Format process
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
format_process <- function(d) {
  # Split data
  d_top <- d[1,] ### This need to be changed
  d <- d[2:nrow(d),]
  # Add column
  m_col <- stringr::str_split(d$X.ARC.FILE., " ")

  d$mM_val <- sapply(m_col, function(x) paste0(x[2], "_", x[3]))
  d$mM <- sapply(m_col, function(x) paste0(x[2]))
  return(list(d = d, d_top = d_top))
}

#' values_process
#'
#' @param d
#' @param rep_num
#' @param analyte
#' @param analytes
#' @param X_sign
#' @param d_top
#'
#' @return
#' @export
#'
#' @examples
value_process <- function(d, rep_num, analyte, analytes, X_sign, d_top) {
  val <- d$X
  val <- as.numeric(val)
  val <- zoo::na.fill(val, 0)
  top_val <- suppressWarnings(as.numeric(d_top$X))
  d$X <- top_val - val
  if (!X_sign[which(analytes == analyte)]) d$X = -d$X

  d <- d %>%
    dplyr::group_by(mM_val) %>%
    dplyr::mutate(X_mean = mean(X)/rep_num) %>%
    dplyr::ungroup()
  return(d)
}


# Some bars have only 1 dot, this is because the first element is
# wrongly treated as the baseline to be taken difference out of. Parameters
# that allow manual baseline input are set up and need to be implemented next

