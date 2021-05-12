#' Title
#'
#' @param n distance of grid lines number between 0 and 1
#' @param ... geom_path parameters such as colour, alpha, size, etc.
#'
#' @return
#' @export
#'
#' @examples cowplot::ggdraw()+
#' cowplot::draw_plot(plot)+
#' rel_grid(0.01,color="grey")+
#' rel_grid(0.1)
rel_grid <- function(n=0.1,...) {
  seq01 <- seq(0,1,n)
  seqNA_ls <- lapply(seq01,c,NA)
  seqNA <- do.call(c,seqNA_ls)
  length(seqNA)
  length(seq01)
  seq1 <- rep(seqNA,each=length(seqNA))
  seq2 <- rep(c(seq01,seq01),length(seqNA))

  return(
    cowplot::draw_line(x=c(seq2,seq1),y=c(seq1,seq2),...)
  )
}
