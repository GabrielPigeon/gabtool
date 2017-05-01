#' MY ggplot style
#'
#' @param base_size base size for graph
#'
#' @return
#' @export
#'
#' @examples
theme_gab <- function(base_size=12,base_family = "Times"){
  theme_bw(base_size,base_family)+
    theme(panel.border = element_rect(linetype = "solid", colour = "black",size=1, fill=NA),
          plot.margin= unit(c(0.1,0.1,0.1,0.1), "lines"),
          axis.text	=element_text(colour = "black")
          
          )
}
