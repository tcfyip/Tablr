#' classiflying summary statistics in lists
#'
#' This function gives a summary list of which parameters need which kind of summary statistics.
#'
#' @param par.df  A \code{data.frame} object that stores your table 1 attributes.
#' \tabular{llllrll}{
#'   \strong{Mean.Sd.} \tab \strong{Median.IQR.} \tab \strong{Count.Pct.} \tab \strong{Missing.Pct.} \tab \strong{Order} \tab \strong{Parameter.name.to.display} \tab \strong{Parameters} \cr
#'    TRUE \tab FALSE \tab  TRUE \tab FALSE \tab 5 \tab Sex            \tab Sex                       \cr
#'    TRUE \tab  TRUE \tab FALSE \tab FALSE \tab 2 \tab Age            \tab Age                       \cr
#'    TRUE \tab  TRUE \tab FALSE \tab  TRUE \tab 3 \tab ALT            \tab Alt_LISResultNumericResult\cr
#'    TRUE \tab  TRUE \tab FALSE \tab  TRUE \tab 4 \tab AST            \tab Ast_LISResultNumericResult\cr
#'    TRUE \tab  TRUE \tab FALSE \tab  TRUE \tab 1 \tab FIB 4          \tab FIB_4                     \cr
#'   FALSE \tab FALSE \tab  TRUE \tab  TRUE \tab 6 \tab HBeAg positive \tab HBeAg_Test_result_numeric
#' }
#' @param par.name a \code{character} vector which stores your parameter name in your cohort dataset.
#' @examples \dontrun{
#' # get.stat.par(par,par$Parameters)}
#' @export
get.stat.par<-function(par.df,par.name){
  # par.df := data frame
  # par.name := name of the column in par.df which stores parameter name
  spar<-par.df%>%map_int(is.logical)%>%{which(.!=0)}%>%names
  spar%>%map(~par.df[par.df[,.],"Parameters"])%>%set_names(spar)
}
