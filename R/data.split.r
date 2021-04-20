#' Subset your data
#'
#' This function would extract useful information only. The outputted data contains
#' the interested parameter values and group variable (optional) only. It does not
#' remove any row records but drop useless columns.
#'
#'
#' @param dat your cohort data file, of \code{data.frame} structure. For more details please refer the iris dataset.
#' \tabular{rrrrl}{
#'   \strong{Sepal.Length} \tab \strong{Sepal.Width} \tab \strong{Petal.Length} \tab \strong{Petal.Width} \tab \strong{Species} \cr
#'   5.1 \tab 3.5 \tab 1.4 \tab 0.2 \tab setosa\cr
#'   4.9 \tab 3.0 \tab 1.4 \tab 0.2 \tab setosa\cr
#'   4.7 \tab 3.2 \tab 1.3 \tab 0.2 \tab setosa\cr
#'   4.6 \tab 3.1 \tab 1.5 \tab 0.2 \tab setosa\cr
#'   5.0 \tab 3.6 \tab 1.4 \tab 0.2 \tab setosa\cr
#'   5.4 \tab 3.9 \tab 1.7 \tab 0.4 \tab setosa
#' }
#' @param pars  A \code{data.frame} object that stores your table 1 attributes.
#' \tabular{llllrll}{
#'   \strong{Mean.Sd.} \tab \strong{Median.IQR.} \tab \strong{Count.Pct.} \tab \strong{Missing.Pct.} \tab \strong{Order} \tab \strong{Parameter.name.to.display} \tab \strong{Parameters} \cr
#'    TRUE \tab FALSE \tab  TRUE \tab FALSE \tab 5 \tab Sex            \tab Sex                       \cr
#'    TRUE \tab  TRUE \tab FALSE \tab FALSE \tab 2 \tab Age            \tab Age                       \cr
#'    TRUE \tab  TRUE \tab FALSE \tab  TRUE \tab 3 \tab ALT            \tab Alt_LISResultNumericResult\cr
#'    TRUE \tab  TRUE \tab FALSE \tab  TRUE \tab 4 \tab AST            \tab Ast_LISResultNumericResult\cr
#'    TRUE \tab  TRUE \tab FALSE \tab  TRUE \tab 1 \tab FIB 4          \tab FIB_4                     \cr
#'   FALSE \tab FALSE \tab  TRUE \tab  TRUE \tab 6 \tab HBeAg positive \tab HBeAg_Test_result_numeric
#' }
#' @param All_group a \code{logical} variable; \code{TRUE} = need whole cohort information; \code{FALSE} = no need whole cohort information.
#' @param By_group a \code{logical} variable; \code{TRUE} = compare information by a factor variable; \code{FALSE} = do not compare information by a factor variable.
#' @param group_var \code{optional} string; contains your grouping variable name. \code{group_var} should contain one element only.
#' @param unnest a \code{logical} variable; \code{TRUE}=output a clean dataset of \code{nested tibble} form; \code{TRUE}=output a \code{list} with nested list (if \code{By_group==TRUE}).
#' @examples \dontrun{
#' # dat<-read_sav(file.choose())
#' # par<-read.csv(file.choose())
#' # All_group<-TRUE
#' # By_group<-TRUE
#' # group_var<-"Study_Design"
#' # group_var_disname<-"Study Design"
#' data.split(dat,par,All_group,By_group,"Study_Design",TRUE)}
#' @export
data.split<-function(dat,pars,All_group,By_group,group_var=NULL,unnest=FALSE){
  # dat:= data frame
  # pars := parameters in vector
  # All_group := boolean or integer or float
  # By_group := boolean or integer or float
  # group_var := character or NULL
  All_group_dat<-{if(All_group) dat%>%select(!!pars)%>%nest(data=everything())}
  By_group_dat<-{if(By_group) dat%>%select(!!pars)%>%bind_cols(.,dat[,group_var])%>%group_nest(dat[,group_var])}
  d<-switch(1+All_group+By_group*2,
            NULL,
            list("All_group_dat"=All_group_dat),
            list("By_group_dat"=By_group_dat),
            list("All_group_dat"=All_group_dat,"By_group_dat"=By_group_dat)
  )
  if(unnest){
    unnested.d<-map(d,.%>%unnest(everything()))
    is.by.group<-map(unnested.d%>%map(.,names),~{match(.,group_var)%>%sum(na.rm = TRUE)%>%max})
    map2(unnested.d,is.by.group,~{if(.y) group_split(.x,.x[,group_var])%>%map(tibble)%>%set_names(outer(group_var,d[[2]][,1]%>%unlist,paste)) else .x})
  }else return(d)
}
