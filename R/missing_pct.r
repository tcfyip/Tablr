#' output missing and percentage in a nice format
#'
#' This function gives a table of listing parameters' missing and pct
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
#'
#' @param missing.pct.pars a \code{character} vector which stores all parameter name that need missing pct calculation.
#' @param dp a \code{numeric} variable, number of decimal places to display.
#' @param p1 a \code{string} variable, the separator appeared between missing and pct. i.e. \code{<missing><p1><pct><p2>}
#' @param p2 a \code{string} variable, the separator appeared after pct. i.e. \code{<missing><p1><pct><p2>}
#'
#' @examples \dontrun{
#' # d<-dat
#' # p<-get.stat.par(par,par$Parameters))
#' # missing.pct.p<-as.vector(p[["missing.pct."]])
#' # missing.pct.dp<-2
#' # missing.pct.p1<-"("
#' # missing.pct.p2<-")"
#' # missing_pct(d,missing.pct.p,missing.pct.dp,missing.pct.p1,missing.pct.p2)}
#' @export
missing_pct<-function(dat,missing.pct.pars,dp,p1,p2){
  options(pillar.sigfig=dp+2)
  # dat := nested tibble
  # missing.pct.pars := vector of parameter name needs mean,sd
  cond<-dat%>%map(class)%>%lapply(., `%in%`,"list")%>%map(isTRUE)%>%map(isFALSE)
  map2(dat,cond,~{if(.y)  {
    summary<-.x%>%
      select(!!missing.pct.pars)%>%
      mutate_if(is.character,as.factor)%>%
      mutate_if(is.factor,as.numeric)%>%
      # sapply(.,function(x) ifelse(is.na(x),1,0))%>%
      map_dfr(.,is.na)%>%
      mutate_if(is.logical,as.numeric)%>%
      summarise(across(.cols=everything(),.fns=list(Missing=sum,Missing.prop=mean),.names="{col}_!@#$%{fn}"))

    summary.par.name<-summary%>%names%>%substr(.,1,regexpr("\\_\\!\\@\\#\\$\\%",.)-1)%>%unique

    summary%>%
      t%>%
      unlist%>%
      matrix(nc=2,byrow=TRUE)%>%
      missing_pct_format(.,dp,p1,p2)%>%
      data.frame(summary.par.name,paste("Missing",p1,"Pct",p2,sep=""),.)%>%
      set_names(c("Parameters","Statistics","Value"))


  } else missing_pct(.x,missing.pct.pars,dp,p1,p2)})
}

missing_pct_format<-function(m,dp,p1,p2){
  m[,2]<-m[,2]*100
  m<-m%>%round(.,dp)
  data.frame(paste0(format(m[,1],big.mark=",",trim=TRUE),p1,format(m[,2],big.mark=",",trim=TRUE),p2))
}
