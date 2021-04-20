#' output median and iqr in a nice format
#'
#' This function gives a table of listing parameters' median and iqr
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
#' @param median.iqr.pars a \code{character} vector which stores all parameter name that need median iqr calculation.
#' @param dp a \code{numeric} variable, number of decimal places to display.
#' @param p1 a \code{string} variable, the separator appeared between median and Q1. i.e. \code{<median><p1><iqr><p2>}
#' @param p2 a \code{string} variable, the separator appeared between Q1 and Q3. i.e. \code{<median><p1><Q1><p2><Q3><p3>}
#' @param p3 a \code{string} variable, the separator appeared after Q3. i.e. \code{<median><p1><Q1><p2><Q3><p3>}
#' @examples \dontrun{
#' # d<-dat
#' # p<-get.stat.par(par,par$Parameters))
#' # median.iqr.p<-as.vector(p[["median.iqr."]])
#' # median.iqr.dp<-2
#' # median.iqr.p1<-"("
#' # median.iqr.p2<-","
#' #' # median.iqr.p3<-")"
#' # median_iqr(d,median.iqr.p,median.iqr.dp,median.iqr.p1,median.iqr.p2,median.iqr.p3)}
#' @export
median_iqr<-function(dat,mean.sd.pars,dp,p1,p2,p3){
  options(pillar.sigfig=dp+2)
  # dat := nested tibble
  # mean.sd.pars := vector of parameter name needs mean,sd
  cond<-dat%>%map(class)%>%lapply(., `%in%`,"list")%>%map(isTRUE)%>%map(isFALSE)
  map2(dat,cond,~{if(.y)  {
    summary<-.x%>%
      select(!!mean.sd.pars)%>%
      mutate_if(is.character,as.factor)%>%
      mutate_if(is.factor,as.numeric)%>%
      summarise(across(.cols=where(is.numeric),
                       .fns=list(median=median, Q1=~quantile(., probs = 0.25,na.rm=TRUE,names=FALSE),Q3=~quantile(., probs = 0.75,na.rm=TRUE,names=FALSE)),
                       na.rm=TRUE,
                       .names="{col}_!@#$%{fn}"))

    summary.par.name<-summary%>%names%>%substr(.,1,regexpr("\\_\\!\\@\\#\\$\\%",.)-1)%>%unique

    summary%>%
      mutate(across(.cols=where(is.numeric),round,dp))%>%
      t%>%
      unlist%>%
      matrix(nc=3,byrow=TRUE)%>%
      median_iqr_format(.,p1,p2,p3)%>%
      data.frame(summary.par.name,paste("Median",p1,"Q1",p2,"Q3",p3,sep=""),.)%>%
      set_names(c("Parameters","Statistics","Value"))

  } else median_iqr(.x,mean.sd.pars,dp,p1,p2,p3)})
}

median_iqr_format<-function(m,p1,p2,p3){
  data.frame(paste0(format(m[,1],big.mark=",",trim=TRUE),p1,format(m[,2],big.mark=",",trim=TRUE),p2,format(m[,3],big.mark=",",trim=TRUE),p3))
}
