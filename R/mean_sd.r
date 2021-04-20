#' output mean and standard deviation in a nice format
#'
#' This function gives a table of listing parameters' mean and sd
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
#' @param mean.sd.pars a \code{character} vector which stores all parameter name that need mean sd calculation.
#' @param dp a \code{numeric} variable, number of decimal places to display.
#' @param p1 a \code{string} variable, the separator appeared between mean and sd. i.e. \code{<mean><p1><sd><p2>}
#' @param p2 a \code{string} variable, the separator appeared after sd. i.e. \code{<mean><p1><sd><p2>}
#'
#' @examples \dontrun{
#' # d<-dat
#' # p<-get.stat.par(par,par$Parameters))
#' # mean.sd.p<-as.vector(p[["Mean.Sd."]])
#' # mean.sd.dp<-2
#' # mean.sd.p1<-"("
#' # mean.sd.p2<-")"
#' # mean_sd(d,mean.sd.p,mean.sd.dp,mean.sd.p1,mean.sd.p2)}
#'
mean_sd<-function(dat,mean.sd.pars,dp,p1,p2){
  options(pillar.sigfig=dp+2)
  # dat := nested tibble
  # mean.sd.pars := vector of parameter name needs mean,sd
  cond<-dat%>%map(class)%>%lapply(., `%in%`,"list")%>%map(isTRUE)%>%map(isFALSE)
  map2(dat,cond,~{if(.y)  {
    summary<-.x%>%
      select(!!mean.sd.pars)%>%
      mutate_if(is.character,as.factor)%>%
      mutate_if(is.factor,as.numeric)%>%
      summarise(across(.cols=where(is.numeric),.fns=list(Mean=mean,SD=sd),na.rm=TRUE,.names="{col}_!@#$%{fn}"))

    summary.par.name<-summary%>%names%>%substr(.,1,regexpr("\\_\\!\\@\\#\\$\\%",.)-1)%>%unique

    summary%>%
      mutate(across(.cols=where(is.numeric),round,dp))%>%
      t%>%
      unlist%>%
      matrix(nc=2,byrow=TRUE)%>%
      mean_sd_format(.,p1,p2)%>%
      data.frame(summary.par.name,paste("Mean",p1,"Sd",p2,sep=""),.)%>%
      set_names(c("Parameters","Statistics","Value"))

  } else mean_sd(.x,mean.sd.pars,dp,p1,p2)})
}

mean_sd_format<-function(m,p1,p2){
  data.frame(paste0(format(m[,1],big.mark=",",trim=TRUE),p1,format(m[,2],big.mark=",",trim=TRUE),p2))
}
