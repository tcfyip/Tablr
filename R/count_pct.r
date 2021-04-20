#' output count and percentage in a nice format
#'
#' This function gives a table of listing parameters' count and pct
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
#' @param count.pct.pars a \code{character} vector which stores all parameter name that need count pct calculation.
#' @param dp a \code{numeric} variable, number of decimal places to display.
#' @param p1 a \code{string} variable, the separator appeared between count and pct. i.e. \code{<count><p1><pct><p2>}
#' @param p2 a \code{string} variable, the separator appeared after pct. i.e. \code{<count><p1><pct><p2>}
#'
#' @examples \dontrun{
#' # d<-dat
#' # p<-get.stat.par(par,par$Parameters))
#' # count.pct.p<-as.vector(p[["count.pct."]])
#' # count.pct.dp<-2
#' # count.pct.p1<-"("
#' # count.pct.p2<-")"
#' # count_pct(d,count.pct.p,count.pct.dp,count.pct.p1,count.pct.p2)}
#'
count_pct<-function(dat,count.pct.pars,dp,p1,p2){

  options(pillar.sigfig=dp+2)
  # dat := nested tibble
  # count.pct.pars := vector of parameter name needs mean,sd
  cond<-dat%>%map(class)%>%lapply(., `%in%`,"list")%>%map(isTRUE)%>%map(isFALSE)
  map2(dat,cond,~{if(.y)  {
    dat.as.factor<-.x%>%
      select(c(!!count.pct.pars,-matches(!!group_var)))%>%
      mutate(across(.cols=everything(),as.factor))

    summary<-count_pct_diff.levels(dat.as.factor)

    summary%>%
      mutate(across(.cols=where(is.numeric),round,dp))%>%
      count_pct_format(.,p1,p2)%>%
      data.frame(row.names = rownames(summary))%>%
      set_names("Value")%>%
      rownames_to_column(var="Parameters")%>%
      separate(.,col="Parameters",into=c("Parameters","levels"),sep=":")%>%
      add_column(Statistics=paste("Count",p1,"Pct",p2,sep=""),.after="Parameters")

  } else count_pct(.x,count.pct.pars,dp,p1,p2)})
}

count_pct_diff.levels<-function(dat.as.factor,res_list=NULL){
  if(is.null(res_list)){res_list<-list()}

  cond2<-dat.as.factor%>%map(levels)%>%map_dbl(length)%>%match(.,.[which.max(.)])%>%ifelse(is.na(.),0,.)
  selected.dat<-dat.as.factor%>%
    select(eval(cond2*(1:length(cond2))))
  dat.as.factor.levels<-selected.dat%>%map(levels)%>%unlist
  max.level<-selected.dat%>%map(levels)%>%map_dbl(length)%>%max(na.rm=TRUE)

  summary<-selected.dat%>%
    summarise(across(.cols=everything(),.fns=list(count=table)))%>%
    mutate(across(.cols=everything(),.fns=list(pct=prop.table)))%>%
    as.matrix%>%
    matrix(nc=2)%>%
    as.matrix%>%
    data.frame(row.names = paste(
      rep(selected.dat%>%names,each=max.level),dat.as.factor.levels,sep=":"
    ))%>%set_names(c("count","percentage"))



  res_list[[res_list%>%length+1]]<-summary

  if(cond2%>%match(.,1)%>%is.na%>%any) count_pct_diff.levels(dat.as.factor%>%select(eval((!cond2)*(1:length(cond2)))),res_list)
  else if(!is.null(res_list)) return(res_list%>%do.call(bind_rows,.))
}


count_pct_format<-function(m,p1,p2){
  data.frame(paste0(format(m[,1],big.mark=",",trim=TRUE),p1,format(m[,2],big.mark=",",trim=TRUE),p2))
}
