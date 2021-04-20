# Tablr
R package for Table1 

<h2>This Tablr Vignette will introduce all function in the Tablr package.</h2> 

<h3><b>Required library:</b></h3>
1. devtools - for installing Tablr
2. survival - sample dataset mgus
3. dplyr - piping

<h3><b>Optional library:</b></h3>
1. haven - read sav
2. data.table - fast read spss

<span style="color:green">Step 1 - install Tablr package </span>:


```{r eval=FALSE}
install.packages("devtools")
install.packages("survival")
library(devtools)
library(survival)
library(dplyr)
install_github("huiwk/Tablr")
library(Tablr)
set.seed(63467)
```

<span style="color:green">Step 2 - Preparation </span>:



```{r eval=FALSE}
#Modify dataset, create random missing entries
dat<-survival::mgus[,c("age","sex","alb","creat","hgb")]
N<-80
inds <- as.matrix(expand.grid(1:nrow(dat), 1:ncol(dat)))
inds <- matrix(inds[!is.na(dat[inds])], ncol=2)
selected <- inds[sample(nrow(inds), N), ]
dat[selected] <- NA

#Create Par dataset [Mean(Sd)	Median[IQR]	Count(Pct)	Missing(Pct)	Order	Parameter.name.to.display	Parameters]
par<-data.frame(
  Mean.Sd.=c(1,0,1,1,1)%>%as.logical,
  Median.IQR.=c(1,0,1,1,1)%>%as.logical,
  Count.Pct.=c(0,1,0,0,0)%>%as.logical,
  Missing.Pct.=c(1,1,1,1,1)%>%as.logical,
  Order=c(4,2,3,1,5)%>%as.integer,
  Parameter.name.to.display=c("Age","Sex","Alb","Cr","Hgb"),
  Parameters=dat%>%names
)
All_group<-TRUE
By_group<-TRUE
dat<-data.frame(dat,Study_Design=sample(0:2,dim(dat)[1],replace=TRUE))%>%tibble #Create grouping variable
group_var<-"Study_Design"
```

<span style="color:green">Step 3 - Function Test </span>:

<h3>1. data.split.r </h3>

```{r eval=FALSE}
# data.split(dat,par,All_group,By_group,"Study_Design",TRUE)
data.split(dat,par$Parameters,All_group,By_group,group_var,TRUE)->d
```


<h3>2. get.stat.par.r </h3>

```{r eval=FALSE}
get.stat.par(par,par$Parameters)->p
```


<h3>3. mean_sd.r </h3>

```{r eval=FALSE}
mean_sd(d,as.vector(p[["Mean.Sd."]]),2,"(",")")
mean_sd(d,as.vector(p[["Mean.Sd."]]),3,"±","")
```


<h3>4. median_iqr.r </h3>

```{r eval=FALSE}
median_iqr(d,as.vector(p[["Median.IQR."]]),2,"[",",","]")
median_iqr(d,as.vector(p[["Median.IQR."]]),3,"[",",","]")
```



<h3>5. count_pct.r </h3>

```{r eval=FALSE}
count_pct(d,as.vector(p[["Count.Pct."]]),2,"(","%)")
```



<h3>6. missing_pct.r </h3>

```{r eval=FALSE}
missing_pct(d,as.vector(p[["Missing.Pct."]]),2,",","%")
```


<h3>7. Table1.r </h3>

```{r eval=FALSE}
Table1(dat,par,All_group,By_group,group_var,
       par$Parameters,
       mean.sd.dp = 2, mean.sd.p1 = "±", mean.sd.p2 = "",
       median.iqr.dp = 2, median.iqr.p1 = "[", median.iqr.p2 = ",", median.iqr.p3 = "]",
       count.pct.dp = 2, count.pct.p1 = "(", count.pct.p2 = ")",
       missing.pct.dp = 2, missing.pct.p1 = ",", missing.pct.p2 = "%")
```
























