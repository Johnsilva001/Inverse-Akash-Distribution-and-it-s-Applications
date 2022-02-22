#code for the probability density function plot for Inverse Akash Distribution

pdf_plot = function () 
{
  y<- 3.5
  x<- seq(0,1,1/1000)
  n<- length(x)
  my.akash<- rep(0,n)
  for (i in (i:n)){
    my.akash[i]<- (y^3/((y^2)+2))*((1+(x[i])^2)/(x[i])^4)*exp(-(y/x[i]))
  }
  plot(my.akash,ylab="f(x)",col=2,xlab="x",type="l",lty=1,lwd=1)
  legend(x=50,y=0.18,legend=c(expression(theta=="3.5"),expression(theta=="4.0"),expression(theta=="4.5"),expression(theta=="5.0"),expression(theta=="5.5"),expression(theta=="6.0")),col=c(2,4,1,3,6,6),lty=c(1,2,3,4,5,6),lwd=c(1,1,1,1,1,1),bty="n")
  my.akash
}

pdf_plot()

#The above will just give a single line plot, so we duplicate the above code
#We remove the argument (x) inside the code and place it outside
#We also remove the plot and legend function from inside the code
#Note that we have to duplicate as many as the lines in the plot

x<- seq(0,1,1/1000)

pdf_plot1 = function (x) 
{
  y<- 4
  n<- length(x)
  my.akash<- rep(0,n)
  for (i in (i:n)){
    my.akash[i]<- (y^3/((y^2)+2))*((1+(x[i])^2)/(x[i])^4)*exp(-(y/x[i]))
  }
   my.akash
}

lines(pdf_plot1(x),col=4,lwd=1,lty=2)

pdf_plot2 = function (x) 
{
  y<- 4.5
  n<- length(x)
  my.akash<- rep(0,n)
  for (i in (i:n)){
    my.akash[i]<- (y^3/((y^2)+2))*((1+(x[i])^2)/(x[i])^4)*exp(-(y/x[i]))
  }
  my.akash
}

lines(pdf_plot2(x),col=1,lwd=1,lty=3)

pdf_plot3 = function (x) 
{
  y<- 5.0
  n<- length(x)
  my.akash<- rep(0,n)
  for (i in (i:n)){
    my.akash[i]<- (y^3/((y^2)+2))*((1+(x[i])^2)/(x[i])^4)*exp(-(y/x[i]))
  }
  my.akash
}

lines(pdf_plot3(x),col=3,lwd=1,lty=4)

pdf_plot4 = function (x) 
{
  y<- 5.5
  n<- length(x)
  my.akash<- rep(0,n)
  for (i in (i:n)){
    my.akash[i]<- (y^3/((y^2)+2))*((1+(x[i])^2)/(x[i])^4)*exp(-(y/x[i]))
  }
  my.akash
}

lines(pdf_plot4(x),col=6,lwd=1,lty=5)

pdf_plot5 = function (x) 
{
  y<- 6
  n<- length(x)
  my.akash<- rep(0,n)
  for (i in (i:n)){
    my.akash[i]<- (y^3/((y^2)+2))*((1+(x[i])^2)/(x[i])^4)*exp(-(y/x[i]))
  }
  my.akash
}

lines(pdf_plot5(x),col=6,lwd=1,lty=6)


#code for the Hazard rate function plot

hrf_plot = function () 
{
  y<- .7
  x<- seq(0,1,1/1000)
  n<- length(x)
  my.akash<- rep(0,n)
  F.akash<- rep(0,n)
  for (i in (i:n)){
    my.akash[i]<- (y^3/((y^2)+2))*((1+(x[i])^2)/(x[i])^4)*exp(-(y/x[i]))
    F.akash[i]<- (1+((y+(2*(x[i])))/((y^2)+2))*(y/(x[i])^2))*exp(-(y/x[i]))
  }
  hh<- my.akash/(1-F.akash)
  plot(hh,ylab="h(x)",col=2,xlab="x",type="l",lty=1,lwd=1)
  legend(x=600,y=5.8,legend=c(expression(theta=="0.7"),expression(theta=="0.8"),expression(theta=="1.0"),expression(theta=="1.8")),col=c(2,4,6,3),lty=c(1,2,3,4),lwd=c(2,2,1,1),bty="n")
  hh
}

hrf_plot()

#The above will just give a single line plot, so we duplicate the above code
#We remove the argument (x) inside the code and place it outside
#We also remove the plot and legend function from inside the code
#Note that we have to duplicate as many as the lines in the plot

x<- seq(0,1,1/1000)

hrf_plot1 = function (x) 
{
  y<- 0.8
  n<- length(x)
  my.akash<- rep(0,n)
  F.akash<- rep(0,n)
  for (i in (i:n)){
    my.akash[i]<- (y^3/((y^2)+2))*((1+(x[i])^2)/(x[i])^4)*exp(-(y/x[i]))
    F.akash[i]<- (1+((y+(2*(x[i])))/((y^2)+2))*(y/(x[i])^2))*exp(-(y/x[i]))
  }
  hh<- my.akash/(1-F.akash)
   hh
}

lines(hrf_plot1(x),col=4,lwd=2,lty=2)

hrf_plot2 = function (x) 
{
  y<- 1.0
  n<- length(x)
  my.akash<- rep(0,n)
  F.akash<- rep(0,n)
  for (i in (i:n)){
    my.akash[i]<- (y^3/((y^2)+2))*((1+(x[i])^2)/(x[i])^4)*exp(-(y/x[i]))
    F.akash[i]<- (1+((y+(2*(x[i])))/((y^2)+2))*(y/(x[i])^2))*exp(-(y/x[i]))
  }
  hh<- my.akash/(1-F.akash)
  hh
}

lines(hrf_plot2(x),col=6,lwd=1,lty=3)

hrf_plot3 = function (x) 
{
  y<- 1.8
  n<- length(x)
  my.akash<- rep(0,n)
  F.akash<- rep(0,n)
  for (i in (i:n)){
    my.akash[i]<- (y^3/((y^2)+2))*((1+(x[i])^2)/(x[i])^4)*exp(-(y/x[i]))
    F.akash[i]<- (1+((y+(2*(x[i])))/((y^2)+2))*(y/(x[i])^2))*exp(-(y/x[i]))
  }
  hh<- my.akash/(1-F.akash)
  hh
}

lines(hrf_plot3(x),col=3,lwd=1,lty=4)


#Code for the optimization of the model parameter 
#Install package
#Load librabry

install.packages("AdequacyModel")
library(AdequacyModel)

#Import the first Data set (Data X)

x1 = c(6.53,7,10.42,14.48,16.10,22.70,34,41.55,42,45.28,49.40,53.62,63,64,83,84,91,108,
      112,129,133,133,139,140,140,146,149,154,157,160,160,165,146,149,154,157,160,
      160,165,173,176,218,225,241,248,273,277,297,405,417,420,440,523,583,594,110
      1,1146,1417)

#Import the second Data set (Data Y)

x2 = c(12.20,23.56,23.74,25.87,31.98,37,41.35,47.38,55.46,58.36,63.47,68.46,78.26,74.4
       7,81.43,84,92,94,110,112,119,127,130,133,140,146,155,159,173,179,194,195,209, 
       249,281,319,339,432,469,519,633,725,817,1776 )

#Some Notations
#IAD = Inverse Akash Distribution
#IED = Inverse Exponential Distribution
#ILD = Inverse Lindley Distribution
#AIC = Akaike information criterion
#BIC = Bayesian information criterion
#-L = Negative Log-Likelihood Function
#A* = Anderson Darling Test
#W* = Cramer-Von Mises Criterion
#KS = Kologorov-Smirnov Statistics
#SE = Standard Error Estimate of the Parameter



#Model Optimization for IAD using the first Data set (Data X)
#We first code the pdf and cdf of IAD

pdf_iad = function (par,x) 
{
  y<- par[1]
  (y^3/(2+y^2))*((1+x^2)/x^4)*exp(-(y/x))
}

cdf_iad = function (par,x) 
{
  y<- par[1]
  (1+(((2*y*x)+y^2)/(((y*x)^2)+(2*x^2))))*exp(-(y/x))
}


optim_iad1 = goodness.fit(pdf=pdf_iad, cdf=cdf_iad, starts=0.5, data=x1, method = "BFGS", domain = c(6.53,1417),
             mle = NULL)
optim_iad1

#Model Optimization for IED using the first Data set (Data X)
#We first code the pdf and cdf of IED

pdf_ied = function (par,x) 
{
  y<- par[1]
  (y/x^2)*exp(-(y/x))
}

cdf_ied = function (par,x) 
{
  y<- par[1]
  exp(-(y/x))
}


optim_ied1 = goodness.fit(pdf=pdf_ied, cdf=cdf_ied, starts=0.5, data=x1, method = "BFGS", domain = c(6.53,1417),
                         mle = NULL)
optim_ied1

#Model Optimization for ILD using the first Data set (Data X)
#We first code the pdf and cdf of ILD

pdf_ild = function (par,x) 
{
  y<- par[1]
  (y^2/(y+1))*((1+x)/x^3)*exp(-(y/x))
}

cdf_ild = function (par,x) 
{
  y<- par[1]
  (1+(y/(x+(y*x))))*exp(-(y/x))
}


optim_ild1 = goodness.fit(pdf=pdf_ild, cdf=cdf_ild, starts=0.5, data=x1, method = "BFGS", domain = c(6.53,1417),
                         mle = NULL)
optim_ild1



#Model Optimization for IAD using the second Data set (Data Y)

optim_iad2 = goodness.fit(pdf=pdf_iad, cdf=cdf_iad, starts=0.5, data=x2, method = "BFGS", domain = c(12.20,1776),
                         mle = NULL)
optim_iad2

#Model Optimization for IED using the second Data set (Data Y)

optim_ied2 = goodness.fit(pdf=pdf_ied, cdf=cdf_ied, starts=0.5, data=x2, method = "BFGS", domain = c(12.20,1776),
                          mle = NULL)
optim_ied2

#Model Optimization for ILD using the second Data set (Data Y)

optim_ild2 = goodness.fit(pdf=pdf_ild, cdf=cdf_ild, starts=0.5, data=x2, method = "BFGS", domain = c(12.20,1776),
                          mle = NULL)
optim_ild2
