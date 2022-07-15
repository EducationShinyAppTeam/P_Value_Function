
library(exactci)
library(ggplot2)
# generate sample data----
getsample<-function(selection,n,popsd){
  if(selection=='Binomial'){populationdata<-sample(c(0,1),1000,prob=c(0.5,0.5),replace = TRUE)}
  if(selection=='Poisson'){populationdata<-rpois(1000,25)}
  if(selection=='Normal'){populationdata<- rnorm(n=1000,mean = 90, sd = popsd)}
  if(selection=='Uniform'){populationdata<- runif(n=1000, min=0, max=120)}
  sampledata<-sample(populationdata,n)
  return(sampledata)
}
# generate binwidth----
bw<-function(samplingdata){ifelse(IQR(samplingdata) == 0, 0.1, 2 * IQR(samplingdata) / (length(samplingdata)^(1/3)))}

# generate sampling distribution----
getsampling<-function(selection,theta,n,popsd){
  
  ## single proportion----
  if(selection=='Binomial'){
    samplingdata<- rnorm(30000,mean=theta,sd=sqrt(theta*(1-theta)/n))
    ggplot(
      data = as.data.frame(samplingdata),
      mapping = aes(x = samplingdata)
    )+
      geom_histogram(
        aes(y=..density..),
        col = boastUtils::boastPalette[4],
        binwidth = bw(samplingdata)
        
      )+
      stat_function(
        fun = dnorm,
        args = list(mean=theta, sd = sqrt(theta*(1-theta)/n)),
        size = 1
      )+
      geom_vline(
        mapping = aes(xintercept = theta, color = "value"), 
        size = 1
        )+
      labs(
        x = "Proportions",
        y = "Density",
        title = "Sampling Distribution of Proportion",
        alt = "A histogram with a smooth curve showing the sampling distribution of proportion"
      )+
      scale_color_manual(
        name = NULL,
        labels=c(
          "value"="Null value"
        ),
        values = c(
          "black")
      )+
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    
  }
  ## single mean----
  else{
    samplingdata<- rnorm(30000,mean=theta,sd=(popsd/sqrt(n)))
    ggplot(
      data = as.data.frame(samplingdata),
      mapping = aes(x = samplingdata)
    )+
      geom_histogram(
        aes(y=..density..),
        col = boastUtils::boastPalette[4],
        binwidth = bw(samplingdata)
        
      )+
      stat_function(
        fun = dnorm,
        args = list(mean=theta, sd = (popsd/sqrt(n))),
        size = 1
      )+
      geom_vline(
        mapping = aes(xintercept = theta, color = "value") , 
        size = 1
        )+
      labs(
        x = "Means",
        y = "Density",
        title = "Sampling Distribution of Mean",
        alt = paste("A histogram with a smooth curve showing the sampling distribution of mean")
      )+
      scale_color_manual(
        name = NULL,
        labels=c(
          "value"="Null value"
        ),
        values = c(
          "black")
      )+
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    
  }
}