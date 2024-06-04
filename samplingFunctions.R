# Load Packages ----
library(exactci)
library(ggplot2)

# Generate sample data ----
getSample <- function(selection, n, popsd) {
  if (selection == 'Binomial') {
    populationData <- sample(
      x = c(0, 1),
      size = 1000,
      prob = c(0.5, 0.5),
      replace = TRUE
    )
  } else if (selection == 'Poisson') {
    populationData <- rpois(n = 1000, lambda = 25)
  } else if (selection == 'Normal') {
    populationData <- rnorm(n = 1000, mean = 90, sd = popsd)
  } else if (selection == 'Uniform') {
    populationData <- runif(n = 1000, min = 0, max = 120)
  }
  
  sampleData <- sample(x = populationData, size = n)
  return(sampleData)
}

# Generate Binwidth----
bw <- function(samplingData){
  width <- ifelse(
    test = IQR(samplingData) == 0,
    yes = 0.1,
    no = 2 * IQR(samplingData) / (length(samplingData)^(1/3))
  )
  return(width)
}

# Generate Sampling Distribution----
getSamplingDist <- function(selection, theta, n, popsd){
  
  ## Single proportion----
  if (selection == 'Binomial') {
    samplingData <- rnorm(n = 30000, mean = theta, sd = sqrt(theta*(1 - theta)/n))
    ggplot(
      data = as.data.frame(samplingData),
      mapping = aes(x = samplingData)
    ) +
      geom_histogram(
        mapping = aes(y = after_stat(density)),
        fill = boastUtils::boastPalette[4],
        color = boastUtils::boastPalette[7],
        binwidth = bw(samplingData)
      ) +
      stat_function(
        fun = dnorm,
        args = list(mean = theta, sd = sqrt(theta*(1 - theta)/n)),
        linewidth = 1
      ) +
      geom_vline(
        mapping = aes(xintercept = theta, color = "value"), 
        linewidth = 1
        ) +
      labs(
        x = "Proportions",
        y = "Density",
        title = "Sampling Distribution of Proportion",
        alt = "A histogram with a smooth curve showing the sampling distribution of proportion"
      ) +
      scale_color_manual(
        name = NULL,
        labels = c("value" = "Null value"),
        values = c("black")
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.025)))
  }
  ## single mean----
  else {
    samplingData <- rnorm(n = 30000, mean = theta, sd = (popsd/sqrt(n)))
    ggplot(
      data = as.data.frame(samplingData),
      mapping = aes(x = samplingData)
    ) +
      geom_histogram(
        mapping = aes(y = after_stat(density)),
        fill = boastUtils::boastPalette[4],
        color = boastUtils::boastPalette[7],
        binwidth = bw(samplingData)
      ) +
      stat_function(
        fun = dnorm,
        args = list(mean = theta, sd = (popsd/sqrt(n))),
        linewidth = 1
      ) +
      geom_vline(
        mapping = aes(xintercept = theta, color = "value") , 
        linewidth = 1
        ) +
      labs(
        x = "Means",
        y = "Density",
        title = "Sampling Distribution of Mean",
        alt = paste("A histogram with a smooth curve showing the sampling distribution of mean")
      ) +
      scale_color_manual(
        name = NULL,
        labels = c("value" = "Null value"),
        values = c("black")
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.025)))
  }
}