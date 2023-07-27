# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(exactci)
library(ggplot2)
library(dplyr)

# Load additional functions ----
source("samplingFunctions.R")

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "purple",
    ## Header ----
    dashboardHeader(
      title = "P-Value Function", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "P-Value Function")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("P-Value Function and Hypothesis Testing"),
          p("This app can generate the p-value function, which displays 
            confidence limits and p-values of any confidence level of the
            parameter. The user can use this app to do hypothesis testing for
            one sample."
          ),
          h2("Instructions"),
          tags$ol(
            tags$li("Click the Go button to enter the Explore page."),
            tags$li("Enter the tab of hypothesis test which one you want to do, 
                    select these inputs, then click the simulate button to view 
                    the sample information."),
            tags$li("View p-value function plot, sampling distribution and test 
                    results."),
            tags$li("Adjust your inputs and click the re-simulate button again to 
                    start a new test.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Jing Fu.",
            br(),
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 07/27/2023 by NJH.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following information that will be used in the app."),
          box(
            title = strong("p-values"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "The probability value of the estimator we chose is at least as 
            extreme as the value we observed under the null hypothesis. 
            If we carry out our study infinite times and the null hypothesis 
            accurately simulates what we are studying, then we will expect our 
            estimator to produce values at least as extreme as what we have seen 
            100*(p-value)% of the time. The larger the p-value, the more often 
            we will hope that our estimator to take on a value at least as extreme 
            as we see; The smaller, the less often."
          ),
          box(
            title = strong("p-value function"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "The p-value function displays the confidence intervals and p-values 
            for any confidence level of the parameter. The p-value function can 
            display a large amount of information: point estimates of the parameter, 
            one-tail and two-tails confidence intervals at any level, and one-tail 
            and two-tails p-values of any null and non-null values."
          ),
          box(
            title = strong("Confidence Level"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "The confidence level, k%, represents the long-run proportion of
            corresponding confidence intervals that contain the true value of the
            parameter. If we were to repeat the entire experiment, then k% of the
            time we will make an interval that contains the true value of the
            population parameter."
          )
        ),
        ### Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore the p-value funtion"),
          p("On this page, you can explore p-value functions in four different 
            population distributions:"),
          tags$ul(
            tags$li(
              "Single Proportion",
              tags$ul(
                tags$li("\\(X\\sim Bin\\left(n=1000,\\; p=0.5\\right)\\)")
              )
            ),
            tags$li(
              "Single Mean",
              tags$ul(
                tags$li("\\(X\\sim Poi\\left(\\lambda=25\\right)\\)"),
                tags$li("\\(X\\sim N\\left(\\mu=90,\\; \\sigma\\right)\\), you will
                        be able to control the population standard deviation, 
                        \\(\\sigma\\)"),
                tags$li("\\(X\\sim Uni\\left(a=0,\\; b=120\\right)\\)")
              )
            )
          ),
          p("Select the hypothesis test type, select the population distribution, 
            and then change the inputs. Once you are ready, click simulate. 
            All p-values are the results of the two tailed hypothesis test."
          ),
          tabsetPanel(
            id = "whichType",
            type = "tabs",
            #### Single Proportion ----
            tabPanel(
              title = "Single Proportion",
              value = "pro",
              br(),
              fluidRow(
                ##### Input controls ----
                column(
                  width = 4,
                  offset = 0,
                  wellPanel(
                    # population dis
                    radioButtons(
                      inputId = "binomial", 
                      label = "Population distribution", 
                      choices = c("Binomial"),
                      selected = "Binomial", 
                      width = '100%'
                    ),
                    # confidence level
                    sliderInput(
                      inputId = "clofp",
                      label = "Confidence level, \\(1-\\alpha\\)",
                      min = 0.6,
                      max = 0.99,
                      step = 0.01,
                      value = 0.95
                    ),
                    # sample size
                    sliderInput(
                      inputId = "nofp",
                      label = "Sample size, n",
                      min = 2,
                      max = 90,
                      step = 1,
                      value = 50
                    ),
                    # null hypothesis
                    sliderInput(
                      inputId = "theta0ofp",
                      label = "Null hypothesis, \\(H_0\\)",
                      min = 0,
                      max = 1,
                      step = 0.01,
                      value = 0.55
                    ),
                    # simulate button
                    div(
                      style = "text-align: center;",
                      bsButton(
                        inputId = "simforp",
                        label = "Simulate!",
                        size = "large",
                        style = "default"
                      )
                    ),
                    br(),
                    uiOutput("sampledataPop")
                  )
                ),
                ##### Output Section----
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("pfunctionPop"),
                  br(),
                  checkboxInput(
                    inputId = "resultsPop",
                    label = "Results table",
                    value = FALSE
                  ),
                  conditionalPanel(
                    condition = "input.resultsPop==1",
                    tableOutput("pvaluePop")
                  ),
                  br(),
                  plotOutput("sampledistPop")
                )
              )
            ),
            #### Single Mean ----
            tabPanel(
              title = "Single Mean",
              value = "mean",
              br(),
              fluidRow(
                column(
                  width = 4,
                  offset = 0,
                  wellPanel(
                    ##### Input controls ----
                    # population dis
                    radioButtons(
                      inputId = "types", 
                      label = "Population distribution", 
                      choices = c("Poisson","Normal","Uniform"),
                      selected = "Normal", 
                      width = '100%'
                    ),
                    # confidence level
                    sliderInput(
                      inputId = "cl",
                      label = "Confidence level, \\(1-\\alpha\\)",
                      min = 0.6,
                      max = 0.99,
                      step = 0.01,
                      value = 0.95
                    ),
                    # sample size
                    sliderInput(
                      inputId = "n",
                      label = "Sample size, n",
                      min = 2,
                      max = 90,
                      step = 1,
                      value = 50
                    ),
                    # null hypothesis
                    sliderInput(
                      inputId = "theta0",
                      label = "Null hypothesis, \\(H_0\\)",
                      min = 0,
                      max = 120,
                      value = 90,
                      step = 1
                    ),
                    # Std. Deviation for Normal
                    conditionalPanel(
                      condition = "input.types == 'Normal'",
                      numericInput(
                        inputId = "norsd",
                        label = "Standard deviation, \\(\\sigma\\)",
                        min = 1,
                        max = 50, 
                        value = 10,
                        step = 1
                      )
                    ),
                    # simulate button
                    div(
                      style = "text-align: center;",
                      bsButton(
                        inputId = "sim",
                        label = "Simulate!",
                        size = "large",
                        style = "default"
                      )
                    ),
                    br(),
                    uiOutput("sampledataMean")
                  )
                ),
                ##### Output section ----
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("pfunctionMean"),
                  br(),
                  checkboxInput(
                    inputId = "resultsMean",
                    label = "Results table", 
                    value = FALSE
                  ),
                  conditionalPanel(
                    condition = "input.resultsMean==1",
                    tableOutput("pvalue")
                  ),
                  br(),
                  plotOutput("sampledist")
                )
              )
            )
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2023). boastUtils: BOAST utlities.
            (v 0.1.12.3). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Change, W., and Borges Ribeiro, B. (2021). shinydashboard: Create 
            dashboards with 'shiny'. (v 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J.J., Sievert, C., Schloerke, B.,
            Xie, Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2022).
            shiny: Web application framework for R. (v1.7.4). [R Package].
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Fay, M. P. (2010). Two-sided exact tests and matching confidence 
            intervals for discrete data. R Journal 2(1), 53-58. (v 1.4-2).
            [R package]. Available from https://CRAN.R-project.org/package=exactci"
          ),
          p(
            class = "hangingindent",
            "Infanger, D. and Schmidt-Trucksäss, A. (2019). P value functions: An
            underused method to present research results and to promote 
            quantitative reasoning. Statistics in Medicine. 38(21). 4189-4197.
            Available from https://doi.org/10.1002/sim.8293"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2023). shinyWidgets: Custom
            inputs widgets for shiny. (v0.7.6). [R Package]. Availble from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K. (2023). dplyr: A 
            Grammar of Data Manipulation. (v 1.1.2).[R package]. Available from 
            https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.4.2) [R package]. Available from
            https://ggplot2.tidyverse.org"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)



# Define server logic ----
server <- function(input, output, session) {
  
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This app can explore p-value function in different situations 
        or do a two-tailed hypothesis test for one single sample."
      )
    }
  )
  
  ## Go button ----
  observeEvent(
    eventExpr = input$go,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore"
      )
    }
  )
  
  ## Update mean null hypothesis slider ----
  observeEvent(
    eventExpr = input$types,
    handlerExpr = {
      defaultValue <- switch(
        EXPR = input$types,
        Poisson = 25,
        Normal = 90,
        Uniform = 60
      )
      maxValue <- ifelse(
        test = input$types == "Poisson",
        yes = 50,
        no = 120
      )
      
      updateSliderInput(
        session = session,
        inputId = "theta0",
        value = defaultValue,
        max = maxValue
      )
    },
    label = "here1"
  )
  
  ## Update simulate buttons ----
  
  ### Proportion ----
  observeEvent(
    eventExpr = input$simforp,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "simforp",
        label = " Re-simulate!",
        icon("retweet"),
        style = "default",
        disabled = FALSE 
      )
    }
  )
  ### Mean ----
  observeEvent(
    eventExpr = input$sim,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "sim",
        label = " Re-simulate!",
        icon = icon("retweet"),
        style = "default",
        disabled = FALSE
      )
    }
  )
  
  ## Update inputs values ----
  ## population selection
  selection <- eventReactive(
    eventExpr = c(input$sim, input$simforp),
    valueExpr = {
      switch(
        EXPR = input$whichType,
        mean = input$types,
        pro = input$binomial
      )
    }
  )
  
  
  ### update popsd of Mean
  popsd <- eventReactive(
    eventExpr = c(input$sim, input$simforp),
    valueExpr = {
      switch(
        EXPR = selection(),
        Binomial = 1,
        Poisson = 5, # original had sqrt(25)
        Normal = input$norsd,
        Uniform = sqrt(1200)
      )
    }
  )
  
  ## Get Sample Data ----
  sampledatap <- reactive(
    getSample(
      selection = selection(),
      n = input$nofp,
      popsd = popsd()
    )
  )
  successp <- reactive(sum(sampledatap()))
  phatp <- reactive(round(mean(sampledatap()), digits = 3))
  
  sampledata <- reactive(
    getSample(
      selection = selection(),
      n = input$n,
      popsd = popsd()
    )
  )
  meanhat <- reactive(round(mean(sampledata()), digits = 3))
  
  ### error message
  observeEvent(
    eventExpr = input$sim ,
    handlerExpr = {
      if (popsd() <= 0) {
        sendSweetAlert(
          session = session,
          type = "error",
          title = "Error: Please check your standard deviation",
          html = TRUE 
        )
      }
    }
  )
  
  
  ### update p-value
  getpvalue <- function(selection){
    if (selection == 'Binomial') {
      pValue <- binom.exact(
        x = successp(),
        n = input$nofp,
        p = input$theta0ofp,
        alternative = "two.side",
        tsmethod = "central"
      )$p.value
    } else if (selection == 'Poisson') {
      pValue <- poisson.exact(
        x = sum(sampledata()),
        T = input$n,
        r = input$theta0,
        alternative = "two.side",
        tsmethod = "central"
      )$p.value
    } else if (selection == 'Normal' || selection == 'Uniform') {
      z_score <- (meanhat() - input$theta0)/(popsd()/sqrt(input$n))
      pValue <- 2*pnorm(-abs(z_score))
    }
    return(round(pValue, digits = 3))
  }
  
  ### Show Sample Information ----
  observeEvent(
    eventExpr = input$simforp,
    handlerExpr = {
      output$sampledataPop <- renderUI({
        validate(
          need(
            expr = selection() == 'Binomial',
            message = ""
          )
        )
        withMathJax(
          h3("Sample Information"),
          p(
            "\\(n =\\) ", input$nofp, br(),
            "\\(x =\\) ", successp(), br(),
            "\\(\\widehat{p} =\\) ", phatp(), br(),
            "Assumptions: \\( n\\widehat{p} \\geq 5\\) and \\( n(1-\\widehat{p}) \\geq 5\\)",
            ifelse(
              test = input$nofp * phatp() >= 5 & input$nofp * (1 - phatp()) >= 5,
              yes = " are met.",
              no = " are not met."
            )
          )
        )
      })
    }
  )
  
  observeEvent(
    eventExpr = input$sim,
    handlerExpr = {
      output$sampledataMean <- renderUI({
        validate(
          need(
            expr = selection() != 'Binomial',
            message = ""
          )
        )
        validate(
          need(
            expr = input$norsd > 0 || selection() == "Uniform" ||
              selection() == "Poisson",
            message = ""
          ) 
        )
        withMathJax(
          h3("Sample Information"),
          p(
            "\\(n =\\) ", input$n, br(),
            "\\(\\bar{x} =\\) ", meanhat(), br(),
            "\\(\\sigma =\\) ", round(popsd(), digits = 3)
          )
        )
      })
    }
  )

  ### Plots----
  output$pfunctionMean <- renderPlot({
    validate(
      need(
        expr = selection() != 'Binomial',
        message = "Set parameters and press the Simulate button!"
      )
    )
    validate(
      need(
        expr = input$norsd > 0 || selection() == "Uniform" ||
          selection() == "Poisson",
        message = "Please input a valid standard error"
      )
    )
    if (selection() == 'Poisson') {
      alpha <- 1 - input$cl
      ### calculate p-value 
      pValue <- getpvalue(selection())
      ### ci
      ci <- poisson.exact(
        x = sum(sampledata()),
        T = input$n,
        r = input$theta0,
        alternative = "two.side",
        tsmethod = "central",
        conf.level = input$cl
      )$conf.int
      ### set xlim
      cimax <- poisson.exact(
        x = sum(sampledata()),
        T = input$n,
        r = input$theta0,
        alternative = "two.side",
        tsmethod = "central",
        conf.level = 0.999
      )$conf.int
      xlim <- c(max(0, cimax[1]), cimax[2])
      ### get p-values list
      change <- (xlim[2] - xlim[1])/1500
      theta <- xlim[1]
      pvaluelist <- c()
      thetalist <- c()
      genepvalues <- function(theta){
        poisson.exact(
          x = sum(sampledata()),
          T = input$n,
          r = theta,
          alternative = "two.side",
          tsmethod = "central",
          conf.level = 0.99
        )$p.value
      }
      while (theta <= xlim[2]) {
        pvalues <- genepvalues(theta)
        pvaluelist <- c(pvaluelist, pvalues)
        thetalist <- c(thetalist, theta)
        theta <- theta + change
      }
      ### plot
      data <- as.data.frame(cbind(thetalist, pvaluelist))
      data <- rename(data, theta = thetalist)
      data <- rename(data, pValue = pvaluelist)
      g1 <- ggplot() +
        geom_line(
          data = data,
          mapping = aes(x = theta, y = pValue),
          color = "blue",
          linewidth = 1,
          alpha = 0.5
        ) +
        scale_x_continuous(
          limits = xlim,
          expand = expansion(mult = 0, add = 0)
        ) +
        scale_y_continuous(expand = expansion(mult = .05)) +
        labs(
          title = "P-value Function",
          x = "Null hypothesis mean", 
          y = "P-value",
          alt = "A plot of a set of p values versus different means "
        ) +
        geom_segment(
          aes(
            x = ci[1], 
            y = 0, 
            xend = ci[2], 
            yend = 0, 
            colour = "Confidence interval"
            ),
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_point(
          mapping = aes(x = c(ci[1], ci[2]),y = c(0,0)),
          alpha = 0
        ) +
        geom_errorbarh(
          aes(xmin = ci[1], xmax = ci[2], y = 0, colour = "Confidence interval"),
          height = 0.05*1,
          linewidth = 1
        ) +
        geom_segment(
          aes(
            x = xlim[1], 
            y = 1, 
            xend = meanhat(), 
            yend = 1, 
            colour = "Observed estimate"
            ),
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_segment(
          aes(
            x = meanhat(), 
            y = 0, 
            xend = meanhat(), 
            yend = 1, 
            colour = "Observed estimate"
            ),
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_segment(
          aes(
            x = xlim[1], 
            y = pValue, 
            xend = input$theta0, 
            yend = pValue, 
            colour = "Null value"
            ),
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_segment(
          aes(
            x = input$theta0, 
            y = 0, 
            xend = input$theta0, 
            yend = pValue, 
            colour = "Null value"
            ),
          linewidth = 1,
          na.rm = TRUE
        ) +
        scale_color_manual(
          name = NULL,
          values = c(
            "Confidence interval" = psuPalette[1],
            "Observed estimate" = psuPalette[4],
            "Null value" = "black"
          )
        ) +
        theme_bw() +
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16),
          legend.position = "bottom"
        )
      return(g1)
    }
    # other two
    if (selection() == 'Normal' || selection() == 'Uniform') {
      alpha <- 1 - input$cl
      ### calculate p value
      pValue <- getpvalue(selection())
      ### set xlim
      genelimit <- function(){
        lowerboundmax <- meanhat() + qnorm(0.001/2)*(popsd()/sqrt(input$n))
        upperboundmax <- meanhat() + qnorm(1 - 0.001/2)*(popsd()/sqrt(input$n))
        limit <- c(lowerboundmax, upperboundmax)
        return(limit)
      }
      lowerboundmax <- genelimit()[1]
      upperboundmax <- genelimit()[2]
      genepvalues <- function(theta){
        z_score <- (meanhat() - theta)/(popsd()/sqrt(input$n))
        pValue <- 2*pnorm(-abs(z_score))
        return(pValue)
      }
      #ci
      getci <- function(alpha){
        lowerbound <- meanhat() + qnorm(alpha/2)*(popsd()/sqrt(input$n))
        upperbound <- meanhat() + qnorm(1 - alpha/2)*(popsd()/sqrt(input$n))
        bound <- c(lowerbound, upperbound)
        return(bound)
      }
      ### lower bound should larger than 0 
      lowerbound <- max(0, getci(alpha)[1])
      upperbound <- getci(alpha)[2]
      ci <- c(lowerbound, upperbound)
      ### get p-value list
      thetarange <- c(lowerboundmax, upperboundmax)
      changetheta <- diff(thetarange)/1500
      theta <- lowerboundmax
      pvaluelist <- c()
      thetalist <- c()
      while (theta <= upperboundmax) {
        pvalues <- genepvalues(theta)
        pvaluelist <- c(pvaluelist, pvalues)
        thetalist <- c(thetalist, theta)
        theta = theta + changetheta
      }
      ### basic plot
      xlim <- c(max(0,lowerboundmax), upperboundmax)
      ###plot
      data <- as.data.frame(cbind(thetalist, pvaluelist))
      data <- rename(data, theta = thetalist)
      data <- rename(data, pValue = pvaluelist)
      g2 <- ggplot() +
        geom_line(
          data = data,
          mapping = aes(x = theta, y = pValue),
          color = "blue",
          linewidth = 1,
          alpha = 0.5
        ) +
        scale_x_continuous(
          limits = xlim,
          expand = expansion(mult = 0, add = 0)
        ) +
        scale_y_continuous(expand = expansion(mult = .05)) +
        labs(
          title = "P-value Function",
          x = "Null hypothesis mean", 
          y = "P-value",
          alt = "A plot of a set of p values versus different means "
        ) +
        geom_segment(
          aes(
            x = ci[1], 
            y = 0, 
            xend = ci[2], 
            yend = 0, 
            colour = "Confidence interval"
            ),
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_point(
          mapping = aes(x = c(ci[1], ci[2]),y = c(0, 0)),
          alpha = 0
        ) +
        geom_errorbarh(
          aes(xmin = ci[1], xmax = ci[2], y = 0, colour = "Confidence interval"),
          height = 0.05*1,
          linewidth = 1
        ) +
        geom_segment(
          aes(
            x = xlim[1], 
            y = 1, 
            xend = meanhat(), 
            yend = 1, 
            colour = "Observed estimate"
            ),
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_segment(
          aes(
            x = meanhat(), 
            y = 0, 
            xend = meanhat(),
            yend = 1, 
            colour = "Observed estimate"
            ),
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_segment(
          aes(
            x = xlim[1],
            y = pValue,
            xend = input$theta0, 
            yend = pValue, 
            colour = "Null value"
            ),
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_segment(
          aes(
            x = input$theta0, 
            y = 0, 
            xend = input$theta0, 
            yend = pValue, 
            colour = "Null value"
            ),
          linewidth = 1,
          na.rm = TRUE
        ) +
        scale_color_manual(
          name = NULL,
          values = c(
            "Confidence interval" = psuPalette[1],
            "Observed estimate" = psuPalette[4],
            "Null value" = "black"
          )
        ) +
        theme_bw() +
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16),
          legend.position = "bottom"
        )
      return(g2)
    }
  }
  )
  
  output$sampledist <- renderPlot({
    validate(
      need(
        expr = selection() != 'Binomial',
        message = "Set parameters and press the Simulate button!"
      )
    )
    validate(
      need(
        expr = input$norsd > 0 || selection() == "Uniform" || selection() == "Poisson",
        message = "Please input a valid standard error"
      )
    )
    getSamplingDist(selection(), input$theta0, input$n, popsd())
  })
  
  output$pfunctionPop <- renderPlot({
    validate(
      need(
        expr = input$simforp,
        message = "Set parameters and press the Simulate button!"
      )
    )
    validate(
      need(
        expr = selection() == 'Binomial',
        message = "Set parameters and press the Simulate button!"
      )
    )
    alphaP <- 1 - input$clofp
    ### calculate pvalue
    pValue <- getpvalue(selection())
    ### ci
    ciP <- binom.exact(
      x = successp(),
      n = input$nofp,
      p = input$theta0ofp,
      alternative = "two.side",
      tsmethod = "central",
      conf.level = input$clofp
    )$conf.int
    ### set xlim
    cimaxP <- binom.exact(
      x = successp(),
      n = input$nofp,
      p = input$theta0ofp,
      alternative = "two.side",
      tsmethod = "central",
      conf.level = 0.999
    )$conf.int
    xlimP <- c(max(0, cimaxP[1]), cimaxP[2])
    ### get p-value list
    changeP <- (xlimP[2] - xlimP[1])/1500
    thetaP <- xlimP[1]
    pvaluelistP <- c()
    thetalistP <- c()
    genepvaluesP <- function(thetaP){
      binom.exact(
        x = successp(),
        n = input$nofp,
        p = thetaP,
        alternative = "two.side",
        tsmethod = "central"
      )$p.value
    }
    while (thetaP <= xlimP[2]) {
      pvaluesP <- genepvaluesP(thetaP)
      pvaluelistP <- c(pvaluelistP,pvaluesP)
      thetalistP <- c(thetalistP,thetaP)
      thetaP <- thetaP + changeP
    }
    ### plot
    data <- as.data.frame(cbind(thetalistP, pvaluelistP))
    data <- rename(data, theta = thetalistP)
    data <- rename(data, pValue = pvaluelistP)
    gP <- ggplot() +
      geom_line(
        data = data,
        mapping = aes(x = theta, y = pValue),
        color = "blue",
        linewidth = 1,
        alpha = 0.5
      ) +
      scale_x_continuous(
        limits = xlimP,
        expand = expansion(mult = 0, add = 0)
      ) +
      scale_y_continuous(expand = expansion(mult = .05)) +
      labs(
        title = "P-value Function",
        x = "Null hypothesis proportion", 
        y = "P-value",
        alt = "A plot of a set of p values versus different proportions "
      ) +
      geom_segment(
        aes(
          x = ciP[1], 
          y = 0, 
          xend = ciP[2], 
          yend = 0, 
          colour = "Confidence interval"
          ),
        linewidth = 1,
        na.rm = TRUE
      ) +
      geom_point(
        mapping = aes(x = c(ciP[1], ciP[2]), y = c(0, 0)),
        alpha = 0
      ) +
      geom_errorbarh(
        aes(xmin = ciP[1], xmax = ciP[2], y = 0, colour = "Confidence interval"),
        height = 0.05*1,
        linewidth = 1
      ) +
      geom_segment(
        aes(
          x = xlimP[1], 
          y = 1, 
          xend = phatp(), 
          yend = 1, 
          colour = "Observed estimate"
          ),
        linewidth = 1,
        na.rm = TRUE
      ) +
      geom_segment(
        aes(
          x = phatp(), 
          y = 0, 
          xend = phatp(), 
          yend = 1, 
          colour = "Observed estimate"
          ),
        linewidth = 1,
        na.rm = TRUE
      ) +
      geom_segment(
        aes(
          x = xlimP[1],
          y = pValue,
          xend = input$theta0ofp, 
          yend = pValue,
          colour = "Null value"
          ),
        linewidth = 1,
        na.rm = TRUE
      ) +
      geom_segment(
        aes(
          x = input$theta0ofp,
          y = 0, 
          xend = input$theta0ofp, 
          yend = pValue, 
          colour = "Null value"
          ),
        linewidth = 1,
        na.rm = TRUE
      ) +
      scale_color_manual(
        name = NULL,
        values = c(
          "Confidence interval" = psuPalette[1],
          "Observed estimate" = psuPalette[4],
          "Null value" = "black"
        )
      ) +
      theme_bw() + 
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    return(gP)
  }
  )
  
  output$sampledistPop <- renderPlot({
    validate(
      need(
        expr = input$simforp,
        message = "Set parameters and press the Simulate button!"
      )
    )
    validate(
      need(
        expr = selection() == 'Binomial',
        message = "Set parameters and press the Simulate button!"
      )
    )
    getSamplingDist(selection(), input$theta0ofp, input$nofp)
  }
  )
  
  ### table----
  output$pvaluePop <- renderTable(
    expr = {
      validate(
        need(
          expr = selection() == 'Binomial',
          message = ""
        )
      )
      
      pValue <- getpvalue(selection())
      ciP <- binom.exact(
        x = successp(),
        n = input$nofp,
        p = input$theta0ofp,
        alternative = "two.side",
        tsmethod = "central",
        conf.level = input$clofp
      )$conf.int
      ctable <- matrix(c(phatp(), pValue, ciP[1], ciP[2]), nrow = 1)
      c2 <- paste("Confidence interval", " lower bound", sep = "<br>")
      c3 <- paste("Confidence interval", " upper bound", sep = "<br>")
      colnames(ctable) <- c("Sample proportion", "P-value", c2, c3)
      ctable
    },
    bordered = TRUE,
    sanitize.text.function = identity
  )
  
  output$pvalue <- renderTable(
    expr = {
      validate(
        need(
          expr = selection() != 'Binomial',
          message = ""
        )
      )
      validate(
        need(
          expr = input$norsd > 0 || selection() == "Uniform" ||
            selection() == "Poisson",
          message = ""
        ) 
      )
      if (selection() == "Poisson") {
        pValue <- getpvalue(selection())
        ci <- poisson.exact(
          x = sum(sampledata()),
          T = input$n,
          r = input$theta0,
          alternative = "two.side",
          tsmethod = "central",
          conf.level = input$cl
        )$conf.int
        ctable <- matrix(c(meanhat(),pValue,ci[1],ci[2]),nrow = 1)
        c2 <- paste("Confidence interval"," lower bound",sep = "<br>")
        c3 <- paste("Confidence interval"," upper bound",sep = "<br>")
        colnames(ctable) <- c("Sample mean","P-value",c2,c3)
        return(ctable)
      } else if (selection() == "Normal" || selection() == "Uniform") {
        pValue <- getpvalue(selection())
        getci <- function(alpha){
          lowerbound <- meanhat() + qnorm(alpha/2)*(popsd()/sqrt(input$n))
          upperbound <- meanhat() + qnorm(1 - alpha/2)*(popsd()/sqrt(input$n))
          bound <- c(lowerbound,upperbound)
          return(bound)
        }
        alpha <- 1 - input$cl
        ctable <- matrix(
          data = c(meanhat(), pValue, max(0, getci(alpha)[1]), getci(alpha)[2]),
          nrow = 1
        )
        c2 <- paste("Confidence interval", " lower bound", sep = "<br>")
        c3 <- paste("Confidence interval", " upper bound", sep = "<br>")
        colnames(ctable) <- c("Sample mean", "P-value", c2, c3)
        return(ctable)
      }
    },
    bordered = TRUE,
    sanitize.text.function = identity
  )

}

# Boast App Call ----
boastApp(ui = ui, server = server)
