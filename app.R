# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(exactci)
library(ggplot2)
library(dplyr)

# Load additional funCtions
source("samplingfunctions.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "purple",
    ### Create the app header ----
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
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("P-Value Function and Hypothesis Testing"), # full name.
          p("This app can generate the p-value function, which displays 
            confidence limits and p-values of any confidence level of the parameter. 
            And the user can use this app to do hypothesis testing for one sample."),
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
          ##### Go Button
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
          ##### Create two lines of space
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
            div(class = "updated", "Last Update: 6/5/2022 by JF.")
          )
        ),
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Explore the p-value funtion"),
          p("On this page, you can explore p-value functions in different 
            situations."),
          p("Select the hypothesis test type, select the population distribution 
            and then change the inputs. Once you are ready, click simulate. All 
            p values are the results of the two tailed hypothesis test."),
          fluidPage(
            tabsetPanel(
              id = "whichtype",
              type = "tabs",
              ##### Testing for single proportion ----
              tabPanel(
                title = "Single proportion",
                value = "pro",
                column(
                  width = 4,
                  offset = 0,
                  wellPanel(
                    #### input part----
                    #population dis
                    tags$strong("Population distribution"),
                    radioButtons(
                      inputId = "binomial", 
                      label = NULL, 
                      choices = c("Binomial"),
                      selected = "Binomial", 
                      width = '100%'),
                    #confidence level
                    tags$strong("Confidence level"),
                    numericInput(
                      inputId = "clofp",
                      label = HTML(paste("1 - ","&alpha;")),
                      min = 0.8,
                      max = 0.99,
                      step = 0.01,
                      value = 0.95
                    ),
                    br(),
                    #sample size
                    tags$strong("Sample size"),
                    sliderInput(
                      inputId = "nofp",
                      label = "n",
                      min = 2,
                      max = 90,
                      step = 1,
                      value = 50
                    ),
                    br(),
                    #null hypothesis
                    tags$strong("Null hypothesis"),
                    sliderInput(
                      inputId = "theta0ofp",
                      label= div(HTML(paste0("H",tags$sub("0"),": p"))),
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
                #### output prat----
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("pfunctionPop"),
                  br(),
                  plotOutput("sampledistPop"),
                  br(),
                  checkboxInput("resultsPop",
                                "Show test results", FALSE),
                  conditionalPanel(
                    condition = "input.resultsPop==1",
                    tableOutput("pvaluePop")
                  )
                )
              ),
              ##### Testing for single mean ----
              tabPanel(
                title = "Single mean",
                value = "mean",
                column(
                  width = 4,
                  offset = 0,
                  wellPanel(
                    #### input part----
                    #population dis
                    tags$strong("Population distribution"),
                    radioButtons(
                      inputId = "types", 
                      label = NULL, 
                      choices = c("Poisson","Normal","Uniform"),
                      selected = "Normal", 
                      width = '100%'
                    ),
                    br(),
                    #confidence level
                    tags$strong("Confidence level"),
                    numericInput(
                      inputId = "cl",
                      label = HTML(paste("1 - ","&alpha;")),
                      min = 0.8,
                      max = 0.99,
                      step = 0.01,
                      value = 0.95
                    ),
                    br(),
                    #sample size
                    tags$strong("Sample size"),
                    sliderInput(
                      inputId = "n",
                      label = "n ",
                      min = 2,
                      max = 90,
                      step = 1,
                      value = 50
                    ),
                    br(),
                    #null hypothesis
                    tags$strong("Null hypothesis"),
                    uiOutput("choosetheta0"),
                    br(),
                    #popsd for normal
                    uiOutput("choosepopsd"),
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
                #### output part----
                column(
                  width = 8,
                  offset = 0,
                  plotOutput("pfunctionMean"),
                  br(),
                  plotOutput("sampledist"),
                  br(),
                  checkboxInput("resultsMean",
                                "Show test results", FALSE),
                  conditionalPanel(
                    condition = "input.resultsMean==1",
                    tableOutput("pvalue")
                  )
                )
              )
            )
          )
        ),
        #### Set up the References Page ----
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
            "Carey, R. and Hatfield, N. J. (2022). boastUtils: BOAST utlities.
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
            "Chang, W., Cheng J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny:
            Web application framework for R. (v 1.7.1). [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Fay, M. (2010). exactci: Exact P-values and Matching Confidence 
            Intervals for simple Discrete Parametric Cases. (v 1.4-2).[R package].
            Available from 
            https://CRAN.R-project.org/package=exactci
            "
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2022). shinyWidgets: Custom
            inputs widgets for shiny. (v 0.7.0). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K. (2022). dplyr: A 
            Grammar of Data Manipulation. (v 1.0.9).[R package]. Available from 
            https://dplyr.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.3.6) [R package]. Available from
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
        or do a two-tailed hypothesis test for one single sample"
      )
    }
  )
  ## set buttons----
  ### explore button
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
  ### simulate button proportion
  observeEvent(
    eventExpr = input$simforp,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "simforp",
        label = "Re-simulate!",
        style = "default",
        disabled = FALSE 
      )
    }
  )
  ### simulation button mean
  observeEvent(
    eventExpr = input$sim,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "sim",
        label = "Re-simulate!",
        style = "default",
        disabled = FALSE
      )
    }
  )
  
  ## update inputs values----
  ## population selection
  selection<-eventReactive(
    eventExpr = c(input$simforp,input$sim),
    valueExpr = {
      
      if(input$whichtype=='mean'){
        
        value<-input$types}
      
      if(input$whichtype=='pro'){
        
        value<-input$binomial}
      
      return(value)
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  
  
  ### update theta0 of Mean
  output$choosetheta0<-renderUI({
    if(input$types == 'Poisson'){
      theta0<-
        sliderInput(
          inputId = "theta0",
          label = div(HTML(paste0("H",tags$sub("0"),": ", "&mu;"))),
          min = 0,
          max = 50,
          value = 25,
          step = 1)
    }
    if(input$types == 'Normal'){
      theta0<-
        sliderInput(
          inputId = "theta0",
          label = div(HTML(paste0("H",tags$sub("0"),": ", "&mu;"))),
          min = 0,
          max = 120,
          value = 90,
          step = 1)
    }  
    if(input$types == 'Uniform'){
      theta0<-
        sliderInput(
          inputId = "theta0",
          label = div(HTML(paste0("H",tags$sub("0"),": ", "&mu;"))),
          min = 0,
          max = 120,
          value = 60,
          step = 1)
    }
    return(theta0)
  })
  
  ### update sd of normal
  output$choosepopsd<-renderUI({
    if(input$types == 'Normal'){
      
      numericInput(
        inputId = "norsd",
        label = tags$div(
          tags$strong("Standard deviation"),
          HTML("&sigma;")
        ),
        min = 1,
        max = 50, 
        value = 10,
        step = 1)
    }
  }
  )
  
  ### update popsd of Mean
  getpopsd<-function(selection){
    #default value
    if(selection=='Binomial'){
      popsd<-1
    }
    if(selection=='Poisson'){
      popsd<-sqrt(25)
    }
    if(selection=='Normal'){
      popsd<-input$norsd
    }
    if(selection=='Uniform'){
      popsd<-sqrt(1200)
    }
    return(popsd)
  }
  
  
  ### update some statistics
  ### proportion part 
  theta0p<-
    eventReactive(
      eventExpr = input$simforp,
      valueExpr = {
        input$theta0ofp
      }
    )
  
  np<-
    eventReactive(
      eventExpr = input$simforp,
      valueExpr = {
        input$nofp
      }
    )
  
  clp<-
    eventReactive(
      eventExpr = input$simforp,
      valueExpr = {
        input$clofp
      }
    )
  
  sampledatap<-
    eventReactive(
      eventExpr = input$simforp,
      valueExpr = {
        getsample(selection(),np())
      }
    )
  
  successp<-
    eventReactive(
      eventExpr = input$simforp,
      valueExpr = {
        sum(sampledatap())
      }
    )
  
  phatp<-
    eventReactive(
      eventExpr = input$simforp,
      valueExpr = {
        round(mean(sampledatap()),3)
      }
    )
  
  ### mean part
  popsd<-
    eventReactive(
      eventExpr = input$sim,
      valueExpr = {
        getpopsd(input$types)
        
      }
    )
  
  theta0<-
    eventReactive(
      eventExpr = input$sim,
      valueExpr = {
        input$theta0
        
      }
    )
  
  n<-
    eventReactive(
      eventExpr = input$sim,
      valueExpr = {
        input$n
        
      }
    )
  
  cl<-
    eventReactive(
      eventExpr = input$sim,
      valueExpr = {
        input$cl
        
      }
    )
  
  sampledata<-
    eventReactive(
      eventExpr = input$sim,
      valueExpr = {
        getsample(input$types,input$n,popsd())
        
      }
    )
  
  meanhat<-
    eventReactive(
      eventExpr = input$sim,
      valueExpr = {
        round(mean(sampledata()),3)
        
      }
    )
  
  ### error message
  observeEvent(
    eventExpr = input$sim ,
    handlerExpr = {
      if(input$cl>=1 || input$cl<=0 || popsd() <=0 ){
        sendSweetAlert(
          session = session,
          type = "error",
          title = "Error: Please check your inputs",
          text = tags$div(
            p("Invalid confidence level", tags$strong("AND/OR") ),
            p("Invalid population standard deviation")
          ),
          html = TRUE 
        )
      }
    }
  )
  
  observeEvent(
    eventExpr =  input$simforp,
    handlerExpr = {
      if(input$clofp>=1 || input$clofp<=0 ){
        sendSweetAlert(
          session = session,
          type = "error",
          title = "Error: Please check your inputs",
          text = tags$div(
            p("Invalid confidence level")
          ),
          html = TRUE 
        )
      }
    }
  )
  
  
  
  ### update p-value
  getpvalue<-function(selection){
    if(selection=='Binomial'){
      p_value<-binom.exact(
        x = successp(),
        n = np(),
        p = theta0p(),
        alternative="two.side",
        tsmethod = "central"
      )$p.value
    }
    if(selection=='Poisson'){
      p_value<-poisson.exact(
        x = sum(sampledata()),
        T = n(),
        r = theta0(),
        alternative = "two.side",
        tsmethod = "central"
      )$p.value
    }
    if(selection=='Normal'||selection=='Uniform'){
      z_score<-(meanhat()-theta0())/(popsd()/sqrt(n()))
      p_value<-2*pnorm(-abs(z_score))
    }
    
    return(round(p_value,3))
    
  }
  
  #### show sample data
  observeEvent(
    eventExpr = input$simforp,
    handlerExpr = {
      output$sampledataPop<-renderUI(
        withMathJax(
          p("Sample Data:"),
          p("\\(n =\\) ", np()),
          p("\\(x =\\) ", successp()),
          p("\\(\\hat{p} =\\) ", phatp()),
          helpText(
            paste0("Assumptions \\( n\\hat{p} \\geq 5\\) and \\( n(1-\\hat{p}) \\geq 5\\)", 
                   ifelse(np() * phatp() >= 5 & np() * (1 - phatp()) >= 5, " are met.", " are not met.")))
        )
      )
    }
  )
  
  
  observeEvent(
    eventExpr = input$sim,
    handlerExpr = {
      output$sampledataMean<-renderUI({
        validate(
          need( popsd()> 0 || selection() =="Uniform" ||selection() =="Poisson",
                message = "Please input a valid standard error"
          )
        )
        withMathJax(
          p("Sample Data:"),
          p("\\(n =\\) ", n()),
          p("\\(\\bar{x} =\\) ", meanhat()),
          p("\\(\\sigma =\\) ", round(popsd(),3))
        )
      }
      )
    }
  )
  
  
  #### plots----
  
  output$pfunctionMean<-renderPlot({
    validate(
      need(cl() > 0 && cl() < 1,
           message = "Please input a valid confidence level"
      ),
      need(popsd() > 0 || selection()=="Uniform"|| selection()=="Poisson",
           message = "Please input a valid standard error"
      )
    )
    if(selection()=='Poisson'){
      alpha<-1-cl()
      ### calculate p-value 
      p_value<-getpvalue(selection())
      ### ci
      ci<-poisson.exact(
        x = sum(sampledata()),
        T = n(),
        r = theta0(),
        alternative = "two.side",
        tsmethod = "central",
        conf.level = cl()
      )$conf.int
      ### set xlim
      cimax<-poisson.exact(
        x = sum(sampledata()),
        T = n(),
        r = theta0(),
        alternative = "two.side",
        tsmethod = "central",
        conf.level = 0.999
      )$conf.int
      
      xlim<-c(max(0,cimax[1]),cimax[2])
      
      ### get p-values list
      change<-(xlim[2]-xlim[1])/1500
      theta<-xlim[1]
      pvaluelist<-c()
      thetalist<-c()
      genepvalues<-function(theta){
        poisson.exact(
          x = sum(sampledata()),
          T = n(),
          r = theta,
          alternative = "two.side",
          tsmethod = "central",
          conf.level = 0.99
        )$p.value
      }
      while(theta<=xlim[2]){
        pvalues<-genepvalues(theta)
        pvaluelist<-c(pvaluelist,pvalues)
        thetalist<-c(thetalist,theta)
        theta<-theta+change
      }
      ### plot
      data<-as.data.frame(cbind(thetalist,pvaluelist))
      data<-rename(data,theta=thetalist)
      data<-rename(data,p_value=pvaluelist)
      g1<-
        ggplot()+
        geom_line(
          data=data,
          mapping = aes(x=theta,y=p_value)
        )+
        lims(
          x=xlim,
          y=c(0,1)
        )+
        labs(
          title = "P-value Function",
          x = "Null hypothesis mean", 
          y = "P-value",
          alt = "A plot of a set of p values versus different means "
        )+
        geom_segment(
          aes(x = ci[1], y = alpha, xend = ci[2], yend = alpha, colour = "Confidence interval")
        )+
        geom_segment(
          aes(x = ci[2], y = 0, xend = ci[2], yend = alpha, colour = "Confidence interval")
        )+
        geom_segment(
          aes(x = ci[1], y = 0, xend = ci[1], yend = alpha, colour = "Confidence interval")
        )+
        geom_segment(
          aes(x = xlim[1], y = 1, xend = meanhat(), yend = 1, colour = "Observed estimate")
        )+
        geom_segment(
          aes(x = xlim[1], y = p_value, xend = theta0(), yend = p_value, colour = "Null value")
        )+
        geom_segment(
          aes(x = theta0(), y = 0, xend = theta0(), yend = p_value, colour = "Null value")
        )+
        scale_color_manual(
          name = NULL,
          values = boastUtils::boastPalette
        )+
        theme_bw()+
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16)
        )
      
      return(g1)
    }
    # other two
    if(selection()=='Normal'||selection()=='Uniform'){
      alpha<-1-cl()
      ### calculate p value
      p_value<-getpvalue(selection())
      ### set xlim
      genelimit<-function(){
        lowerboundmax<-meanhat()+qnorm(0.001/2)*(popsd()/sqrt(n()))
        upperboundmax<-meanhat()+qnorm(1-0.001/2)*(popsd()/sqrt(n()))
        limit<-c(lowerboundmax,upperboundmax)
        return(limit)
      }
      
      lowerboundmax<-genelimit()[1]
      upperboundmax<-genelimit()[2]
      
      genepvalues<-function(theta){
        
        z_score<-(meanhat()-theta)/(popsd()/sqrt(n()))
        p_value<-2*pnorm(-abs(z_score))
        
        return(p_value)
      }
      
      #ci
      getci<-function(alpha){
        
        lowerbound<-meanhat()+qnorm(alpha/2)*(popsd()/sqrt(n()))
        upperbound<-meanhat()+qnorm(1-alpha/2)*(popsd()/sqrt(n()))
        bound<-c(lowerbound,upperbound)
        
        return(bound)
      }
      
      ### lower bound should larger than 0 
      lowerbound<-max(0,getci(alpha)[1])
      upperbound<-getci(alpha)[2]
      ci<-c(lowerbound,upperbound)
      
      ### get p-value list
      thetarange<-c(lowerboundmax,upperboundmax)
      changetheta<-diff(thetarange)/1500
      theta<-lowerboundmax
      pvaluelist<-c()
      thetalist<-c()
      while(theta<=upperboundmax){
        pvalues<-genepvalues(theta)
        pvaluelist<-c(pvaluelist,pvalues)
        thetalist<-c(thetalist,theta)
        theta=theta+changetheta}
      
      
      ### basic plot
      xlim<-c(max(0,lowerboundmax),upperboundmax)
      
      ###plot
      data<-as.data.frame(cbind(thetalist,pvaluelist))
      data<-rename(data,theta=thetalist)
      data<-rename(data,p_value=pvaluelist)
      g2<-
        ggplot()+
        geom_line(
          data=data,
          mapping = aes(x=theta,y=p_value)
        )+
        lims(
          x=xlim,
          y=c(0,1)
        )+
        labs(
          title = "P-value Function",
          x = "Null hypothesis mean", 
          y = "P-value",
          alt = "A plot of a set of p values versus different means "
        )+
        geom_segment(
          aes(x = ci[1], y = alpha, xend = ci[2], yend = alpha, colour = "Confidence interval")
        )+
        geom_segment(
          aes(x = ci[2], y = 0, xend = ci[2], yend = alpha, colour = "Confidence interval")
        )+
        geom_segment(
          aes(x = ci[1], y = 0, xend = ci[1], yend = alpha, colour = "Confidence interval")
        )+
        geom_segment(
          aes(x = xlim[1], y = 1, xend = meanhat(), yend = 1, colour = "Observed estimate")
        )+
        geom_segment(
          aes(x = xlim[1], y = p_value, xend = theta0(), yend = p_value, colour = "Null value")
        )+
        geom_segment(
          aes(x = theta0(), y = 0, xend = theta0(), yend = p_value, colour = "Null value")
        )+
        scale_color_manual(
          name=NULL,
          values = boastUtils::boastPalette
        )+
        theme_bw()+
        theme(
          plot.caption = element_text(size = 18),
          text = element_text(size = 18),
          axis.title = element_text(size = 16)
        )
      
      return(g2)
    }
  }
  )
  
  observeEvent(
    eventExpr = input$sim,
    handlerExpr = {
      output$sampledist<-renderPlot(
        isolate({
          validate(
            need(input$cl > 0 && input$cl < 1,
                 message = "Please input a valid confidence level"
            ),
            need(input$norsd > 0 || input$types=="Uniform"|| input$types=="Poisson",
                 message = "Please input a valid standard error"
            )
          )
          getsampling(selection(),input$theta0,input$n,popsd())
        }
        )
      )
    }
  )
  
  output$pfunctionPop<-renderPlot({
    validate(
      need(clp() > 0 && clp() < 1,
           message = "Please input a valid confidence level"
      )
    )
    alphaP<-1-clp()
    ### calculate pvalue
    p_value<-getpvalue(selection())
    ### ci
    ciP<-binom.exact(
      x = successp(),
      n = np(),
      p = theta0p(),
      alternative = "two.side",
      tsmethod = "central",
      conf.level = clp()
    )$conf.int
    ### set xlim
    cimaxP<-binom.exact(
      x = successp(),
      n = np(),
      p = theta0p(),
      alternative="two.side",
      tsmethod = "central",
      conf.level = 0.999
    )$conf.int
    
    xlimP<-c(max(0,cimaxP[1]),cimaxP[2])
    
    ### get p-value list
    changeP<-(xlimP[2]-xlimP[1])/1500
    thetaP<-xlimP[1]
    pvaluelistP<-c()
    thetalistP<-c()
    genepvaluesP<-function(thetaP){
      binom.exact(
        x = successp(),
        n = np(),
        p = thetaP,
        alternative="two.side",
        tsmethod="central"
      )$p.value
    }
    while(thetaP<=xlimP[2]){
      pvaluesP<-genepvaluesP(thetaP)
      pvaluelistP<-c(pvaluelistP,pvaluesP)
      thetalistP<-c(thetalistP,thetaP)
      thetaP<-thetaP+changeP
    }
    
    ### plot
    data<-as.data.frame(cbind(thetalistP,pvaluelistP))
    data<-rename(data,theta=thetalistP)
    data<-rename(data,p_value=pvaluelistP)
    gP<-
      ggplot()+
      geom_line(
        data=data,
        mapping = aes(x=theta,y=p_value)
      )+
      lims(
        x=xlimP,
        y=c(0,1)
      )+
      labs(
        title = "P-value Function",
        x = "Null hypothesis proportion", 
        y = "P-value",
        alt = "A plot of a set of p values versus different proportions "
      )+
      geom_segment(
        aes(x = ciP[1], y = alphaP, xend = ciP[2], yend = alphaP, colour = "Confidence interval")
      )+
      geom_segment(
        aes(x = ciP[2], y = 0, xend = ciP[2], yend = alphaP, colour = "Confidence interval")
      )+
      geom_segment(
        aes(x = ciP[1], y = 0, xend = ciP[1], yend = alphaP, colour = "Confidence interval")
      )+
      geom_segment(
        aes(x = xlimP[1], y = 1, xend = phatp(), yend = 1, colour = "Observed estimate")
      )+
      geom_segment(
        aes(x = xlimP[1], y = p_value, xend = theta0p(), yend = p_value, colour = "Null value")
      )+
      geom_segment(
        aes(x = theta0p(), y = 0, xend = theta0p(), yend = p_value, colour = "Null value")
      )+
      scale_color_manual(
        name = NULL,
        values = boastUtils::boastPalette
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16)
      )
    
    return(gP)
  })
  
  observeEvent(
    eventExpr = input$simforp,
    handlerExpr = {
      output$sampledistPop<-renderPlot(
        isolate({
          validate(
            need(input$clofp > 0 && input$clofp < 1,
                 message = "Please input a valid confidence level"
            )
          )
          getsampling(selection(),input$theta0ofp,input$nofp)
        }
        )
      )
    }
  )
  
  ### table----
  observeEvent(
    eventExpr = input$simforp,
    
    handlerExpr = {
      output$pvaluePop<-renderTable(
        isolate({
          validate(
            need(input$clofp > 0 && input$clofp < 1,
                 message = "Please input a valid confidence level"
            )
          )
          
          p_value<-getpvalue(selection())
          ciP<-binom.exact(
            x = successp(),
            n = input$nofp,
            p = input$theta0ofp,
            alternative="two.side",
            tsmethod = "central",
            conf.level = input$clofp
          )$conf.int
          ctable<-matrix(c(phatp(),p_value,ciP[1],ciP[2]),nrow=1)
          colnames(ctable)<-c("Sample proportion","P-value","Lower bound","Upper bound")
          ctable
        }
        )
      )
    }
  )
  
  observeEvent(
    eventExpr = input$sim,
    handlerExpr = {
      output$pvalue<-renderTable(
        isolate({
          validate(
            need(input$cl > 0 && input$cl < 1,
                 message = "Please input a valid confidence level"
            ),
            need(input$norsd > 0 || selection()=="Uniform"|| selection()=="Poisson",
                 message = "Please input a valid standard error"
            ) 
          )
          
          if(selection()=="Poisson"){
            p_value<-getpvalue(selection())
            ci<-poisson.exact(
              x = sum(sampledata()),
              T = input$n,
              r = input$theta0,
              alternative = "two.side",
              tsmethod = "central",
              conf.level = input$cl
            )$conf.int
            ctable<-matrix(c(meanhat(),p_value,ci[1],ci[2]),nrow=1)
            colnames(ctable)<-c("Sample mean","P-value","Lower bound","Upper bound")
            return(ctable)
          }
          
          if(selection()=="Normal" || selection()=="Uniform"){
            p_value<-getpvalue(selection())
            getci<-function(alpha){
              lowerbound<-meanhat()+qnorm(alpha/2)*(popsd()/sqrt(input$n))
              upperbound<-meanhat()+qnorm(1-alpha/2)*(popsd()/sqrt(input$n))
              bound<-c(lowerbound,upperbound)
              return(bound)
            }
            alpha<-1-input$cl
            ctable<-matrix(c(meanhat(),p_value,max(0,getci(alpha)[1]),getci(alpha)[2]),nrow=1)
            colnames(ctable)<-c("Sample mean","P-value","Lower bound","Upper bound")
            return(ctable)
          }
        }
        )
      )
    }
  )
  
  
}


# Boast App Call ----
boastApp(ui = ui, server = server)