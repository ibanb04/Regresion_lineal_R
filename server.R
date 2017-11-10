library(shiny)
library(ggplot2)
library(xtable)
library(plotly)
library(MASS)
library(Hmisc)

shinyServer(function(input, output) {
  
  Dataset <- reactive({
    if (is.null(input$file)) {
      # User has not uploaded a file yet
      return(marinverts)
    }
    Dataset <-as.data.frame(read.csv(input$file$datapath))
    return(Dataset)
  })
  
  varnames<-reactive({names(Dataset())})
  
  linearmodel<-reactive({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("mod<-lm(data=d,%s~%s)",y,x,x)
    eval(parse(text=tx))
    return(mod)
  })
  
  quadmodel<-reactive({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("mod<-lm(data=d,%s~%s+I(%s^2))",y,x,x)
    eval(parse(text=tx))
    return(mod)
  })
  
  nbmodel<-reactive({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("mod<-glm.nb(data=d,%s~%s)",y,x)
    eval(parse(text=tx))
    return(mod)
  })
  
  poissonmodel<-reactive({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("mod<-glm(data=d,%s~%s,family=poisson)",y,x)
    eval(parse(text=tx))
    return(mod)
  })
  
  ggstart<-reactive({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("g0<-ggplot(data=d,aes(x=%s,y=%s))",x,y)
    eval(parse(text=tx))
    return(g0)
  })
  
  output$table <- renderDataTable(Dataset())
  
  output$lplot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point() + geom_smooth(method=lm)+theme_bw()
    ggplotly(g1)
  })
  
  output$splot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point() + geom_smooth() +theme_bw()
    ggplotly(g1)
  })
  
  output$qplot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point() + geom_smooth(method="lm",formula=y~x+I(x^2), se=TRUE) +theme_bw()
    ggplotly(g1)
  })
  
  output$nbplot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point() +geom_smooth(method="glm.nb", se=TRUE) +theme_bw()
    ggplotly(g1)
  })
  
  output$poisplot <- renderPlotly({
    g0<-ggstart()
    g1<-g0+geom_point() +theme_bw()
    g1<-g1+ stat_smooth(method="glm",method.args=list( family="poisson"), se=TRUE)
    ggplotly(g1)
  })
  
  output$rplot <- renderPlotly({
    x<-input$X
    y<-input$Y
    d<-Dataset()
    tx<-sprintf("d$rank_%s<-rank(d$%s)",x,x)
    eval(parse(text=tx))
    tx<-sprintf("d$rank_%s<-rank(d$%s)",y,y)
    eval(parse(text=tx))
    
    tx<-sprintf("outp<-cor.test(d$%s,d$%s,method='spearman')",y,x)
    eval(parse(text=tx))
    output$cor <- renderPrint(outp)
    
    tx<-sprintf("g0<-ggplot(data=d,aes(x=rank_%s,y=rank_%s))",x,y)
    eval(parse(text=tx))
    
    g1<-g0+geom_point() +theme_bw() + geom_smooth(method=lm)
    ggplotly(g1)
  })
  
  output$dplot <- renderPlot({
    mod<-linearmodel()
    par(mfcol=c(2,2))
    plot(mod)
  })
  
  output$sumtable <- renderText({
    mod<-linearmodel()
    a<-summary(mod)
    output$sum<-renderPrint(a)
    print(xtable(a),type="html")
    
  } )
  
  output$anova<-renderPrint(anova(linearmodel()))
  
  output$quadsum<-renderPrint(
    {
      mod<-quadmodel()
      summary(mod)
    })
  output$quadanova<-renderPrint(
    {
      mod<-quadmodel()
      anova(mod)
    })
  
  output$nbsum<-renderPrint(
    {
      mod<-nbmodel()
      summary(mod)
    })
  output$nbanova<-renderPrint(
    {
      mod<-nbmodel()
      anova(mod)
    })
  
  output$poissum<-renderPrint(
    {
      mod<-poissonmodel()
      summary(mod)
    })
  output$poisanova<-renderPrint(
    {
      mod<-poissonmodel()
      anova(mod)
    })
  
  output$antable <- renderText({
    mod<-linearmodel()
    a<-anova(mod)
    print(xtable(a),type="html")
    
  } )
  
  output$sumtext <- renderText({
    x<-input$X
    y<-input$Y
    mod<-linearmodel()
    a<-summary(mod)
    int<-round(coef(mod)[1],4)
    slope<-round(coef(mod)[2],4)
    rsq<-round(a$r.squared,3)
    sprintf("The regression line is %s = %s + %s %s with an r squared value of %s",y,int,slope,x,rsq)
    
  } )
  output$X <- renderUI({ 
    selectInput("X", "Seleccione la variable independiente", varnames())
  })
  output$Y <- renderUI({ 
    selectInput("Y", "Seleccione la variable dependiente", varnames(),selected=varnames()[2])
  })
  
  
})