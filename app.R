rm(list = ls())
cat("\014") 

if (!("plotly" %in% rownames(installed.packages()))) {
  print("Please make sure that you have plotly installed before running the app")
  print("Please run the code again after install the package")
  install.packages("plotly")
}
library("shiny")
library("ggplot2")
library("plotly")

lifeExpectancy <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip = 4, header = TRUE)
fertilityRate <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", skip = 4, header = TRUE)
countryCode <- read.csv("Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv")
population <- read.csv("population.csv")

 # clean data
dropcols <- c("Indicator.Name", "Indicator.Code", "X2015", "X2016", "X")
lifeExpectancy <- lifeExpectancy[, !(colnames(lifeExpectancy) %in% dropcols)]
fertilityRate <- fertilityRate[, !(colnames(fertilityRate) %in% dropcols)]
population <- population[, !(colnames(population) %in% dropcols)]
colnames(lifeExpectancy)[3:ncol(lifeExpectancy)] <- sapply(colnames(lifeExpectancy)[3:ncol(lifeExpectancy)], function(x) substr(x, 2, 5))
colnames(fertilityRate)[3:ncol(fertilityRate)] <- sapply(colnames(fertilityRate)[3:ncol(fertilityRate)], function(x) substr(x, 2, 5))
colnames(population)[3:ncol(population)] <- sapply(colnames(population)[3:ncol(population)], function(x) substr(x, 2, 5))
fertilityRate <- merge(x = fertilityRate, y = countryCode[c("Country.Code", "Region")], by = "Country.Code", all.x = TRUE)
keeprows <- fertilityRate$Region != ""
lifeExpectancy <- lifeExpectancy[keeprows,]
fertilityRate <- fertilityRate[keeprows,]
fertilityRate$Region <- droplevels(fertilityRate$Region)
population <- population[keeprows,]
regions <- levels(fertilityRate$Region)
color <- c("http://www.iconsdb.com/icons/download/color/3B7CE3/square-rounded-16.ico",
           "http://www.iconsdb.com/icons/download/color/E82E09/square-rounded-16.ico",
           "http://www.iconsdb.com/icons/download/orange/square-rounded-16.ico",
           "http://www.iconsdb.com/icons/download/color/169C28/square-rounded-16.ico",
           "http://www.iconsdb.com/icons/download/color/9E14A3/square-rounded-16.ico",
           "http://www.iconsdb.com/icons/download/caribbean-blue/square-rounded-16.ico",
           "http://www.iconsdb.com/icons/download/barbie-pink/square-rounded-16.ico")

ui <- fluidPage(
  headerPanel("Yixin's Homework 2"),
  mainPanel(
    plotlyOutput("plot"),
    sliderInput("year", label = NULL, min = 1960, max = 2014, value = 2014, 
                animate = animationOptions(interval = 250, playButton = icon('play', "fa-2x"), pauseButton = icon('pause', "fa-2x")), 
                ticks = FALSE, width = "100%")
  ),
  sidebarPanel(
    checkboxGroupInput("region", "Regions", choiceNames = mapply(regions, color, 
                                                                 FUN = function(region, colorURL) {tagList(tags$img(src=colorURL, width=10, height=10), region)}, 
                                                                 SIMPLIFY = FALSE, USE.NAMES = FALSE),
                       choiceValues = regions),
    sliderInput("pop", label = "Population", min = 1, max = 20, step = 0.2, value = 8, ticks = FALSE),
    tags$script(HTML("
        $(document).ready(function() {setTimeout(function() {
                     supElement = document.getElementById('pop').parentElement;
                     $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
                     }, 50);})
                     "))
  )
)

server <- function(input, output)  {
  output$plot <- renderPlotly({
    df <- data.frame(c(fertilityRate["Region"], fertilityRate["Country.Name"], lifeExpectancy[as.character(input$year)], 
                       fertilityRate[as.character(input$year)], population[as.character(input$year)]))
    colnames(df) <- c("Region","Country", "le", "fr", "pop")
    df <- df[order(-df[, "pop"]),] 
    alpha <- c(1, 1, 1, 1, 1, 1, 1)
    if (!is.null(input$region)) {
      alpha.in <- (regions %in% input$region) * 1
      alpha.not <- as.numeric(!(regions %in% input$region)) * 0.15
      alpha <- alpha.in + alpha.not
    }
    p <- ggplot(df, aes(le, fr, size = pop, fill = Region, ctry = Country,
                        text = paste('<em>', Country, '</em></br> Life Expenctancy: ', round(le, 1), 
                                     '</br> Fertility Rate: ', round(fr, 2), '</br> Population: ', round(pop, 2)))) + 
      geom_point(shape=21, stroke = 0.2) + scale_size(range = c(1, input$pop)) + 
      scale_fill_manual(values = alpha(c("#3773d3", "#cc3426", "#efa615", "#369105", "#800584", "#07aff2", "#d8089a"), alpha)) + 
      scale_x_continuous("Life expectancy", limits = c(10, 90), breaks = seq(10,90,10)) + theme_bw() + theme(legend.position = "none") + 
      scale_y_continuous("Fertility rate", limits = c(1, 9), breaks = seq(1,9,1)) 
    ggplotly(p, tooltip=c("text"))
  })
}

shinyApp(ui = ui, server = server)

