####
#### Part 1: Demographics and Hospitals
#### 

library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(maps)
library(maptools)
library(sp)
library(rgeos)
library(googleVis)

data("state.fips")
mapStates <- map("state", fill = TRUE, plot = FALSE)
sta.fips = state.fips$fips[match(mapStates$names, state.fips$polyname)] * 1000
HealthData <- read.csv("HealthData.csv", header = TRUE)
HealthData$Physicians <- HealthData$MD.s..NF..Prim.Care.Pat.Care.Excl.Hsp.Rsdnts+HealthData$MD.s..NF..Prim.Care.Pat.Care.Hosp.Rsdnts+HealthData$DO.s..NF..Prim.Care.Pat.Care.Excl.Hsp.Rsdnts+HealthData$DO.s..NF..Prim.Care.Pat.Care.Hosp.Rsdnts
HealthData$HospitalsPer <- 1000 * HealthData$Total.Number.Hospitals/HealthData$Population.Estimate
HealthData$BedsPer <- 1000 * HealthData$Total.Hospital.Beds/HealthData$Population.Estimate
HealthData$PhysiciansPer <- 1000 * HealthData$Physicians/HealthData$Population.Estimate
mapStates$abb = state.fips$abb[match(mapStates$names, state.fips$polyname)]

xlim <- list(
  min = min(HealthData$Per.Capita.Income) - 500,
  max = max(HealthData$Per.Capita.Income) + 500
) 
ylim <- list(
  min = min(HealthData$HospitalsPer),
  max = max(HealthData$HospitalsPer)
)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(width="1000px",
                        googleChartsInit(),                      
                        div(class="outer",
                            tags$head(
                              # Include our custom CSS
                              includeCSS("HealthData.css")
                            ),
                            # Application title
                            tags$div(class="header", id= NULL, NULL, textOutput("titleString") ),
                            tags$div(class="topPanel", leafletOutput("map", width="100%", height="400px")),
                            
                            tags$div(class="bottomLeftPanel", htmlOutput("bubbleChart")),
                            
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = FALSE, top = "auto", left = "auto", right = 5, bottom = 5,
                                          width = "25%", height = "400px",
                                          
                                          h4("State Explore"),
                                          
                                          radioButtons("category", label = h5("Category"),
                                                       choices = list("Per Capita Income" = 1, "Population Per Square Mile" = 2, "Hospitals (Per 1000 people)" = 3, "Hospital Beds (Per 1000 people)" = 4, "Primary Care Physicians (Per 1000 people)" = 5), 
                                                       selected = 1),
                                          sliderInput("year", label = h5("Year"), min = 2010, max = 2012, sep="",
                                                      value = 2010, animate = FALSE)
                            )              
                        ))
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  categoryString <- reactive({
    switch (input$category,
            "1" = "Per Capita Income ",
            "2" = "Population Per Square Mile",
            "3" = "Hospitals (Per 1000 People)",
            "4" = "Hospital Bed (Per 1000 People)",
            "5" = "Primary Care Physicians (Per 1000 People)")
  })
  
  yearData <- reactive({
    df <- HealthData %>%
      filter(Year == input$year) %>%
      arrange(Region)    
  })
  
  xVar <- reactive({
    switch (input$category,
            "3" = "HospitalsPer",
            "4" = "BedsPer",
            "5" = "PhysiciansPer")    
  })
  
  output$titleString <- renderText({
    paste("US", categoryString(), "-", input$year)
  })  
  
  output$map <- renderLeaflet({
    
    df <- yearData()
    qpal <- switch (input$category,
                    "1" =  colorQuantile("Greens", df$Per.Capita.Income, n = 10),
                    "2" = colorQuantile("Greens", df$Population.per.Square.Mile, n = 10),
                    "3" = colorQuantile("Greens", df$HospitalsPer, n = 10),
                    "4" = colorQuantile("Greens", df$BedsPer, n = 10),
                    "5" = colorQuantile("Greens", df$PhysiciansPer, n = 10))
    
    mapStates$income = df$Per.Capita.Income[match(sta.fips, df$FIPS)]
    mapStates$population = df$Population.Estimate[match(sta.fips, df$FIPS)]
    mapStates$hospitals = df$Total.Number.Hospitals[match(sta.fips, df$FIPS)]
    mapStates$beds = df$Total.Hospital.Beds[match(sta.fips, df$FIPS)]
    mapStates$physicians = df$Physicians[match(sta.fips, df$FIPS)]
    mapStates$hospitalsPer = df$HospitalsPer[match(sta.fips, df$FIPS)]
    mapStates$bedsPer = df$BedsPer[match(sta.fips, df$FIPS)]
    mapStates$physiciansPer = df$PhysiciansPer[match(sta.fips, df$FIPS)]
    mapStates$populationDensity = df$Population.per.Square.Mile[match(sta.fips, df$FIPS)]
    
    colorVar <- switch (input$category,
                        "1" = ~qpal(mapStates$income),
                        "2" = ~qpal(mapStates$populationDensity),
                        "3" = ~qpal(mapStates$hospitalsPer),
                        "4" = ~qpal(mapStates$bedsPer),
                        "5" = ~qpal(mapStates$physiciansPer))
    colorValue <- switch(input$category,
                         "1" = ~mapStates$income,
                         "2" = ~mapStates$populationDensity,
                         "3" = ~mapStates$hospitalsPer,
                         "4" = ~mapStates$bedsPer,
                         "5" = ~mapStates$physiciansPer)
    
    leaflet(mapStates) %>%
      addProviderTiles("Stamen.TonerLite",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(lng = mapStates$x, 
                  lat = mapStates$y, 
                  color = colorVar,
                  smoothFactor = 0.2, fillOpacity = 0.75,
                  #fillColor = stateColors,
                  stroke = FALSE) %>%
      addLegend("bottomright", pal = qpal, values = colorValue, title = "",
                opacity = 1)
  })
  
  showStatePopup <- function(selectedState, lat, lng) {
    df <- yearData()
    idx <- which(mapStates$names==selectedState)
    hIdx <- which(as.character(df$`State.Abbrev`) == mapStates$abb[idx])
    imgFile <- paste(mapStates$abb[idx], ".jpg", sep="")
    if (mapStates$abb[idx] == 'CO') {
      imgFile <- "CO.gif"
    }
    if(input$category == 1)
      content <- as.character(tagList(
        tags$table(tags$tr(tags$td(tags$img(src=imgFile,height=72,width=72)),
                           tags$td(style="padding:2px", tags$h4(df$`State.Name`[hIdx]), tags$p(tags$b("Per Capita Income:"), 
                                                                                               sprintf(" %s", dollar(df$Per.Capita.Income[hIdx]))))))
      ))
    else if(input$category == 2)
      content <- as.character(tagList(
        tags$table(tags$tr(tags$td(tags$img(src=imgFile,height=72,width=72)),
                           tags$td(style="padding:2px", tags$h4(df$`State.Name`[hIdx]), tags$p(tags$b("Population Per Square Mile:"), 
                                                                                               sprintf(" %s",format(df$Population.per.Square.Mile[hIdx], big.mark=",", scientific=FALSE) )))))
      ))
    else if(input$category == 3)
      content <- as.character(tagList(
        tags$table(tags$tr(tags$td(tags$img(src=imgFile,height=72,width=72)),
                           tags$td(style="padding:2px", tags$h4(df$`State.Name`[hIdx]), tags$p(tags$b("Hospitals Per 1000 people:"), 
                                                                                               sprintf(" %.3f", df$HospitalsPer[hIdx])))))
      ))
    else if(input$category == 4)
      content <- as.character(tagList(
        tags$table(tags$tr(tags$td(tags$img(src=imgFile,height=72,width=72)),
                           tags$td(style="padding:2px", tags$h4(df$`State.Name`[hIdx]), tags$p(tags$b("Hospital Beds Per 1000 people:"), 
                                                                                               sprintf(" %.3f", df$BedsPer[hIdx])))))
      ))
    else if(input$category == 5)
      content <- as.character(tagList(
        tags$table(tags$tr(tags$td(tags$img(src=imgFile,height=72,width=72)),
                           tags$td(style="padding:2px", tags$h4(df$`State.Name`[hIdx]), tags$p(tags$b("Primary Care Physicians Per 1000 people:"), 
                                                                                               sprintf(" %.3f", df$PhysiciansPer[hIdx])))))
      ))
    leafletProxy("map") %>% addPopups(lng, lat, content)
  }   
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if(is.null(event))
      return()       
    lat <- event$lat
    lon <- event$lng
    coords <- as.data.frame(cbind(lon, lat))
    point <- SpatialPoints(coords)
    mapStates_sp <- map2SpatialPolygons(mapStates, IDs = mapStates$names)
    
    i <- point [mapStates_sp, ]
    selected <- mapStates_sp [i]
    idx <- over(i, mapStates_sp)
    stateNames <- sapply(mapStates_sp@polygons, function(x) x@ID)
    isolate({
      showStatePopup(stateNames[idx], lat, lon)
    })
  })
  
  output$bubbleChart <- renderGvis({
    Sys.sleep(0.3)
    if (input$category != 1 && input$category != 2) {
      gvisBubbleChart(yearData(), idvar="State.Name",xvar=xVar(),
                      yvar="Per.Capita.Income",colorvar="Region",
                      sizevar="Population.per.Square.Mile",
                      options = list(
                        width="100%", height = "400px",
                        #fontName = "Source Sans Pro",
                        fontSize = 13,
                        # Set axis labels and ranges
                        hAxis = switch(input$category,
                                       "3" = '{title: "Hospitals (Per 1000 People)"}',
                                       "4" = '{title: "Hospital Bed (Per 1000 People)"}',
                                       "5" = '{title: "Primary Care Physicians (Per 1000 People)"}'),
                        vAxis = '{
                        title: "Income, per capita ($USD)",
                        minValue: 30000, maxValue: 80000
    }',
                        # The default padding is a little too spaced out
                        chartArea = '{
                        top: 40, left: 100, bottom: 10,
                        height: "75%", width: "75%"
  }',
                        # Allow pan/zoom
                        explorer = list(),
                        # Set bubble visual props
                        bubble = '{
                        opacity: 0.4, stroke: "none",
                        label: "",
                        textStyle: "{color: none}"
  }'
                      ))
  }
    })
  })

# Run the application 
shinyApp(ui = ui, server = server)


####
#### Part 2: Analysis of sample states
####
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(maps)
library(maptools)
library(sp)
library(rgeos)
library(googleVis)

require(igraph)
require(googleVis)
library(corrplot)

HealthData <- read.csv("chydata.csv", header = TRUE)
student <- read.csv("student.csv", head = TRUE)
HealthData$DoctorPer <- HealthData$DoctorNumber * 10000 / HealthData$HospitalAdmission
HealthData$HospitalPer <- HealthData$HospitalNumber * 1000000 / HealthData$HospitalAdmission
HealthData$BedPer <- HealthData$BedNumber * 10000 / HealthData$HospitalAdmission
HealthData$Student <- student$Students
HealthData$StudentPer <- HealthData$Student * 10000 / HealthData$HospitalAdmission

HealthNumeric <- subset(HealthData, select = c(HospitalAdmission, Population, ExpenseonNumber, Income,DoctorPer, HospitalPer,  BedPer, StudentPer ))
colnames(HealthNumeric)[3] <- "Expense"

H <- cor(HealthNumeric)
corrplot(H, method = "circle", title = "Correlation between Different Aspects")

GaugeIncome <- subset(HealthNumeric, select = c(State, Income))

Gauge1 <-  gvisGauge(GaugeIncome, 
                     options=list(min=40000, max=55000, greenFrom=40000,
                                  greenTo=45000, yellowFrom=45000, yellowTo=50000,
                                  redFrom=50000, redTo=55000, width=500, height=400))
plot(Gauge1)

HealthNumeric$State <- HealthData$State
HealthAdmissionPie <- subset(HealthNumeric, select = c(State, HospitalAdmission))

Pie3 <- gvisPieChart(HealthAdmissionPie, options=list(
  width=1000,
  height=1000,
  slices="{2: {offset: 0}, 0: {offset: 0}}",
  title='Hospital Admissions for Selected States',
  #legend='none',
  pieSliceText='label',
  pieHole=0.5))
plot(Pie3)


####
#### Part 3: Race and Beds Distribution
####
library('ggplot2')
library('plm')
dfph <- read.csv("data_dcf.csv")
fb2010 <- apply(dfph[,86:88], 1, sum)
mb2010 <- apply(dfph[,83:85], 1, sum)
fw2010 <- apply(dfph[,80:82], 1, sum)
mw2010 <- apply(dfph[,77:79], 1, sum)
population <- dfph[,51:63]
population <- as.vector(as.matrix(population))
num_bed <- dfph[,64:76]
num_bed <- as.vector(as.matrix(num_bed))
bed_per_pop <- 1000*num_bed/population
num_fb <- cbind(dfph[39:40], fb2010, dfph[41:50])
prop_fb <- as.vector(as.matrix(num_fb))/population
num_mb <- cbind(dfph[27:28], mb2010, dfph[29:38])
prop_mb <- as.vector(as.matrix(num_mb))/population
num_fw <- cbind(dfph[15:16], fw2010, dfph[17:26])
prop_fw <- as.vector(as.matrix(num_fw))/population
num_mw <- cbind(dfph[3:4], mw2010, dfph[5:14])
prop_mw <- as.vector(as.matrix(num_mw))/population
states <- dfph[,2]
states <- rep(as.vector(states),13)
years <- sort(rep(c(2000:2012),48), decreasing = TRUE)
df <- cbind(years, bed_per_pop, prop_fb, prop_mb, prop_fw, prop_mw)
df <- as.data.frame(df)
df$states <- states
plm <- plm(bed_per_pop ~ prop_fb + prop_mb + prop_fw + prop_mw, data=df,index = c("states","years"), effect="time")
df$fitted <- df$bed_per_pop - plm$residuals
p1 <- ggplot(df, aes(x=log(prop_mb), y=bed_per_pop)) + geom_point(col = "orange")
p1 + facet_wrap(~years, nrow = 3) + stat_smooth(aes(colour="Local Polynomial"), se = FALSE) + stat_smooth(aes(colour="Linear Regression"), method = 'lm', se = TRUE) + scale_colour_discrete("") + xlab("Logarithm Proportion of Blcak Male over 65") + ylab("Beds per 1000 Persons") + labs(title = "Black Man vs Beds per Person") + theme(legend.position = c(0.8, 0.2), legend.text = element_text(colour="black", size=6),
                                                                                                                                                                                                                                                                                                                                             axis.text = element_text(size=6), axis.title = element_text(size=6),
                                                                                                                                                                                                                                                                                                                                             plot.title =element_text(size=8) )

p2 <- ggplot(df, aes(x=log(prop_mw), y=bed_per_pop)) + geom_point(col = "orange")
p2 + facet_wrap(~years, nrow = 3) + stat_smooth(aes(colour="Local Polynomial"), se = FALSE) + stat_smooth(aes(colour="Linear Regression"), method = 'lm', se = TRUE) + scale_colour_discrete("") + xlab("Logarithm Proportion of White Male over 65") + ylab("Beds per 1000 Persons") + labs(title = "White Man vs Beds per Person") + theme(legend.position = c(0.8, 0.2), legend.text = element_text(colour="black", size=6),
                                                                                                                                                                                                                                                                                                                                             axis.text = element_text(size=6), axis.title = element_text(size=6), 
                                                                                                                                                                                                                                                                                                                                             plot.title =element_text(size=8) )

p3 <- ggplot(df, aes(x=fitted, y=bed_per_pop)) + geom_point(aes(colour = " bed_per_pop = \n 660*prop_fb - \n 975*prob_mb + \n 19*prob_fw + \n 33*prob_mw + \n fixed effects\n F = 47.01, p < 0.0001"))
p3 + facet_wrap(~years, nrow = 3) + xlab("Fitted Value of Beds per 1000 Persons ") + ylab("Beds per 1000 Persons") + labs(title = "Panel Linear Regression") +
  theme(legend.position = c(0.82, 0.15), 
        legend.text = element_text(colour="black", size=6,face="bold.italic"),
        axis.text = element_text(size=6), axis.title = element_text(size=6),
        plot.title =element_text(size=8),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue")) + scale_colour_discrete("") + 
  guides(colour = guide_legend(override.aes = list(shape = NA)))


####
#### Part 4: Hospitals and Patient admissions
####
library(ggplot2)
library(reshape)

# number of hospitals in 2007 vs 2009 i.e. before and after market crash
f <- read.csv("hospital-bed-population-cleaned.csv")
df1 = melt(data.frame(A=f$Total.Number.Hospitals..2007., B=f$Total.Number.Hospitals..2009., 
                      State=f$State),
           variable_name="Year")

p1 <- ggplot(df1, aes(State, value, fill=Year)) + 
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Year",
                      labels=c("2007", "2009"))+
  xlab("State")+ylab("Number of Hospitals")

p1 <- p1+theme(axis.text.x=element_text(size=6,angle = -60, hjust = -0.5),
               axis.title=element_text(size=6),
               axis.text.y=element_text(size=6),
               legend.text=element_text(size=6),
               legend.title=element_text(size=6))

# compare number of hospital admissions in 2007 vs 2009
df2 = melt(data.frame(A=f$Tot.Hospital.Admissions..2007., B=f$Tot.Hospital.Admissions..2009., 
                      State=f$State),
           variable_name="Year")

p2 <- ggplot(df2, aes(State, value, fill=Year)) + 
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Year",
                      labels=c("2007", "2009"))+
  xlab("State")+ylab("Number of admissions")

p2 <- p2+theme(axis.text.x=element_text(size=6,angle = -60,hjust = -0.5),
               axis.title=element_text(size=6),
               axis.text.y=element_text(size=6),
               legend.text=element_text(size=6),
               legend.title=element_text(size=6))

# plot both plots in one
require(grid)
library(gridExtra)
grid.arrange(p1, p2, ncol=1, top=textGrob("Effect of 2008 market crash on Hospitals and Patient admissions", gp=gpar(fontsize=8, font=3)))


####
#### Part 5:
####

library(chron)
library(reshape2)
library(dplyr)
library(corrplot)

# Make the data tidy
makeTidyData <- function(raw_data)
{
  
  tData <- data.frame(
    "StateName" = raw_data[,3], # non-numeric
    
    "TotalPopulation2012" = as.numeric(raw_data[,4]),
    "TotalHospitalAdmission2012" = as.numeric(raw_data[,5]), # numeric, NAs introduced by coercion
    "TotalHospitalAdmission2011" = as.numeric(raw_data[,6]), # numeric, NAs introduced by coercion
    "TotalHospitalAdmission2010" = as.numeric(raw_data[,7]), # numeric, NAs introduced by coercion
    "TotalHospitalAdmission2009" = as.numeric(raw_data[,8]),# numeric, NAs introduced by coercion
    "TotalHospitalAdmission2008" = as.numeric(raw_data[,9]),# numeric, NAs introduced by coercion
    "TotalHospitalAdmission2007" = as.numeric(raw_data[,10]), # numeric, NAs introduced by 
    "TotalHospital2012" = as.numeric(raw_data[,11]), # numeric, NAs introduced by coercion
    "TotalHospital2011" = as.numeric(raw_data[,12]), # numeric, NAs introduced by coercion
    "TotalHospital2010" = as.numeric(raw_data[,13]), # numeric, NAs introduced by coercion
    "TotalHospital2009" = as.numeric(raw_data[,14]), # numeric, NAs introduced by coercion
    "TotalHospital2008" = as.numeric(raw_data[,15]), # numeric, NAs introduced by coercion
    "TotalHospital2007" = as.numeric(raw_data[,16]), # numeric, NAs introduced by coercion
    "TotalBeds2012" = as.numeric(raw_data[,17]), # numeric, NAs introduced by coercion
    "TotalBeds2011" = as.numeric(raw_data[,18]), # numeric, NAs introduced by coercion
    "TotalBeds2010" = as.numeric(raw_data[,19]), # numeric, NAs introduced by coercion
    "TotalBeds2009" = as.numeric(raw_data[,20]), # numeric, NAs introduced by coercion
    "TotalBeds2008" = as.numeric(raw_data[,21]), # numeric, NAs introduced by coercion
    "TotalBeds2007" = as.numeric(raw_data[,22]), # numeric, NAs introduced by coercion
    "TotalPopulation2012" = as.numeric(raw_data[,24]),
    "TotalPopulation2011" = as.numeric(raw_data[,25]),
    "TotalPopulation2010" = as.numeric(raw_data[,26]),
    "TotalPopulation2009" = as.numeric(raw_data[,27]),
    "TotalPopulation2008" = as.numeric(raw_data[,28]),
    "TotalPopulation2007" = as.numeric(raw_data[,29]),
    "TotalMaleLess15" = as.numeric(raw_data[,30]),
    "TotalMale15To19" = as.numeric(raw_data[,31]),
    "TotalMale20To24" = as.numeric(raw_data[,32]),
    "TotalMale25To44" = as.numeric(raw_data[,33]),
    "TotalMale45To64" = as.numeric(raw_data[,34]),
    "TotalMale65Plus" = as.numeric(raw_data[,35]),
    "TotalFeMaleLess15" = as.numeric(raw_data[,36]),
    "TotalFeMale15To19" = as.numeric(raw_data[,37]),
    "TotalFeMale20To24" = as.numeric(raw_data[,38]),
    "TotalFeMale25To44" = as.numeric(raw_data[,39]),
    "TotalFeMale45To64" = as.numeric(raw_data[,40]),
    "TotalFeMale65Plus" = as.numeric(raw_data[,41])
    
  )
  
  return(tData)
}

fname <- "Hospitals-Bed-Population-2007-2012.csv"
raw_data <- read.csv(fname)
tidy_data <- makeTidyData(raw_data)

tidy_data$TotalPopulationLess15 <- tidy_data$TotalMaleLess15 + tidy_data$TotalFeMaleLess15
tidy_data$TotalPopulation15To19 <- tidy_data$TotalMale15To19 + tidy_data$TotalFeMale15To19
tidy_data$TotalPopulation20To24 <- tidy_data$TotalMale20To24 + tidy_data$TotalFeMale20To24
tidy_data$TotalPopulation25To44 <- tidy_data$TotalMale25To44 + tidy_data$TotalFeMale25To44
tidy_data$TotalPopulation45To64 <- tidy_data$TotalMale45To64 + tidy_data$TotalFeMale45To64
tidy_data$TotalPopulation65Plus <- tidy_data$TotalMale65Plus + tidy_data$TotalFeMale65Plus
tidy_data$AvgHostpitalAdmission=rowMeans(tidy_data[,c("TotalHospitalAdmission2012", "TotalHospitalAdmission2011","TotalHospitalAdmission2010","TotalHospitalAdmission2009","TotalHospitalAdmission2008","TotalHospitalAdmission2007")], na.rm=TRUE)
tidy_data$AvgHostpitalBed=rowMeans(tidy_data[,c("TotalBeds2012", "TotalBeds2011","TotalBeds2010","TotalBeds2009","TotalBeds2008","TotalBeds2007")], na.rm=TRUE)
tidy_data$AvgHostpitals =rowMeans(tidy_data[,c("TotalHospital2012", "TotalHospital2011","TotalHospital2010","TotalHospital2009","TotalHospital2008","TotalHospital2007")], na.rm=TRUE)


dataForPCA <- subset(tidy_data, select=c(AvgHostpitalAdmission,TotalPopulationLess15,TotalPopulation15To19,TotalPopulation20To24,TotalPopulation25To44,TotalPopulation45To64,TotalPopulation65Plus,TotalBeds2012,TotalPopulation2012))

corr <- cor(dataForPCA, use="complete.obs")
corrplot(corr, method="pie",type="lower")


library(chron)
library(reshape2)
library(circlize)
library(dplyr)

# Make the data Tidy
makeTidyData2 <- function(raw_data)
{
  
  tData <- data.frame(
    "StateName" = raw_data[,3], # non-numeric
    
    "TotalPopulation2012" = as.double(raw_data[,4]),
    "TotalHospitalAdmission2012" = as.double(raw_data[,5]), # numeric, NAs introduced by coercion
    "TotalHospital2012" = as.double(raw_data[,11]), # numeric, NAs introduced by coercion
    "TotalBeds2012" = as.double(raw_data[,17]) # numeric, NAs introduced by coercion
  )
  
  return(tData)
}

fname <- "Hospitals-Bed-Population-2007-2012.csv"
raw_data <- read.csv(fname)
tidy_data <- makeTidyData2(raw_data)

# Get the top 10 states by population.
top10Data <- head(tidy_data[order(tidy_data$TotalPopulation2012, decreasing=TRUE), ], 10)

rownames(top10Data) <- top10Data$StateName
top10Data$StateName <- NULL
df = data.frame(from = rep(rownames(top10Data), times = ncol(top10Data)),
                to = rep(colnames(top10Data), each = nrow(top10Data)),
                value = as.vector(top10Data),
                stringsAsFactors = FALSE)
chordDiagram(df)
circos.clear()


####
#### Part 6:
####
library(ggplot2)
library(scatterplot3d)
library(googleVis)
library(RColorBrewer)
library(dplyr)
library(plotly)
library(reshape2)
library(maps)
library(choroplethr)
library(choroplethrMaps)

raw_dat = read.csv("Data.csv")
gen_dat = read.csv("GeneralData.csv")

new_names <- c("num_STG", "num_STNG", "num_VET", "num_STG_pay", "num_STNG_pay", "num_STG_pay_exp", "num_STNG_pay_exp", "num_STG_exp", "num_STNG_exp", "num_VET_exp")
gen_names <- c("Population")
years <- as.vector(seq(1995,2012,by=1))

for (i in years) {
  indx <- grepl(i,colnames(raw_dat))
  n = (dim(raw_dat[indx])[2]/2)+1
  temp <- raw_dat[indx]
  names(temp) <- new_names
  temp$FIPS = raw_dat$FIPS
  temp$Year = i
  if (i==1995) {
    cln_dat <- temp
  } else {
    print(i)
    cln_dat <- rbind(cln_dat,temp)
  }
}

# match fips with state and abbreviation name
statefips = read.csv("fips.csv")
cln_dat$abb <- as.character(statefips$abb[match(statefips$FIPS, cln_dat$FIPS/1000)])
cln_dat$state <- tolower(as.character(statefips$state[match(statefips$FIPS, cln_dat$FIPS/1000)]))
cln_dat <- mutate(cln_dat, total_exp = num_STG_pay_exp + num_STNG_pay_exp + num_STG_exp + num_STNG_exp + num_VET_exp, 
                  total_hosp = num_STG_pay + num_STNG_pay + num_STG + num_STNG + num_VET)

# attach general data

for (i in years) {
  indx <- which(grepl(i,colnames(gen_dat)))
  names(gen_dat)[indx] <- i
}

gen_temp <- melt(gen_dat, id.vars = "FIPS")
gen_temp <- gen_temp[order(gen_temp[,2], decreasing=T),]
gen_temp <- gen_temp %>% rename(Year = variable, Pop = value)
cln_dat <- cbind(cln_dat,gen_temp$Pop)
names(cln_dat)[16] <- "Pop"

## Next match fips with state abbreviation and name

usa_map <- map_data("state")

cln_dat <- mutate(cln_dat, pop_per_cap = (total_exp/Pop))

#Define region and value before rendering

j=1
for (i in years) {
  mapdat <- cln_dat %>% filter(Year==i) %>% mutate(region = state, value = (pop_per_cap*1000))
  
  choro = StateChoropleth$new(mapdat)
  choro$title = paste(i,"Health Expenditures per Capita")
  choro$ggplot_scale = scale_fill_brewer(name="Spending per capita",palette="YlOrRd", drop=F)
  
  anims[[j]] <- choro$render()
  j=j+1
}

choroplethr_animate(anims)

yeardat <- filter(cln_dat, Year == 1995)
p <- ggplot(yeardat, aes(x=total_hosp, y=pop_per_cap, size=Pop)) +
  geom_point()
p


