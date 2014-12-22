source("global.r")

shinyUI(navbarPage("Network Assessment",
#titlePanel("Network Assessment"),
tabPanel("Compare daily data from 2 sites",
sidebarLayout(
sidebarPanel(
selectInput("parmsel", label = h3("Select Pollutant"),choices = as.list(unique(daily.data$parm))),
uiOutput("tssitesel1"),
uiOutput("tspocsel1"),
uiOutput("tssitesel2"),
uiOutput("tspocsel2"),
uiOutput("tsyearsel")
),
mainPanel(h5("The data plotted below are the 8-hour daily max ozone (parameter code 44201) and 24-hour mean PM2.5 (parameter code 88101) concentrations from the EPA AQS data system.  Select a pollutant and 2 sites from the dropdown menus to the left to view and compare the selected sites' data." ),
plotOutput('timeserplot',height=500),
plotOutput('scatplot',height=500)
)
)
),

tabPanel("100 Km Neighbor Correlation Maps",
sidebarLayout(
sidebarPanel(
selectInput("parmsel2", label = h3("Select Pollutant"),choices = as.list(unique(intersite.cors$parm))),
uiOutput("corsitesel"),
uiOutput("coryearsel"),
selectInput("additdatasel", label = h3("Overlay other data?"),choices = as.list(c('None'='None','2010 Population'='population','2010 DS 4th Max O3'='O3Max4','2010 DS Mean PM2.5'='PM25Mean')))
),
mainPanel(
plotOutput('cormaps',height=1100)
)
)
)
))

