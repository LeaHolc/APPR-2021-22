library(shiny)

shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(position = "left",
                sidebarPanel(
                  selectInput(
                    "vrsta",
                    label = "Stopnja izobrazbe:",
                    choices = c("OŠ ali manj", "srednje poklicno izobraževanje", "strokovno, splošno izobraževanje",
                                "izobraževanje prve, druge, tretje stopnje", "Skupaj"),
                    selected = "Skupaj"
                  ),
                  selectInput(
                    "obcina.vnos",
                    label = "Občina:",
                    choices = brezposelnost.shiny$obcina,
                    selected = "LJUBLJANA"
                  ))
                ,
                mainPanel(plotOutput("graf"))),
  uiOutput("izborTabPanel")))