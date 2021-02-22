library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(readr)
library(sf)
library(leaflet)
library(dplyr)
library(plotly)

options(encoding = "UTF-8")

#texte sourcen
source("text.R", local=TRUE)



dashboardPage(
  dashboardHeader(title = "Mobilitätsindex ZH"),
  dashboardSidebar(
    
    HTML("<br> <p> &nbsp; &nbsp; Verkehrsaufkommen und <br> &nbsp; &nbsp; Mobilitätsverhalten <br> &nbsp; &nbsp; im Kanton Zürich </p>"),
    #HTML("<br> <p> &nbsp; &nbsp; Wöchentliche Aktualisierung </p>"),
    
    hr(),
    
    HTML("<p><big> &nbsp; &nbsp; Inhalt </big></p>"),
    HTML("<p> &nbsp; &nbsp; Was ist der Mobilitätsindex ZH?</p>"),
    HTML("<p> &nbsp; &nbsp; Verkehrsaufkommen</p>"),
    HTML("<p> &nbsp; &nbsp; Wo wurde gemessen?</p>"),
    
    
    
    hr(),
    HTML("<p><big> &nbsp; &nbsp; Links </big></p>"),
    sidebarMenu(
      menuItem("Code/Methodik", 
               icon = icon("file-code"), 
               href = "https://github.com/christophbaur/mobilitaets_index_zh"),
      menuItem("Github", 
               icon = icon("github"), 
               href = "https://github.com/christophbaur"),
      menuItem("LinkedIn", 
               icon = icon("linkedin-in"), 
               href = "https://www.linkedin.com/in/christoph-baur-89759a202/")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
                              
                              /* body */
                              .content-wrapper, .right-side {
                              background-color: #FFFFFF;
                              }
                              
                              '))),

    fluidPage(
          fluidRow(
            #um beim normalen Browsern die Breite zu begrenzen column eingefügt
            column(
              width=8, offset = 2, align="left",
            
            h1("Was ist der Mobilitätsindex ZH?",align = "left"),
            
            p(text_einleitung),
            
            #extra space
            h1(HTML("<br>")),
            
            h1("Veränderung des Verkehrsaufkommens",align = "left"),
            p(text_verk_allg),
            
            h2("Mit Wetterinfo"),
            
            plotlyOutput("verk_aufk",
                         height="100%"),
            
            p(text_verk_wetter),
            
            h2("Mit Veränderung der Bewegungsmuster",align = "left"),
            
            selectInput("google_sel",
                        label="Google Indikator",
                        choices = c("Freizeit/Einkauf",
                                    "Läden des täglichen Bedarfs",
                                    "Bahnhöfe und Haltestellen",
                                    "Arbeitsplätze",
                                    "Wohnorte")),
            
            plotlyOutput("google_ind",
                         height="100%"),
            
            p(text_google_ind),
            
            h1(HTML("<br>")),
            
            h1("Wo wurde gemessen?"),
            
            p(text_karte),
            
            leafletOutput("karte"),
            
            h1(HTML("<br>")),
            
            hr(),
            
            p(text_fusszeile)
            
            )#end column
            
        )
      )
    
    
    
  )
)