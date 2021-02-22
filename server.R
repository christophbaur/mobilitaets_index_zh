source("fct_plot_verk_aufk.R", local = TRUE)
source("fct_plot_google_ind.R", local = TRUE)
source("fct_karte.R", local = TRUE)

#laden der Daten
mobility_index <- read.csv("data/mobility_index.csv")%>%
  mutate(Date=as.Date(Date))

geo_info <- read.csv("data/geo_info.csv")

 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$verk_aufk <- renderPlotly({
    
    fct_plot_verk_aufk(data=mobility_index)
    
  })
  

  output$google_ind <- renderPlotly({

    fct_plot_google_ind(data=mobility_index,
                        google_ind=input$google_sel)

  })


  output$karte <- renderLeaflet({
    fct_karte(data=geo_info)
  })
  
})
