#Visualisierung
fct_plot_google_ind <- function(data, google_ind){
  
  
  #Daten laden
  # mobility_index <- read.csv("mobility_index.csv")%>%
  #   mutate(Date=as.Date(Date))
  
  mobility_index <- data
  
  #Google Indikator
  #google_ind='Arbeitsplätze'
  
  #Übersetzung Google Indikator User-Input/columnname
  google_uebersetzung <- tibble(columnname=c("mobility_retail_and_recreation",
                                       "mobility_grocery_and_pharmacy",
                                       "mobility_transit_stations",
                                       "mobility_workplaces",
                                       "mobility_residential"),
                                user_input=c("Freizeit/Einkauf",
                                       "Läden des täglichen Bedarfs",
                                       "Bahnhöfe und Haltestellen",
                                       "Arbeitsplätze",
                                       "Wohnorte"))
  google_uebersetzung <-filter(google_uebersetzung,user_input==google_ind)$columnname
  
  
  #gewählter Google-Indikator auf spezielle Spalte schreiben
  
  google_sel <- mobility_index%>%
    select(Date, contains(as.character(google_uebersetzung)))
  #Spalte allgemeingültig benennen für einfachere Auswahl im Plot
  colnames(google_sel)[2] <- "google_index"
  
  
  # Code teilweise übernommen von
  # https://github.com/ropensci/plotly/issues/954
  ## Create first plot, 'P_A' with trace 'A'
  A_Axis <- list(side = "left", title = "Indexwert",
                 tickformat = "%",
                 range=c(0,2.99)
                 #zeroline = TRUE,
                 #showline = TRUE
  )
  
  fig1 <- plot_ly(mobility_index,
                  x=~Date
  )%>%
    add_lines(y=~avg_7day_index_miv,
              mode='scatter',
              name='MIV')%>%
    add_lines(y=~avg_7day_index_velo,
              mode='scatter',
              name='Velo')%>%
    add_lines(y=~avg_7day_index_fuss,
              mode='scatter',
              name='Fuss')%>%
    add_lines(y=~avg_7day_index_oev,
              mode='scatter',
              name='öV (VBZ Hardbr.)')%>%
    
    #1. Lockdown
    add_annotations(x = as.Date('2020-03-15')+1,
                    y = 2,
                    text = '<b>Lockdown</b>',
                    textangle = -90,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE,
                    #arrowhead = 4,
                    #arrowsize = .5,
                    #ax = -20,
                    #ay = -40,
                    opacity=0.5,
                    font = list(size=9)
    )%>%
    #2. Lockdown
    add_annotations(x = as.Date('2021-01-18')+1,
                    y = 2,
                    text = '<b>Lockdown</b>',
                    textangle = -90,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE,
                    #arrowhead = 4,
                    #arrowsize = .5,
                    #ax = -20,
                    #ay = -40,
                    opacity=0.5,
                    font = list(size=9)
    )%>%
    #'Freedome-day'
    add_annotations(x = as.Date('2022-04-01'),
                    y = 0,
                    text ='<b>Aufheben aller Massnahmen</b>',
                    #textangle = -90,
                    xref = "x",
                    yref = "y",
                    ax = 0,
                    ay = -225,
                    showarrow = TRUE,
                    arrowhead  = 7,
                    opacity=0.5,
                    font = list(size=9))%>%
    layout(
      #title = "Verkehrsaufkommen, indexiert",
      yaxis = A_Axis,
      xaxis = list(
        zeroline=TRUE,
        showline=TRUE,
        title='',
        ticks = "outside",
        showspikes = TRUE,
        spikemode= 'across+toaxis+marker',
        spikesnap= 'hovered data',
        spikedash = 'solid',
        spikethickness = 0.5,
        spikecolor = 'gray',
        rangeselector = list(
          buttons = list(
            list(
              count = 3,
              label = "letzte 3 Monate",
              step = "month",
              stepmode = "backward"),
            list(
              count = 6,
              label = "letzte 6 Monate",
              step = "month",
              stepmode = "backward"),
            list(
              count = 1,
              label = "letztes Jahr",
              step = "year",
              stepmode = "backward"),
            list(step = "all",
                 label = "alles"))),
        
        rangeslider = list(type = "date",
                           thickness = 0.05),
        hovermode='x unified',
        hoverlabel=list(bgcolor="white"),
        legend=list(orientation='h',
                    xanchor = 'center',
                    yanchor = 'top',
                    y=-0.05,
                    x=0.5)),
      shapes = list(
        #Erster Lockdown
        list(type= 'rect',
             layer = 'below',
             fillcolor = "#73daff",
             line = list(color = "#73daff"),
             opacity = 0.2,
             x0 = as.Date('2020-03-15'),
             x1 = as.Date('2020-05-11'),
             xref = "x",
             y0 = 0, 
             y1 = 2.5, 
             yref = "y"),
        #zweiter Lockdown
        list(type= 'rect',
             layer = 'below',
             fillcolor = "#73daff",
             line = list(color = "#73daff"),
             opacity = 0.2,
             x0 = as.Date('2021-01-18'),
             x1 = as.Date('2021-02-28'),
             xref = "x",
             y0 = 0, 
             y1 = 2.5, 
             yref = "y")))
  
  #fig1
  
  B_Axis <- list(side = "left", title = "Indexwert",
                 tickformat = "%",
                 range=c(0,1.49)#,
                 #zeroline = TRUE,
                 #showline = TRUE
  )
  
  fig2 <- plot_ly(google_sel,
                  x=~Date)%>%
    add_lines(y=~google_index,
              name=google_ind,
              mode = 'lines', 
              fill = 'tozeroy',
              line=list(color='#59c4e6'),
              fillcolor=list(color='#a5e7f0',
                             opacity=0.5))%>%
    layout(yaxis=B_Axis)
  
  
  
  fig <- subplot( fig1, fig2,
                  nrows = 2,
                  shareX = TRUE,
                  titleY = TRUE,
                  heights = c(0.6, 0.2))%>%
    
    layout(annotations = list(
      list(x = 0.0 , y = 0.90,
           text = "<b>Aufkommen indexiert nach Verkehrsart</b>",
           showarrow = F,
           xref='paper', yref='paper',
           font=list(size=12,
                     color='black')),
      list(x = 0.0 , y = 0.25,
           text = paste0("<b>Bewegungsmuster ",google_ind, "</b>"),
           showarrow = F,
           xref='paper', yref='paper',
           font=list(size=12,
                     color='black'))
    ),
    xaxis=list(title='',
               ticks = "outside",
               showspikes = TRUE,
               spikemode= 'across+toaxis+marker',
               spikesnap= 'hovered data',
               spikedash = 'solid',
               spikethickness = 0.5,
               spikecolor = 'gray'),
    hovermode='x unified',
    hoverlabel=list(bgcolor="white"),
    legend=list(orientation='h',
                xanchor = 'center',
                yanchor = 'top',
                y=-0.05,
                x=0.5),
    margin=list(r='70', t='0', b='80'),
    height="600"
    
    
    )%>%
    config(displaylogo = FALSE,
           displayModeBar=TRUE,
           
           
           modeBarButtonsToRemove = c(
             'sendDataToCloud', 'autoScale2d',
             'pan2d','select2d','lasso2d',
             'toggleSpikelines', "zoomIn2d", "zoomOut2d", 'zoom2d'
           ))
  
  return(fig)
  
}