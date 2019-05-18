# ---------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------- #
# -------------------------------- Server Side ------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------- #

server <- function(session, input, output) {
  gm <- read.csv2("data/gapminder.csv")
  
  # -------------------------------------------------- #
  # -------------------------------------------------- #
  # -------------------First Tab --------------------- #
  # -------------------------------------------------- #
  # -------------------------------------------------- #
  

  # ----------------- #
  # ---Bubble Plot--- #
  # ----------------- #
  p <- eventReactive(input$action_1,
                     {gm %>% 
                         filter(Year>input$Year & country %in% input$countries)
                       },
                     ignoreNULL = FALSE)
      
  output$bubble <- renderPlotly({

    plot_ly(data = p(),
                 x = ~GDP_per_capita, 
                 y = ~life_expectancy,
                 color = ~continent,
                 colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'),
                 frame = ~Year,
                 type = "scatter",
                 mode = "markers",
                 size = ~population,
                 sizes = c(100,3000),
                 marker = list(symbol = 'circle',
                               line = list(color = '#FFFFFF')),
                 width = 900, height = 500,
                 hoverinfo = "text",
            text = ~paste("</br>Country: ", country,
                          "</br>GDP/person: ", format(paste("$",comma(GDP_per_capita)), digits = 2, format = "f"),
                          "</br>Life Exp: ", life_expectancy, "years",
                          "</br>Population: ", population)) %>% 
      layout(autosize = F, 
             title = ~paste("Life Expectancy Versus GDP (year:", Year, ")"),
             xaxis = list(type = "log",
                          title = "GDP per Capita (log scale)",
                          tickprefix = "$"),
             yaxis = list(title = "Life Expectancy")) %>% 
      config(displayModeBar = FALSE) %>% 
      animation_opts(
        frame = 250
      ) %>% 
    animation_slider(
      currentvalue = list(prefix = "Year: ", font = list(color="#FA8072"))
    )
  })
  
  

  # -------------------------------------------------- #
  # -------------------------------------------------- #
  # ------------------Second Tab --------------------- #
  # -------------------------------------------------- #
  # -------------------------------------------------- #    
    

    # ----------------- #
    # ---Line Plot----- #
    # ----------------- #
  
  plot <- eventReactive(input$action_2,
                     {
                       if("Select All" %in% input$countries_2){
                        gm %>% 
                           filter(Year >= input$dates[1] & Year <= input$dates[2]) %>% 
                           select(input$line_yaxis, country, Year) %>% 
                           accumulate_by(~Year)
                       } else {
                        gm %>% 
                           filter(country %in% input$countries_2 & Year >= input$dates[1] & Year <= input$dates[2]) %>% 
                           select(input$line_yaxis, country, Year) %>% 
                           accumulate_by(~Year)
                       }
                         },
                     ignoreNULL = FALSE)
  
  
  
    output$line <- renderPlotly({
      
      plot_ly(data = plot(),
              x = ~Year,
              y = ~plot()[,1],
              frame = ~frame,
              type = "scatter",
              mode = "line",
              color = ~country,
              colors = "Spectral",
              height = 500,
              width = 900) %>% 
        layout(
          yaxis = list(
            title = ~names(variables[variables == names(plot())[1]])
          )
        ) %>% 
        animation_opts(
          frame = 250
        ) %>% 
        animation_slider(
          currentvalue = list(prefix = "Year: ", font = list(color="#FA8072"))
        ) %>% 
        config(displayModeBar = FALSE)
    })
  
    
 
    # -------------------------------------------------- #
    # -------------------------------------------------- #
    # ------------------ Third Tab -------------------- #
    # -------------------------------------------------- #
    # -------------------------------------------------- #    
    
  plot_custom <- eventReactive(input$action_3,
                               {
                                 gm %>% 
                                   filter(Year >= input$time[1] & Year <= input$time[2]) %>% 
                                   filter(country %in% input$countries_3) %>% 
                                   select(input$x, input$y, country, continent, Year, population) 
                               },
                               ignoreNULL = FALSE)
    
    output$custom <- renderPlotly({

      name_x <- names(variables[variables == names(plot_custom())[1]])
      name_y <- names(variables[variables == names(plot_custom())[2]])
      # creating layout
      if (names(plot_custom())[1] %in% c("GDP_per_capita", "health_spend")){
        x <- list(title = name_x,
                  type = "log",
                  tickprefix = "$")
        y <- list(title = name_y)

      } else if (names(plot_custom())[2] == c("GDP_per_capita","health_spend")) {
        x <- list(title = name_x)
        y <- list(title = name_y,
                  type = "log",
                  tickprefix = "$")
      } else {
        x <- list(title = name_x)
        y <- list(title = name_y)
      }

      text_input <- paste("</br>Country: ", plot_custom()[,3],
                          "</br>",name_x,":", plot_custom()[,1],
                          "</br>",name_y,":", plot_custom()[,2])
      
      plot_ly(data = plot_custom(),
              x = ~plot_custom()[,1],
              y = ~plot_custom()[,2],
              frame = ~Year,
              height = 500,
              width = 900,
              size = ~population,
              sizes = c(100,3000),
              color = ~continent,
              colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951'),
              type = "scatter",
              mode = "markers",
              hoverinfo = "text",
              text = ~text_input,
              marker = list(symbol = 'circle', 
                            line = list(color = '#FFFFFF'))) %>% 
        layout(
          xaxis = ~x,
          yaxis = ~y,
          title = ~paste(x$title, "vs. ", y$title, "from the year ", min(Year), "to ", max(Year))) %>% 
        config(displayModeBar = FALSE) %>% 
        animation_opts(
          frame = 250
        ) %>% 
        animation_slider(
          currentvalue = list(prefix = "Year: ", font = list(color="#FA8072"))
        )
    })
    
    # -------------------------------------------------- #
    # -------------------------------------------------- #
    # ------------------ Fourth Tab -------------------- #
    # -------------------------------------------------- #
    # -------------------------------------------------- #     

    output$view <- renderGvis({
      p<- gm %>% 
        filter(Year == input$date) %>%
        select(input$param, Year, country)
      
    p2<- gvisGeoChart(p, locationvar = 'country',  colorvar = input$param)
      
      return(p2)
    })
    
    
  
}

shinyApp(ui, server)
