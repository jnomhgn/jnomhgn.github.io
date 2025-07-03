library(viridis)
library(MASS)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(shiny)
library(truncnorm)
library(bslib)
library(shiny)
library(bslib)
library(DT)
library(showtext)


font_add_google(name = "barlow", family = "Barlow")
showtext_auto()


d = data.frame(rider = c("Rider A", "Rider B"),
               gradient = rep(7, 2),
               speed = c(26, 24),
               rho = c(1.0, 1.0),
               cda = c(0.25, 0.25),
               crr = c(0.003, 0.003),
               riderweight = c(60, 60)
) %>%
  rbind(rep(NA, ncol(.)))

ui <- page_fillable( theme = bs_theme( primary= "#111424", primarylight= "#202543", secondary = "#FEFFF9", secondarydark= "#eff0e8",
                                       base_font = font_google("Barlow")),  
                     layout_sidebar(
                       sidebar = sidebar(open = "always",max_height_mobile = "400px",width = "200px", bg = "#eff0e8",
                                         sliderInput("rho.range", "Air density ±", min = 0, max = 0.2, step = .01, value = 0.05),
                                         sliderInput("cda.range", "Cda ±", min = 0, max = .1, step=0.01, value = 0.05),
                                         sliderInput("crr.range", "Crr ±", min = 0, max = 0.003, step=.001, value = 0.002),
                                         sliderInput("m.rider.range", "Rider weight ±", min = 0, max = 3, step = 1, value = 0)
                                         
                                         
                       ),
                       border = T, border_color = "#2c3e50",
                       height = "400px", fillable = T, fill = T,
                       
                       navset_card_underline(height = "400px",full_screen = T,
                                             title = "Custom gradients and speeds",
                                             nav_panel("Plot", plotOutput("plot")),
                                             nav_panel("Data", dataTableOutput("table")),
                       )
                       
                     )
)  

server <- function(input, output) {
  
  # Set dreactive
  dreactive = reactiveVal(d)
  
  output$table <- renderDT(dreactive(),editable = T)
  
  # Observe changes to the DT table
  observeEvent(input$table_cell_edit, {
    
    # Get changes
    changes = input$table_cell_edit
    
    # Get the current data
    dupdated = dreactive()
    
    # Apply changes
    dupdated[changes$row, changes$col] <- coerceValue(changes$value, dupdated[changes$row, changes$col])
    
    # Update the dreactive
    dreactive(dupdated)
  })
  
  data <- reactive({
    
    dcurrent = dreactive() %>% drop_na(gradient, speed, riderweight) %>%
      mutate(m.rider =riderweight, speed = speed / 3.6, gradient = gradient * 0.01)
    
    # Setup
    set.seed(1) # 40
    
    nobs = 1e6
    
    # Air resistance  parameters
    cda <- sapply(1:nrow(dcurrent), function(x)
      runif(nobs, dcurrent$cda[x] - input$cda.range, dcurrent$cda[x] + input$cda.range)
    )
    rho <- sapply(1:nrow(dcurrent), function(x)
      runif(nobs, dcurrent$rho[x] - input$rho.range, dcurrent$rho[x] + input$rho.range)
    )
    
    # Rolling resistance parameters
    crr <- sapply(1:nrow(dcurrent), function(x)
      runif(nobs, dcurrent$crr[x] - input$crr.range, dcurrent$crr[x] + input$crr.range)
    )
    
    # Constant
    g <- 9.81  # Gravity (m/s²)
    m.rider <- sapply(1:nrow(dcurrent), function(x)
      runif(nobs, dcurrent$m.rider[x] - input$m.rider.range, dcurrent$m.rider[x] + input$m.rider.range)
    )
    m.system <- m.rider + 8
    
    # Measured parameters
    gradient <- dcurrent$gradient
    
    # Speed
    speed <- dcurrent$speed
    
    power = sapply(1:nrow(dcurrent), function(x)
      ((.5 * rho * cda * speed[x]^2) + (crr * m.system[, x] * g * cos(atan(gradient[x]))) + (m.system[, x] * g * sin(atan(gradient[x])))) * speed[x]
    )
    
    # Create final dataset
    plotdata = as.data.frame(power) %>% `colnames<-`(dcurrent$rider) %>%
      pivot_longer(everything(), names_to = "rider", values_to = "power") %>%
      group_by(rider) %>%
      reframe(mean=mean(power), lower=quantile(power, probs = .05), upper=quantile(power, probs = .95)) %>%
      left_join(dcurrent, by = join_by(rider)) %>%
      mutate(mean=mean/m.rider, lower=lower/m.rider, upper=upper/m.rider)
    
    
    results = list(plotdata=plotdata)
    
  })
  
  
  
  output$plot <- renderPlot({
    data()$plotdata %>% ggplot(aes(x=rider, y=mean, ymin=lower, ymax=upper)) +
      geom_pointrange(size=1, shape=21, alpha=1, fill=viridis(n=nrow(data()$plotdata), begin = 0, end = 1))+
      scale_y_continuous(name = "Estimated w/kg", limits = c(4, 9), breaks = seq(0, 10, by=1)) +
      scale_x_discrete(name = "Rider") +
      theme_linedraw(base_size = 10) +
      theme(text = element_text(size=rel(5)),
            plot.title = element_text(size=rel(7), hjust = .5),
            legend.text = element_text(size=rel(5)),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            strip.text.x = element_text(size=rel(5)),
            strip.text.y = element_text(size=rel(5)))
    
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)