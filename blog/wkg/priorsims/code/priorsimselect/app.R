library(viridis)
library(MASS)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(shiny)
library(truncnorm)
library(bslib)
library(showtext)
library(curl)


font_add_google(name = "barlow", family = "Barlow")
showtext_auto()



ui <- page_fillable(theme = bs_theme( primary= "#111424", primarylight= "#202543", secondary = "#FEFFF9", secondarydark= "#eff0e8",
                                      base_font = font_google("Barlow")) ,  
                    layout_sidebar(
                      sidebar = sidebar(open = "always",max_height_mobile = "400px",width = "200px",bg = "#eff0e8",
                                        sliderInput("rho.range", "Air density range", min = .8, max = 1.4, step = .01, value = c(0.95, 1.05)),
                                        sliderInput("cda.range", "Cda range", min = .1, max = .5, step=0.01, value = c(.2, .3)),
                                        sliderInput("crr.range", "Crr range", min = 0.001, max = 0.02, step=.001, value = c(0.003, 0.007)),
                                        sliderInput("m.rider.range", "Rider weight range", min = 30, max = 90, step = 1, value = c(59, 61))
                                        
                      ),
                      border = T, border_color = "#2c3e50",
                      height = "400px", fillable = T, fill = T,
                      # layout_columns( 
                      #   card("Select gradients and speeds", plotOutput("priorsimgeneral"), full_screen = T, fill = T),
                      #   fill = T, max_height = "400px"
                      # ) 
                      navset_card_underline(height = "400px",full_screen = T,
                                            title = "Select gradients and speeds",
                                            nav_panel("Plot", plotOutput("plot")),
                      )
                      
                    )
)  

server <- function(input, output) {
  
  data <- reactive({
    
    # Setup
    set.seed(1) # 40
    
    nobs = 1e6
    
    # Air resistance  parameters
    cda <- runif(nobs, input$cda.range[1], input$cda.range[2])
    rho <- runif(nobs, input$rho.range[1], input$rho.range[2])
    
    
    # Rolling resistance parameters
    crr <- runif(nobs, input$crr.range[1], input$crr.range[2])
    
    # Constant
    g <- 9.81  # Gravity (m/s²)
    m.rider <- runif(nobs, input$m.rider.range[1], input$m.rider.range[2])  # Rider mass (kg) 
    m.system <- m.rider + 8
    
    # Measured parameters
    gradient <- sample(seq(0.06, .16, by = .02), size = nobs, replace = T)
    
    # Speed
    speed.true <- sample(seq(2, 10, length.out=50), size = nobs, replace = T)
    
    power.true = ((.5 * rho * cda * speed.true^2) + (crr * m.system * g * cos(atan(gradient))) + (m.system * g * sin(atan(gradient)) )) * speed.true
    
    
    # Create final dataset
    d = data.frame(speed.true = speed.true * 3.6, gradient = gradient,
                   power.true = power.true / m.rider)
    
    d = d %>% group_by(speed.true, gradient) %>% 
      reframe(mean = mean(power.true), 
              lower=quantile(power.true, probs = .05), 
              upper = quantile(power.true, probs = .95))
    
    
    # Sort by power
    d = d %>% mutate(obs=row_number())
    
    
    results = list(d=d)
    
    
  })
  
  output$plot <- renderPlot({
    
    suppressWarnings({
      
      data()$d %>% ggplot(aes(x=speed.true, y=mean, ymin=lower, ymax=upper, group = as.factor(gradient), col=as.factor(gradient), fill=as.factor(gradient))) + 
        geom_line(lwd=1) +
        geom_ribbon(alpha=.5) +
        scale_color_viridis(name="Gradient", discrete = T) +
        scale_fill_viridis(name="Gradient", discrete = T) +
        scale_y_continuous(name = "Estimated w/kg", limits = c(0, 10), breaks = seq(0, 10, by=1)) +
        scale_x_continuous(name = "Speed in km/h", limits = c(8, 30), breaks = seq(0, 30, by=5)) +
        theme_linedraw(base_size = 10) +
        theme(text = element_text(size=rel(5), family = "Barlow"),
              plot.title = element_text(size=rel(7), hjust = .5),
              legend.text = element_text(size=rel(5)),
              axis.ticks.x = element_blank(),
              axis.line.x = element_blank(),
              strip.text.x = element_text(size=rel(5)),
              strip.text.y = element_text(size=rel(5))) 
      
    })
    
    
  })
  
}

shinyApp(ui = ui, server = server)