
#test case 
test <- matrix(0, ncol=4, nrow= 100);
test <- as.data.frame(test);
test[,1] <- paste0("P",1:nrow(test));

test[,2] <- runif(100, min=0, max=100);
test[,3] <- runif(100, min=0, max=100);
test[,4] <- runif(100, min=0, max=100);
colnames(test) <- c("name", "quality", "cost", "delivery")


test2 <- matrix(0, ncol=4, nrow= 100);
test2 <- as.data.frame(test);
test2[,1] <- paste0("P",1:nrow(test));

test2[,2] <- runif(100, min=50, max=100);
test2[,3] <- runif(100, min=50, max=100);
test2[,4] <- runif(100, min=50, max=100);
colnames(test2) <- c("name", "quality", "cost", "delivery")


#scatter app
rv <- reactiveValues();
ui <- fluidPage(
  plotOutput("plot1",
             click = "plot_click",
             hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
             dblclick = "plot_dblclick"),
  tableOutput("plot_clicked_points1"))

server <- function(input, output, session){
  output$plot1 <- renderPlot({
    ggplot(data = test, aes(x = cost, y = delivery)) + 
      geom_point(aes(colour=quality), size=3, alpha=0.8) +
      scale_colour_gradient(low = "red", high = "blue") +
      scale_x_continuous(limits = c(0, NA)) +
      scale_y_continuous(limits = c(0, NA)) 
    }, width = "auto", height = "auto");

  #show the plot data hovered
  observeEvent(input$plot_hover, 
               {rv$selected_plot <- nearPoints(test, input$plot_hover, threshold = 4, maxpoints = 1)});
  output$plot_clicked_points1 <- renderTable({rv$selected_plot});

  #select the plot on where clicked  
  observeEvent(input$plot_dblclick, 
               {selected_plot <<- nearPoints(test, input$plot_dblclick, threshold = 4, maxpoints = 1)});
  
}

shinyApp(ui, server)




#test
p <- ggplot(data=test, aes(x=cost, y=delivery)) + 
  geom_point(aes(colour=quality), size=3, alpha=0.8) +
  geom_point(data=test2, aes(colour=quality), size=3, alpha=0.8) +
    scale_colour_gradient(low = "red", high = "blue") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA)) 
ggplotly(p)




