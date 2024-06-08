#Install necessary packages
#install.packages("reader")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("arules")

#Load necessary packages
library("reader")
library("dplyr")
library("tidyverse")
library("arules")
library("shiny")
#C:/Users/Len/Documents/Project DS/Project1/grc.csv
#C:/Users/Len/Documents/Project DS/Project1/items.txt

#Take path from user
path <- readline('Enter your csv file path')
#Read data
tryCatch({
  # Code block where you expect an error might occur
  ds <- read.csv(path)
}, error = function(e) {
  # Code block to handle the error
  stop("Please insert the right path and try again")
})

ds
#Explore data structure and check for missing values
str(ds)
summary(ds)
# missing values 
sum(is.na(ds))

#check outliers from data
x<-boxplot(ds$count , main = "count" , col = "pink")
x1<-boxplot(ds$total ,  main = "total" , col = "midnightblue")
x2<-boxplot(ds$rnd ,  main = "rnd" , col = "palevioletred2")
x3<-boxplot(ds$age ,  main = "age" , col = "indianred1")

#Dashboard setup
par(mfrow=c(2,2))

#Compare cash and credit totals using pie
x <- table(ds$paymentType)
percentage <- paste0(round(x*100/sum(x)),"%")
pie(x, labels = percentage, main = "Compare cash and credit", col = c("red", "blue"))
legend("bottomright", legend = c(paste("Cash" ," (",x[["Cash"]] ,")"),paste("Credit"," (",x[["Credit"]] ,")")), fill = c("red", "blue"), cex = 0.6)


#Compare each age and sum of total spending
summarized_data1 <- ds %>%
  group_by(age) %>%
  summarise(total = sum(total))
barplot(height = summarized_data1$total, names = summarized_data1$age,
        xlab = "Age", ylab = "Total Spending", col = "turquoise2")

max(summarized_data1$total)
min(summarized_data1$total)
mean(summarized_data1$total)
median(summarized_data1$total)

#Box plot for total spending
boxplot(ds$total, xlab = "Spending", main = "Distribution of Total Spending", col = "slateblue1")

max(ds$total)
min(ds$total)
mean(ds$total)
median(ds$total)
# max spending = 2500k
# min spending = 100
# mean = 1289.5  , q1 = 750  , q2 = 1800

#Show each city total spending and arrange it by total descending
summarized_data2 <- ds %>%
  group_by(city) %>%
  summarise(total = sum(total)) %>%
  arrange(desc(total))
plot(x = 1:nrow(summarized_data2), y = summarized_data2$total, 
     xlab = "City", ylab = "Total Spending", 
     col = "blue", main = "Cities' Total Spending",
     pch = 19, cex = 1.5, xaxt = "n")
axis(1, at = 1:nrow(summarized_data2), labels = summarized_data2$city, las = 2, cex.axis = 0.7)

max(summarized_data2$total)
min(summarized_data2$total)
mean(summarized_data2$total)
median(summarized_data2$total)

#Take item path from user
itempath <- readline('Enter your txt file path')

#Read item data
tryCatch({
  # Code block where you expect an error might occur
  ad <- read.transactions(itempath, sep = ",")
}, error = function(e) {
  # Code block to handle the error
  stop("Please insert the right path and try again")
})

#Association rules
min_support <- as.numeric(readline("Enter the minimum support: "))
min_confidence <- as.numeric(readline("Enter the minimum confidence: "))
if ((min_support >= 0.001 && min_support <= 1) && (min_confidence >= 0.001 && min_confidence <= 1)) {
  apriori_rules <- apriori(ad, parameter = list(supp = min_support, conf = min_confidence , minlen = 2))
} else {
  print("The minimum support and confidence support should be between 0.001 and 1")
}
inspect(apriori_rules)





#Frequency plot
item_freq <- itemFrequency(ad, type = "absolute")
top_50_items <- sort(item_freq, decreasing = TRUE)[1:50]
print(top_50_items)
item_freq <-top_50_items



#####################################################################
#GUI

ui <- fluidPage(
  titlePanel("Plot Selector"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("plotType", "Select Plot Type:",
                   choices = c("Pie chart compare cash vs credit", "Box plot total spending", "scatter plot cities descend","bar plot ages","Freq items"),
                   selected = "Pie chart compare cash vs credit"),
      h2("Clustering"),
      sliderInput("confidence_slide","select Confidence value: ",
                  min = 0.001,max = 1,value = 0.5, step=0.001),
      actionButton("submit", "Submit"),
      
      sliderInput("support_slide","select Support value: ",
                  min = 0.001,max = 1,value = 0.5, step=0.001),
      actionButton("submit", "Submit"),
      
      sliderInput("kmean","select the value of k mean: ",
                  min=2,max= 4,value=3,step=1),
      actionButton("submit", "Submit"),
      
      
    ),
    mainPanel(
      plotOutput("plot"),
      fluidRow(
        h2("clustring table"), column(6, tableOutput("table1")),
        h2("Apriori table"), column(6, tableOutput("table2"))
      ),
      
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Render plot based on user selection
  output$plot <- renderPlot({
    if (input$plotType == "Pie chart compare cash vs credit") {
      #percentages for pie chart
      x <- table(ds$paymentType)
      precentage <- paste0(round(x*100/sum(x)),"%")
      # pie chart
      pie(x , labels = precentage , main = "Compare  cash and credit", col = c("red", "blue"))
      legend("bottomright", legend = c("cash "," credit"), fill = c("red", "blue"),cex = 0.9)
    }
    else if (input$plotType == "scatter plot cities descend") {
      # total spending per city and arrange in descending order
      #Show each city total spending and arrange it by total descending
      
      plot(x = 1:nrow(summarized_data2), y = summarized_data2$total, 
           xlab = "City", ylab = "Total Spending", 
           col = "blue", main = "Cities' Total Spending",
           pch = 19, cex = 1.5, xaxt = "n")
      axis(1, at = 1:nrow(summarized_data2), labels = summarized_data2$city, las = 2, cex.axis = 0.7)
    }
    else if (input$plotType == "Box plot total spending") {
      #box plot for total spending
      boxplot(ds$total , xlab = "spending", main = "the distribution of total spending",col = "slateblue1")
    } 
    else if (input$plotType == "bar plot ages"){
      barplot(height = summarized_data1$total, name = summarized_data1$age , xlab = "Age", ylab = "total spending",col = "turquoise2")
    }
    else if (input$plotType == "Freq items") {
      item_freq <- itemFrequency(ad, type = "absolute")
      
      # Select top 50 frequent items
      top_50_items <- sort(item_freq, decreasing = TRUE)[1:50]
      
      # Plot top 50 frequent items as a bar plot
      barplot(top_50_items, main = "Top 50 Frequent Items", col = "skyblue")
    }
    
  }) 
  #checking values
  # Define reactive values for kmean, support, and confidence
  reactive_values <- reactive({
    list(
      support = input$support_slide,
      confidence = input$confidence_slide
    )
  })
  
  observeEvent(input$kmean, {
    if (!is.null(input$kmean) && input$kmean >= 2 && input$kmean <= 4) {
        summarized_data3 <- ds %>%
        group_by(customer, age) %>%
        summarise(total = sum(total))
      dd <- data.frame(summarized_data3$total, summarized_data3$age)
      kmean_clusters <- kmeans(dd, centers = input$kmean)
      table1 <- data.frame(name = summarized_data3$customer, age = summarized_data3$age, total = summarized_data3$total, cluster = kmean_clusters$cluster)
      output$table1 <- renderTable({ table1 })
    } else {
      print("Invalid number of clusters")
    }
  })
  
  
  observeEvent(input$confidence_slide, {
    if ((input$support_slide >= 0.001 && input$support_slide <= 1) && 
        (input$confidence_slide >= 0.001 && input$confidence_slide <= 1)) {
      apriori_rules <- apriori(ad, parameter = list(supp = input$support_slide, conf = input$confidence_slide , minlen = 3))
      output$table2 <- renderTable({ inspect(apriori_rules) })
    }
    #else {
    # print("The minimum support and confidence support should be between 0.001 and 1")
  }
  
  )
  
  observeEvent(input$support_slide, {
    if ((input$support_slide >= 0.001 && input$support_slide <= 1) && 
        (input$confidence_slide >= 0.001 && input$confidence_slide <= 1)) {
      apriori_rules <- apriori(ad, parameter = list(supp = input$support_slide, conf = input$confidence_slide , minlen = 2))
      output$table2 <- renderTable({ inspect(apriori_rules) })
    } # else {
    # print("The minimum support and confidence support should be between 0.001 and 1")
  }
  )
  
  
  output$my_kmeans<- renderText({input$kmean})
  output$my_path<- renderText({input$path})
  output$confidence_value <- renderText({input$confidence_slide})
  output$support_value <- renderText({input$support_slide})
  output$my_table<- renderTable({
    table1 <- data.frame(name = summarized_data3$customer, age = summarized_data3$age, total = summarized_data3$total, cluster = kmean_clusters$cluster)
    return(table1)
  })
  output$my_table <- renderTable({
    if (!is.null(summarized_data3) && !is.null(kmean_clusters)) {
      table1 <- data.frame(name = summarized_data3$customer, age = summarized_data3$age, total = summarized_data3$total, cluster = kmean_clusters$cluster)
      return(table1)
    } else {
      return(NULL)
    }
  })
}






# Run the application
shinyApp(ui = ui, server = server)
  