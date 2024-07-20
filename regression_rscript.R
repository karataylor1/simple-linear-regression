#Simple Linear Regression Model with Data from Californian Hospitals


#Intall Graphing Library

install.packages("ggplot2")
library(ggplot2)

#File with Data on MRSI Bloodstream Infections 
#Hospitals with less than 125 beds not required to report
#infections predicted and infections reported so I removed them from the file.

file <- 'mrsa_bsi_odp_2022.csv'

#Error Handling

read_csv_with_encoding <- function(file_path) {
  
  tryCatch({
    
    data <- read.csv(file_path, fileEncoding = "UTF-8-BOM")
    return(data)
    
  }, error = function(e) {
    
    message("Error reading file: ", e)
    return(NULL)
    
  })
}


if (file.exists(file)) {
  
  csv_data <- read_csv_with_encoding(file)
  
  if (!is.null(csv_data)) {
    
    
    #Printing short example of csv file.
    
    print(head(csv_data, 5))

    #Linear Regression Information
    
    lm_model <- lm(Infections_Reported ~ Infections_Predicted, data = result)
    
    print(summary(lm_model))
    
    #Linear Regression Equation
    
    coefficients <- coefficients(lm_model)
    intercept <- as.numeric(coefficients["(Intercept)"])
    slope <- as.numeric(coefficients["Infections_Predicted"])
    
    print(coefficients)
    print(intercept)
    print(slope)
    
    equation <- paste("y =", round(slope, 2), "* x +", round(intercept, 2)) 
    #Rounding to 2 Decimals
    print(equation)

    #Plot 
    
    ggplot(data = result, aes(x = Infections_Predicted, y = Infections_Reported)) +
      geom_point(colour = 'red') +
      geom_smooth(method = "lm", se = FALSE, colour = 'blue') +  
      ggtitle(equation) +
      xlab('Infections Predicted') +
      ylab('Infections Reported') +
      theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

    #Calculate Mean Infections Reported
    
    reported_infections <- csv_data$Infections_Reported
    total <- 0
    amount <- length(reported_infections)
    
    #Loop working with List/Array
    
    for (i in seq_along(reported_infections)) {
      
      total <- total + reported_infections[i]
    }
    
    mean_infections <- total / amount
    print(paste("Mean Infections Reported:", mean_infections))
    
    
  } else {
    
    print("Failed to read the CSV file.")
  }
  
} else {
  
  print(paste("File", file, "does not exist."))
}
