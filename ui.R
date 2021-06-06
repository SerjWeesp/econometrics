# Load libraries, data -----------------------------------------------


# Page 1 - Introduction ----------------------------------------------
data_panel <- tabPanel("Data", 
                       fluidPage(
                         titlePanel("Data summary"),
                         sidebarLayout(
                           sidebarPanel(
                             checkboxInput(
                               inputId = "show_size",
                               label = "Show size"
                             ),
                             checkboxInput(
                               inputId = "show_cols",
                               label = "Show colnames"
                             ),
                             checkboxInput(
                               inputId = "show_sum",
                               label = "Show summary"
                             ),
                             checkboxInput(
                               inputId = "show_na",
                               label = "Show NAs"
                             )
                           ),
                           
                           mainPanel(verbatimTextOutput("prn"),verbatimTextOutput("prn2"), verbatimTextOutput("prn3"),
                                     verbatimTextOutput("prn4"))
                         )
                       )
)

seda_panel <- tabPanel("SEDA", fluidPage(
                                titlePanel("Plots"),
                                 sidebarPanel(
                                   selectInput(inputId = "country1",label =  "Country:", 
                                               unique(data$Country)),
                                   selectInput(inputId = "variable1",label =  "Variable 1:", 
                                               selected = "Compensation.of.employees",
                                               colnames(data_clean)[3:len]),
                                   selectInput(inputId = "variable2", label = "Variable 2:", 
                                               selected = "Value.added..gross",
                                               colnames(data_clean)[3:len])),
                                   mainPanel(
                                    plotOutput("plot1"),
                                    plotOutput("plot2"))
                                 ))





cv_panel <- tabPanel("Cross-validation",
                     fluidPage(
                       titlePanel("Model's options"),
                       sidebarPanel(
                         selectizeInput(
                           inputId = "ctry",
                           label = "Country:",
                           choices = unique(data$Country),
                           multiple = FALSE
                         ),
                         selectizeInput(
                           inputId = "dep",
                           label = "Dependent variable:",
                           choices = colnames(data_clean)[3:len],
                           selected = 'Compensation.of.employees',
                           multiple = FALSE
                         ),
                         numericInput('cv', 'Number of CV periods:', 5,
                                      min = 2, max = 12),
                         numericInput('size', 'Train set size:', 0.7,
                                      min = 0.1, max = 1.0, step=0.1),
                         print('Model 1'),
                         selectizeInput(
                           inputId = "ind1",
                           label = "Independent variable 1:",
                           choices = colnames(data_clean)[3:len],
                           selected = "Operating.surplus.and.mixed.income..gross",
                           multiple = FALSE
                         ),
                         selectizeInput(
                           inputId = "ind2",
                           label = "Independent variable 2:",
                           choices = colnames(data_clean)[3:len],
                           selected = "Value.added..gross",
                           multiple = FALSE
                         ), 
                         print('Model 2'),
                         selectizeInput(
                           inputId = "ind3",
                           label = "Independent variable 1:",
                           choices = colnames(data_clean)[3:len],
                           selected = "Subsidies",
                           multiple = FALSE
                         ),
                         selectizeInput(
                           inputId = "ind4",
                           label = "Independent variable 2:",
                           choices = colnames(data_clean)[3:len],
                           selected = "Tax.on.products",
                           multiple = FALSE
                         )
                       ),
                       mainPanel(
                         plotOutput("plot3")
                       )
                     ))


ui <- navbarPage("Time series cross-validation",
           data_panel,
           seda_panel,
           cv_panel
           
)