
# install.packages("readxl")
library(ggplot2)
library(shiny)
library(shinydashboard)
library(rsconnect)
library(lpSolve)

options(shiny.maxRequestSize=30*1024^2) 

ui <- dashboardPage(
dashboardHeader(),
dashboardSidebar( 

  tabsetPanel(id = 'first_ch',
              
              
  tabPanel("Search Engine", 
           selectInput(inputId = "League",label="Select the league",
                       choices= c("Any", "Premier League","La Liga","Bundesliga","Seria A")),
           selectizeInput("Club", "Select the club",choices=c("Any")),
           sliderInput("Age", label = "Choose the range of age", min = 16, 
                       max = 42, value = c(16, 42)),
           selectInput(inputId = "Position",label="Select the Position of the player",
                       choices= c("Any","GK","Defender","Midfielder","Forward"))
          
           
           
  ),
  tabPanel("Squad Builder",
           textInput("money", label = "How much is your budget? (Mill. €)"),
           sliderInput("gk", label = "Choose number of goalkeepers you want to buy", min = 0, 
                       max = 2, value = 0),
           sliderInput("def", label = "Choose number of defenders you want to buy", min = 0, 
                       max = 5, value = 0),
           sliderInput("mid", label = "Choose number of midfielders you want to buy", min = 0, 
                       max = 5, value = 0),
           sliderInput("fw", label = paste0("Choose number of","forwards", "you want to buy"), min = 0, 
                       max = 4, value = 0),

          
           actionButton(
             inputId = "submit_loc",
             label = "Submit"
           )))
  
  
  # br(),
  # br(),
  # downloadButton("downloadData", "Download",class="butt1")
  ),

dashboardBody(
  
  h3(textOutput("text")),
  br(),
  br(),
  dataTableOutput("table")
))
    
 server <- shinyServer(function(input, output,session) {
      lpsolver <- function(df,num, cap){
        p <- df$Rating
        w <- df$Price
        exact.num.elt <- num
        cap <- cap
        mod <- lp(direction = "max",
                  objective.in = p,
                  const.mat = rbind(w, rep(1, length(p))),
                  const.dir = c("<=", "="),
                  const.rhs = c(cap, exact.num.elt),
                  all.bin = TRUE)
        # Solution
        df <- df[which(mod$solution >= 0.999),]
      }
      
      choice <- reactive({

        df  <- readxl::read_xlsx("final_data3.xlsx", col_names = TRUE) #Reading the file
        if (input$League != "Any"){
        df <-  df[df$League == input$League,]
        }
        choices <- c("Any",sort(unique(df$Clubs)))
        return (choices)
      })
      
      observe({
        updateSelectizeInput(session,"Club", choices=choice())
      })
      
      agg_data <- reactive({
        keep_cols <- c("Players","Clubs","Mins", "Age","Values","Rating","Position")
        df  <- readxl::read_xlsx("final_data3.xlsx", col_names = TRUE) #Reading the file
        if (input$League != "Any"){
          
          df <- df[df$League == input$League,]
        }
        
        if (input$Club != "Any"){
         
          df <- df[df$Clubs == input$Club,]
        }
        # print(input$Age)
        ages = input$Age
        # print(ages)
        # print(ages[1])
        df <- df[(df$Age >= ages[1]) & (df$Age <= ages[2]),]
        choices= c("Any","GK","Defender","Midfielder","Forward")
        real_choices = c("Any","GK","D","M","FW")
        ind = match(input$Position, choices)
        real_position = real_choices[ind]
        if (real_position != "Any"){
        df <- df[grepl(real_position,df$Position1),]
        }
        return (df[keep_cols])
      })
      
squad_builder <- reactive({
  keep_cols <- c("Players","Clubs","Mins", "Age","Values","Rating","Position","Price")
  final  <- readxl::read_xlsx("final_data3.xlsx", col_names = TRUE) #Reading the file
  final <- final[order(-final$Rating),]
  GK <- mean(final[grepl("GK",final$Position1),]$Price)/10**6
  defenders <- mean(final[grepl("D",final$Position1),]$Price)/10**6
  midfields <- mean(final[grepl("M",final$Position1),]$Price)/10**6
  forwards <- mean(final[grepl("FW",final$Position1),]$Price)/10**6
  num_GK = input$gk
  num_DEF = input$def
  num_MID = input$mid
  num_FW = input$fw
  total = as.numeric(input$money)*10**6
  GK_val <- total * GK * num_GK/(GK * num_GK + defenders*num_DEF + midfields*num_MID + forwards*num_FW)
  DEF_val <- total * defenders * num_DEF/(GK * num_GK + defenders*num_DEF + midfields*num_MID + forwards*num_FW)
  MID_val <- total * midfields * num_MID/(GK * num_GK + defenders*num_DEF + midfields*num_MID + forwards*num_FW)
  FW_val <- total * forwards * num_FW/(GK * num_GK + defenders*num_DEF + midfields*num_MID + forwards*num_FW)
  final_gk <- final[(final$Price <= GK_val) & grepl("GK",final$Position1) ,]
  final_def <- final[(final$Price <= DEF_val) & grepl("D",final$Position1),]
  final_mid <- final[(final$Price <= MID_val) & grepl("M",final$Position1),]
  final_fw <- final[(final$Price <= FW_val) & grepl("FW",final$Position1),]
  # paste(length(final_gk),length(final_def),length(final_mid), length(final_fw))
  common_mid_fw <- intersect(final_mid$Players,final_fw$Players)
  common_mid_def <- intersect(final_mid$Players,final_def$Players)
  common_fw_def <- intersect(final_def$Players,final_fw$Players)
  final_mid <- final_mid[!(final_mid$Players %in% c(common_mid_fw,common_mid_def)), ]
  final_def <- final_def[!(final_def$Players %in% common_fw_def),]
  final_gk <- lpsolver(final_gk, num_GK, GK_val)
  final_def <- lpsolver(final_def, num_DEF, DEF_val)
  final_mid <- lpsolver(final_mid, num_MID, MID_val)
  final_fw <- lpsolver(final_fw, num_FW, FW_val)
  
  
  
  final_dt <- rbind(final_gk,final_def, final_mid,final_fw)
  
  
  
  return (final_dt[keep_cols])
  
  
  
})

observeEvent(input$first_ch,
if (input$first_ch == "Search Engine"){
  
  output$table <- renderDataTable({
    agg_data()
    
  })
  }
)
observeEvent(
  eventExpr = input[["submit_loc"]],
  handlerExpr = { 
    output$text <- renderText({
      if(input$money != ""){
      df <- squad_builder()
      saving <- as.numeric(input$money)*10**6 - sum(df["Price"])
      paste("Total savings are:",saving/10**6, "Mill. Euro")
      }
    })
    output$table <- renderDataTable({
      if(input$money != ""){
      squad_builder()[-8]
      }
    })
    
  }
  )
})

shinyApp(ui, server)

