# Carregar Pacotes
# install.packages("pacman")
pacman::p_load(shiny, 
               shinydashboard, 
               shinyWidgets, 
               shinythemes, 
               COVID19, 
               fpp3, 
               plotly, 
               forecast, 
               stringr, 
               drc,
               ggpubr,
               padr)

#------------------------------------------------------------

# Definir o UI
source("R/src/ui/ui.R")

#------------------------------------------------------------

# Definir o servidor
source("R/src/server/server.R")

#------------------------------------------------------------

# Criar o aplicativo Shiny
shinyApp(ui, server)
