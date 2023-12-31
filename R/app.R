# Carregar Pacotes
# install.packages("pacman")
pacman::p_load(shiny, 
               shinydashboard, 
               shinyWidgets, 
               shinythemes, 
               distributional,
               COVID19, 
               fpp3, 
               plotly, 
               forecast, 
               stringr, 
               drc,
               ggpubr,
               padr,
               zoo)

#------------------------------------------------------------

# Definir o UI
source("./src/ui/ui.R")

#------------------------------------------------------------

# Definir o servidor
source("./src/server/server.R")

#------------------------------------------------------------

# Criar o aplicativo Shiny
shinyApp(ui, server)
