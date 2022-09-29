library(shiny)
library(RSQLite)
library(DBI)
library(dplyr)
library(zoo)
library(plotly)

conexion <- dbConnect(
  drv = SQLite(),
  dbname ="C:\\Pruebas_conexion\\Alumbrado.db")
APAES<- dbGetQuery(conexion,"SELECT EMPRESA, Ap.CICLO,F_echa, Ap.NIC, Cliente, TIP_REC, KWH, Importe FROM  Ap, ClientesAP, fechas WHERE ClientesAP.NIC =AP.NIC and Ap.CICLO= fechas.CICLO AND TIP_REC ='TR010' ORDER BY Ap.CICLO ",header=TRUE)

APAES$F_echa<- as.Date(APAES$F_echa, format="%d/ %m/ %Y")

APCAESS<-filter(APAES, EMPRESA == "CAESS") 
APCAESS<-APCAESS%>%
  group_by(F_echa)%>%
  summarise(Mwh1= trunc(sum(KWH)/1000))%>%
  arrange(F_echa)

APCLESA<-filter(APAES, EMPRESA == "CLESA")
APCLESA<-APCLESA%>%
  group_by(F_echa)%>%
  summarise(Mwh2= trunc(sum(KWH)/1000))%>%
  arrange(F_echa)

APEEO<-filter(APAES, EMPRESA == "EEO")
APEEO<-APEEO%>%
  group_by(F_echa)%>%
  summarise(Mwh3= trunc(sum(KWH)/1000))%>%
  arrange(F_echa)

APDEUSEM<-filter(APAES, EMPRESA == "DEUSEM")
APDEUSEM<-APDEUSEM%>%
  group_by(F_echa)%>%
  summarise(Mwh4= trunc(sum(KWH)/1000))%>%
  arrange(F_echa)

VALfIN<-max(APAES$F_echa)
VALINI<-min(APAES$F_echa)
valmed<-(VALfIN-365)

APAES2<- APAES%>%
     group_by(F_echa)%>%
     summarise(Mwh= trunc(sum(KWH)/1000))%>%
     arrange(F_echa)

ui<- fluidPage(
                titlePanel(title = "Facturacion de AP "),
                sidebarLayout(sidebarPanel(
                  
                                      
                                      dateRangeInput(inputId = "rango", "Seleccione el rango", start = min(APAES2$F_echa), end=max(APAES2$F_echa),min = min(APAES2$F_echa), max = max(APAES2$F_echa),format ="dd/mm/yyyy"),
                                      #selectizeInput('primero',"Seleccione alcaldia", choices = NULL, label=NULL)
                                         ),
                              mainPanel(plotlyOutput(outputId = "graf1"),plotlyOutput(outputId = "graf2"),plotlyOutput(outputId = "graf3"),plotlyOutput(outputId = "graf4"),plotlyOutput(outputId = "graf5"))) 

)

server <- function(input, output, session) {
  updateSelectizeInput( session,'primero', choices = APAES$Cliente, server = TRUE)
  
  output$graf1<- renderPlotly({
                             APAES3<-APAES2[APAES2$F_echa>= input$rango[1] & APAES2$F_echa<=input$rango[2],]
                            # plot(APAES3$F_echa, APAES3$Mwh, col= "red",lwd= 4 ,type="l",xlab="Fechas",ylab="Mwh" , main="Energia Mwh AP AES")
                             plot_ly(APAES3,x=~APAES3$F_echa, y=~APAES3$Mwh, type = 'scatter', mode = 'lines',line = list(color = "red",width = 2))%>%
                               layout(title="Energia AP AES", xaxis=list(title="Rango de fechas"), yaxis=list(title="Energia en Mwh"))
                             
                             
                              })
 
  output$graf2<- renderPlotly({
    APAES4<-APCAESS[APCAESS$F_echa>= input$rango[1] & APCAESS$F_echa<=input$rango[2],]
    plot_ly(APAES4,x=~APAES4$F_echa, y=~APAES4$Mwh1, type = 'scatter', mode = 'lines',line = list(color = "skyblue",width = 2))%>%
      layout(title="Energia AP CAESS", xaxis=list(title="Rango de fechas"), yaxis=list(title="Energia en Mwh")) }) 
  
 
  output$graf3<- renderPlotly({
    APAES5<-APCLESA[APCLESA$F_echa>= input$rango[1] & APCLESA$F_echa<=input$rango[2],]
    plot_ly(APAES5,x=~APAES5$F_echa, y=~APAES5$Mwh2, type = 'scatter', mode = 'lines',line = list(color = "BLUE",width = 2))%>%
      layout(title="Energia AP CLESA", xaxis=list(title="Rango de fechas"), yaxis=list(title="Energia en Mwh")) }) 
  
  output$graf4<- renderPlotly({
    APAES6<-APEEO[APEEO$F_echa>= input$rango[1] & APEEO$F_echa<=input$rango[2],]
    plot_ly(APAES6,x=~APAES6$F_echa, y=~APAES6$Mwh3, type = 'scatter', mode = 'lines',line = list(color = "purple",width = 2))%>%
      layout(title="Energia AP EEO", xaxis=list(title="Rango de fechas"), yaxis=list(title="Energia en Mwh")) })
  
  output$graf5<- renderPlotly({
    APAES7<-APDEUSEM[APDEUSEM$F_echa>= input$rango[1] & APDEUSEM$F_echa<=input$rango[2],]
    plot_ly(APAES7,x=~APAES7$F_echa, y=~APAES7$Mwh4, type = 'scatter', mode = 'lines',line = list(color = "green",width = 2))%>%
      layout(title="Energia AP DEUSEM", xaxis=list(title="Rango de fechas"), yaxis=list(title="Energia en Mwh")) })
  
  
  
}  

shinyApp(ui = ui, server = server)


