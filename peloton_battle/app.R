#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




library(shiny)
library(jsonlite)
library(RCurl)
library(ggplot2)
library(pelotonR)
library(lubridate)
library(ggthemes)

workout_frame = as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1u-H49MMV2WkqMilIw3LwPPKI8vHqzN1-_Iuu5ECBHlo/edit?usp=sharing'))
team_cheese = workout_frame[which(workout_frame$Name %in% c('aamk47', 'lipidleah', 'HallieOatmilk')),]
team_curd = workout_frame[which(workout_frame$Name %in% c('KidsDoc93', 'bryannoreen', 'ppnoreen')),]

cheese_agg = aggregate(team_cheese$Minutes,by=list(team_cheese$Name,team_cheese$Date),'sum')
curd_agg =  aggregate(team_curd$Minutes,by=list(team_curd$Name,team_curd$Date),'sum')
names(cheese_agg) = c('Name','Date','Minutes'); names(curd_agg) = c('Name','Date','Minutes');
if(nrow(cheese_agg[which(cheese_agg$Minutes>=60),])>0){
  cheese_agg[which(cheese_agg$Minutes>=60),]$Minutes = 60
}
if(nrow(curd_agg[which(curd_agg$Minutes>=60),])>0){
  curd_agg[which(curd_agg$Minutes>=60),]$Minutes = 60
}


cheese_daily = aggregate(cheese_agg$Minutes,by=list(cheese_agg$Date),'sum')
curd_daily = aggregate(curd_agg$Minutes,by=list(curd_agg$Date),'sum')

daily_topline = merge(data.frame('Group.1'=seq(as.Date('2021-07-01'),as.Date('2022-01-01'),1)),
                      cheese_daily,by='Group.1',all.x=T
                      )
daily_topline = merge(daily_topline,curd_daily,by='Group.1',all.x=T)
names(daily_topline) = c('Date','Cheese','Curd')
daily_topline[is.na(daily_topline)] = 0
daily_topline$Cheese=cumsum(daily_topline$Cheese)
daily_topline$Curd=cumsum(daily_topline$Curd)

daily_topline$Difference = daily_topline$Cheese - daily_topline$Curd

theme_blank <- theme(
  axis.line = element_blank(), 
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(), 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(), 
  #axis.ticks.length = unit(0, "lines"), # Error 
  axis.ticks.margin = unit(c(0,0,0,0), "lines"), 
  legend.position = "none", 
  panel.background = element_rect(fill = "white"), 
  panel.border = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.margin = unit(c(0,0,0,0), "lines"), 
  plot.background = element_rect(fill = "white"),
  plot.margin = unit(c(0,0,0,0), "lines")
)
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML(
      
      '
#left th {
      font-size: 40px;
      color: red;
      border-spacing: 0;
      border-collapse: collapse;
      margin: 0;
      padding: 0;
      
      }
      #left td {
      font-size: 40px;
      color: red;
      border: 1px solid white;
      width:100%; 
      border-spacing: 0;
      border-collapse: collapse;
      margin: 0;
      padding: 0;
      }


#center th {
      font-size: 40px;
      color: white;
     border-spacing: 0;
      border-collapse: collapse;
    margin: 0;
      padding: 0;

}
#center td {
      font-size: 40px;
      color: black;
border: 1px solid white;
width:100%; 
border-spacing: 0;
border-collapse: collapse;
    margin: 0;
      padding: 0;
}

#right th {
      font-size: 40px;
color: blue;
border-spacing: 0;
border-collapse: collapse;
margin: 0;
padding: 0;

}
#right td {
font-size: 40px;
color: blue;
border: 1px solid white;
width:100%; 
border-spacing: 0;
border-collapse: collapse;
margin: 0;
padding: 0;
}
img {
width:100%;
align:center;
}
      '
))),
    fluidRow(column(12,img(src='peloton_battle.png', align = "center"))),
    br(),br(),br(),br(),br(),
    fluidRow(plotOutput('minutesplot')),
    fluidRow(align='center',column(2),
             column(2, align="center",tableOutput('left')),
             column(4, align="center",tableOutput('center')),
             column(2, align="center",tableOutput('right')),
             column(2)
             ),
    fluidRow(
      column(6,align='center',
             span(h1("Team Cheese",fill='red'), style="color:red"),
             h3('aamk47,HallieOatmilk,lipidleah')
             ),
      column(6,align='center',
             span(h1("Team Curd",fill='blue'), style="color:blue"),
             h3('bryannoreen,KidsDoc93,ppnoreen')
             )
      ),
    fluidRow(
      column(6,
             tableOutput('cheesetable')
      ),
      column(6,
             tableOutput('curdtable')
      )
    )

    )
  
  
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$minutesplot <- renderPlot({
     tmp = data.frame('Team'=c('Cheese','Curd'),
       'Minutes'=
      c(
        sum(cheese_agg$Minutes),
      sum(curd_agg$Minutes))
      )
     tmp$Team = as.factor(tmp$Team)
     ggplot(tmp, aes(x=Team, y=Minutes)) +
       geom_bar(stat='identity',fill=c('Red','Blue')) +
       coord_flip() + theme_blank + theme(panel.border = element_blank(), axis.title.y=element_blank(),
                                                                               axis.text.y=element_blank(),
                                                                               axis.ticks.y=element_blank()) +
       geom_text(aes(label = Minutes, x = Team, y = Minutes),size=10, position = position_dodge(width = 0.8), hjust = -0.1) +
       geom_text(aes(label = Team, x = Team, y = 1),size=10, hjust = 1.1) +
       theme(text=element_text(size=21))+ylim(-100,max(tmp$Minutes)+50)+ggtitle('Total Minutes')
   },height=200)
   output$curdtable <- renderTable({
     tmp = team_curd[order(team_curd$Date,decreasing = T),]
     tmp$Date = as.character(tmp$Date)
     tmp
   })
   
   output$cheesetable <- renderTable({    
     tmp = team_cheese[order(team_cheese$Date,decreasing = T),]
     tmp$Date = as.character(tmp$Date)
     tmp
   })
   
output$leader <- renderPlot({
  
  ggplot() +
    geom_rect(data=data.frame(xmin=as.Date('2021-07-01'), xmax=as.Date('2022-01-01'), 
                              ymin=0, ymax=max(abs(daily_topline$Difference))+40), 
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), 
              fill="white", alpha=.15) +
    geom_rect(data=data.frame(xmin=as.Date('2021-07-01'), xmax=as.Date('2022-01-01'), 
                              ymin=-max(abs(daily_topline$Difference))-40, ymax=0), 
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax), 
              fill="white", alpha=.15) +
    geom_line(data=daily_topline,aes(x=Date, y=Difference)) +theme_light() + 
    theme(panel.border = element_blank(),
          panel.grid.minor = element_blank()
          ) +
    ylim(max(abs(daily_topline$Difference))+40,-max(abs(daily_topline$Difference))-40) +
    ggtitle('Lead Tracker') + ylab('Cheese            Curds') + xlab(' ') + scale_y_continuous(labels = abs)
    

  })


output$stats <- renderTable({
  full = c(sum(cheese_agg$Minutes),
               sum(curd_agg$Minutes))
  lastweek = c(sum(cheese_agg[which(cheese_agg$Date>=(Sys.Date()-7)),]$Minutes),
  sum(curd_agg[which(curd_agg$Date>=(Sys.Date()-7)),]$Minutes))
  thirtyday = c(sum(cheese_agg[which(cheese_agg$Date>=(Sys.Date()-30)),]$Minutes),
               sum(curd_agg[which(curd_agg$Date>=(Sys.Date()-30)),]$Minutes))
  workout_days = c(length(unique(cheese_agg$Date)),length(unique(curd_agg$Date)))
  workout_days = length(seq(as.Date('2021-09-01'),Sys.Date(),1))-workout_days
  avg_length = c(mean(cheese_agg$Minutes),mean(curd_agg$Minutes))
  
  tmp = data.frame('Cheese'=c(round(full[1],0),round(thirtyday[1],0),round(lastweek[1],0),round(workout_days[1],0),round(avg_length[1],0)),
             '_'=c('     Total Minutes     ','     30 Day (Minutes)     ','     7 Day (Minutes)     ',
                   '     Missed Days     ','     Avg Workout Time     '),
             'Curds'=c(round(full[1],0),round(thirtyday[2],0),round(lastweek[2],0),round(workout_days[2],0),round(avg_length[2],0))
             )
 # x=DT::datatable(tmp, options = list(dom='f',rownames=F)) %>%
 #   DT::formatStyle(columns = colnames(.), fontWeight = '30')

tmp
})

output$right <- renderTable({
  full = c(sum(cheese_agg$Minutes),
           sum(curd_agg$Minutes))
  lastweek = c(sum(cheese_agg[which(cheese_agg$Date>=(Sys.Date()-7)),]$Minutes),
               sum(curd_agg[which(curd_agg$Date>=(Sys.Date()-7)),]$Minutes))
  thirtyday = c(sum(cheese_agg[which(cheese_agg$Date>=(Sys.Date()-30)),]$Minutes),
                sum(curd_agg[which(curd_agg$Date>=(Sys.Date()-30)),]$Minutes))
  workout_days = c(length(unique(cheese_agg$Date)),length(unique(curd_agg$Date)))
  avg_length = c(mean(cheese_agg$Minutes),mean(curd_agg$Minutes))
  
  tmp = data.frame('Curds'=c(round(full[2],0),round(thirtyday[2],0),round(lastweek[2],0),round(workout_days[2],0),round(avg_length[2],0))
  )
  # x=DT::datatable(tmp, options = list(dom='f',rownames=F)) %>%
  #   DT::formatStyle(columns = colnames(.), fontWeight = '30')
  
  tmp
})

output$center <- renderTable({

  tmp = data.frame('_'=c('     Total Minutes     ','     30 Day (Minutes)     ','     7 Day (Minutes)     ',
                         '     Missed Days     ','     Avg Workout Time     '))
                   
  # x=DT::datatable(tmp, options = list(dom='f',rownames=F)) %>%
  #   DT::formatStyle(columns = colnames(.), fontWeight = '30')
  
  tmp
},align = 'c')

output$left <- renderTable({
  full = c(sum(cheese_agg$Minutes),
           sum(curd_agg$Minutes))
  lastweek = c(sum(cheese_agg[which(cheese_agg$Date>=(Sys.Date()-7)),]$Minutes),
               sum(curd_agg[which(curd_agg$Date>=(Sys.Date()-7)),]$Minutes))
  thirtyday = c(sum(cheese_agg[which(cheese_agg$Date>=(Sys.Date()-30)),]$Minutes),
                sum(curd_agg[which(curd_agg$Date>=(Sys.Date()-30)),]$Minutes))
  workout_days = c(length(unique(cheese_agg$Date)),length(unique(curd_agg$Date)))
  avg_length = c(mean(cheese_agg$Minutes),mean(curd_agg$Minutes))
  
  tmp = data.frame('Cheese'=c(round(full[1],0),round(thirtyday[1],0),round(lastweek[1],0),round(workout_days[1],0),round(avg_length[1],0)))
  
  # x=DT::datatable(tmp, options = list(dom='f',rownames=F)) %>%
  #   DT::formatStyle(columns = colnames(.), fontWeight = '30')
  
  tmp
})

}
# Run the application 
shinyApp(ui = ui, server = server)

