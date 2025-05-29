#-----------------------------------------------
#section 1 : Install & load li

install.packages(c("dygraphs","plotly","xts"))

library(dygraphs)
library(plotly)
library(xts)
library(tidyverse)

#time series vizualization 
#eg 01 simple time series data

Nile_ts <-as.xts(Nile)
dygraph(Nile_ts,main = "Nile River Flow")


"city_day.csv" %>% 
  read_csv() %>% 
  select(c(Date,NO)) %>% 
  dygraph(main = "Time series data")

#example 2 Add range selector

nile_graph <- dygraph(Nile_ts,main = "Nile River Flow with Range") %>% 
  dyRangeSelector()



#Example 3 Multiple Series 
mdeaths
fdeaths
data<- cbind(mdeaths,fdeaths)
data_xts <- as.xts(data)
dygraph(data_xts,main = "Death in the UK") %>% 
  dySeries("mdeaths",label = "male") %>% 
  dySeries("fdeaths", label = "female") %>% 
  dyOptions(colors = c("red","green")) %>% 
  dyRangeSelector()

lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(as.xts(lungDeaths), main = "comparing deaths of male and femail") %>% 
  dySeries("mdeaths", color = "darkblue") %>% 
  dySeries("fdeaths", color = "tomato") %>% 
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2) %>% 
  dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
  dyLegend(show = "auto") %>% 
  dyRangeSelector()
  
  #"auto", "always ,"anmouseover", "follow", "never"




"city_day.csv" %>% 
  read_csv() %>% 
  select(c(Date,NO,NO2)) %>% 
  dygraph(main = "Comparision Time series") %>% 
  dyRangeSelector()





#covert ggplot to plotly  example 1

ggplot(mtcars,aes(x=wt,y= mpg,colour = factor(cyl)))+
  geom_point(size=3)+
  labs(title = "miles per gallon vs weight",
       x="weight",y="mpg")->plot_p
ggplotly(plot_p)



#example_2
















