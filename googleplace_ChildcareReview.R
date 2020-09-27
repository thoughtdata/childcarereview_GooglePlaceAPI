#install.packages("googleway")
#install.packages("leaflet")
#install.packages("RColorBrewer")
#install.packages("zipcode")
library(googleway)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(dplyr)
library(rvest)
library(zipcode)
library(rlist)
library(plyr)

api_key="----------"
df_care<-google_places(search_string = "childcare", location=c(40.746557, -73.930849) , radius=500, key=api_key)

data(zipcode)

############################
##Need to do one loop here##
##10001-11104##           ##
############################

data=data.frame(zipcode)
NYCzip_list<-c(zipcode$zip[zipcode$city=="New York"])

for (i in NYCzip_list ){
  api_key <- "AIzaSyBuAVxBB6FcTmBnVvx8KRUrD9_HYs7_U2c"
  lat_long <- google_geocode("new york city",key = api_key)
  lng_lat <- c(lat_long$results$geometry$location$lng[1],lat_long$results$geometry$location$lat[1])
  Names <- paste("child care in New York" , i)
  ccare<- google_places(search_string=Names, 
                               radius=10000, 
                               key=api_key)
  place_details<- ccare$results$place_id %>% 
    map(~google_place_details(.,key = api_key))
  dataframe_1<-place_details %>% 
    map(~`[[`(.,"result")) %>% 
    map(~`[[`(.,"reviews")) %>% 
    map(~as_tibble(.))
  
  
  dataframe_2<-place_details%>% 
    map(~`[[`(.,"result")) %>% 
    map(~`[[`(.,"formatted_address")) %>% 
    map(~as_tibble(.))
  
  #dataframe_3<-place_details %>% 
 #   map(~`[[`(.,"result")) %>% 
  #  map(~`[[`(.,"formatted_phone_number")) %>% 
   # map(~as_tibble(.)) 
  
  dataframe_4<-place_details %>% 
    map(~`[[`(.,"result")) %>% 
    map(~`[[`(.,"user_ratings_total")) %>% 
    map(~as_tibble(.)) 
 
  dataframe_5<-place_details %>% 
    map(~`[[`(.,"result")) %>% 
    map(~`[[`(.,"geometry")) %>% 
    map(~`[[`(.,"location")) %>% 
    map(~as_tibble(.)) 
  
  dataframe_6<-place_details %>% 
    map(~`[[`(.,"result")) %>% 
    map(~`[[`(.,"rating")) %>% 
    map(~as_tibble(.)) 
  #dataframe_6[index1][[1]] %>% 
  #  rename("average_rating" = "value"  )
  
  index1 <- dataframe_1 %>% 
    map_lgl(~ncol(.)>0  )
  
  index2 <- dataframe_2 %>% 
    map_lgl(~ncol(.)>0  )
  
 # index3 <- dataframe_3 %>% 
 #   map_lgl(~ncol(.)>0  )
  
  index4 <- dataframe_4 %>% 
    map_lgl(~ncol(.)>0  )
  
  index5 <- dataframe_5 %>% 
    map_lgl(~ncol(.)>0  )
  
  index6 <- dataframe_6 %>% 
    map_lgl(~ncol(.)>0  )
  

  dataframenotmissing  <- dataframe_1[index1]
  if((length(which(!index2)) == 0)==FALSE){
    dataframe_2[[which(!index2)]]<-as.tibble(as.character(dataframe_2[!index2] == NA))
  } 
  dataframenotmissing2 <- dataframe_2[index1]%>% 
    map(~rename(.,address=value))
  #if((length(which(!index3)) == 0)==FALSE){
   # dataframe_3[[which(!index3)]]<-as.tibble(as.character(dataframe_3[!index3] == NA))
 # }
  
 # dataframenotmissing3 <-dataframe_3[index1] 
   # map(~rename(.,pnumber=value))
   dataframenotmissing4 <- dataframe_4[index1]%>% 
    map(~rename(.,userratingstotal=value))
  dataframenotmissing5 <- dataframe_5[index1]
  dataframenotmissing6 <- dataframe_6[index1]%>% 
    map(~rename(.,average_rating=value))
  
    test<-dataframenotmissing %>% 
    map2(.y = dataframenotmissing2,~cbind(.x,.y)) %>% 
 #   map2(.y = dataframenotmissing3,~cbind(.x,.y)) %>% 
    map2(.y = dataframenotmissing4,~cbind(.x,.y)) %>% 
    map2(.y = dataframenotmissing5,~cbind(.x,.y)) %>% 
    map2(.y = dataframenotmissing6,~cbind(.x,.y)) %>% 
    bind_rows() 
    # bind rows together
    assign((paste("childcare",sep="", i)), test )
}

class(NYCzip_list)
list_of_objects <- mget(ls(.GlobalEnv, pattern = "childcare"), envir = .GlobalEnv)
list_of_objects <- mget(ls(pattern="childcare"))
bdata<-do.call("rbind.fill", list_of_objects)
rownames(bdata) <- c()
DFlist_New<-distinct(bdata)

write.csv(DFlist_New, file = "DFlist_New.csv")
DFlist_New
