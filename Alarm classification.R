#Libraries used 
library(readr)
library(plotly)
library(compare)
library(tm)
library(DT)
library(wordcloud)
library(caret)
library(e1071)
library(gmodels)
library(RMySQL)
library(ssh)
#library(sendmailR)
library(mailR)
library(dplyr)
library(tidyverse)
library(pracma)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(ggplot2)
library(stringr)
library(stringi)
library(lubridate)
  

#Functions 
{
Inventory_records<-function(xmlpath)
{library(XML)
  library(plyr)
  library(reshape2)
  library(varhandle)
  doc <- xmlInternalTreeParse(xmlpath)
  rootNode <- xmlRoot(doc)
  #xmlSApply(rootNode,xmlValue)
  #xpathSApply(rootNode, "/cmData/managedObject",xmlValue)
  menusample <- xmlToList(rootNode,)
  inventory <- ldply(menusample$cmData[1:length(menusample$cmData)], ... = (stringsAsFactors = FALSE),data.frame)
  #View(inventory)
  inventory<-inventory[,6:18]
  inventory1 <- subset(inventory, inventory$p..attrs=="identificationCode"&inventory$.attrs!="1.0"&inventory$.attrs!="UNIT")
  inventory1<-inventory1[grep("PLMN",inventory1$.attrs),]
  inventory1<-unfactor(inventory1)
  inventory1$BSC<-str_extract_all(inventory1$.attrs, pattern = "BSC-[0-9]+")
  inventory1$BSC<-gsub("BSC-", "", inventory1$BSC, ignore.case = FALSE, perl = FALSE,
                fixed = FALSE, useBytes = FALSE)
  inventory1$BCF<-str_extract_all(inventory1$.attrs, pattern = "BCF-[0-9]+")
  inventory1$BCF<-gsub("BCF-", "", inventory1$BCF, ignore.case = FALSE, perl = FALSE,
                       fixed = FALSE, useBytes = FALSE)
  inventory1$MODULE<-str_extract_all(inventory1$.attrs, pattern = "UNIT-[0-9]+")
  inventory1$MODULE<-gsub("UNIT-", "", inventory1$MODULE, ignore.case = FALSE, perl = FALSE,
                       fixed = FALSE, useBytes = FALSE)
  return(inventory1)
}
Ftp<-function()
{library(RCurl)
  ftpUpload("Localfile.html", "ftp://User:Password@FTPServer/Destination.html")}
Labeler<- function(df,cleaned_data,sensitivity) 
{
  
  for (row in 1:nrow(df)) {
    text2<-df[row,"additional_text"]
    {for (row1 in 1:nrow(cleaned_data))
    {text1 <- cleaned_data[row1, "additional_text"]
    if (length(agrep(text1,text2,max.distance = sensitivity))!=0)
    {df[row,"Resolution"]<-cleaned_data[row1,"Resolution"]
    df[row,"Instruction"]<-cleaned_data[row1,"Instructions"]
    break}
    }}
  }
  return(df)
}
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
Db<-function(db)
{
  
  con <- dbConnect(RMySQL::MySQL(),
                   dbname="",
                   host="",
                   port=3306,
                   user="",
                   password="")
  #lapply(dbListConnections(MySQL()), dbDisconnect)
  on.exit(dbDisconnect(con))
  if (db==1)
  {res<-dbSendQuery(con,'SELECT * FROM bnnn WHERE alarm_number BETWEEN 76004 AND 76108 LIMIT 100000')}
  if (db==2)
  res<-dbSendQuery(con,'SELECT * FROM bnnn WHERE alarm_number BETWEEN 76004 AND 76107 AND event_time >2019-07-01')
  else
    print("Check your DB parameters")
  alarms<-dbFetch(res,n = -1)
  dbClearResult(res)
  return(alarms)
  #disconnect db 
  dbDisconnect(con)
  
  
}
Db_write<-function(alarms_data)
{con <- dbConnect(RMySQL::MySQL(),
                   dbname="bnbn",
                   host="x.x.x.x",
                   port=3306,
                   user="",password="")
    dbWriteTable(con, value = alarms_data, name = "Tabl", append = TRUE,overwrite=FALSE )
  dbDisconnect(con)}
Db_read<-function()
{con <- dbConnect(RMySQL::MySQL(),
                  dbname="bnbn",
                  host="x.x.x",
                  port=3306,
                  user="",password="")
on.exit(dbDisconnect(con))
res<-dbSendQuery(con,'SELECT * FROM nnn WHERE a BETWEEN 76004 AND 76107 LIMIT 100000')
#dbWriteTable(con, value = alarms_data, name = "TBL_ALARM", append = TRUE )

alarms<-dbFetch(res,n = -1)
dbClearResult(res)
return(alarms)
dbDisconnect(con)}
ssh_connector<-function(hostip,issued_command)
{

  host<-hostip
  session <- ssh_connect(host, keyfile = NULL, passwd = , verbose = FALSE)
  ssh_session_info(session)
  ssh_exec_wait(session, command = "")
  ssh_exec_wait(session, command = "")

  ssh_exec_wait(session, command = issued_command)
  ssh_disconnect(session)
  
}
BSC_Details<-function(df5){
  #df5$BSC<-str_split_fixed(df5$probable_cause, "/",1)
  df5$BSC<-str_extract_all(df5$probable_cause, pattern = "BSC-[0-9]+")
  df5$BSC<-gsub("BSC-", "", df5$BSC, ignore.case = FALSE, perl = FALSE,
                fixed = FALSE, useBytes = FALSE)
  #BSC, BCF,BTS information 
  df5$BCF<-str_extract_all(df5$probable_cause, pattern = "BCF-[0-9]+")
  df5$BCF<-gsub("BCF-", "", df5$BCF, ignore.case = FALSE, perl = FALSE,
                fixed = FALSE, useBytes = FALSE)
  df5$BTS<-str_extract_all(df5$probable_cause, pattern = "BTS-[0-9]+")
  df5$BTS<-gsub("BTS-", "", df5$BTS, ignore.case = FALSE, perl = FALSE,
                fixed = FALSE, useBytes = FALSE)
  df5$BTS<-gsub("character(0)", "NA", df5$BTS, ignore.case = FALSE, perl = FALSE,
                fixed = FALSE, useBytes = FALSE)
  df5$TRX<-str_extract_all(df5$probable_cause, pattern = "TRX-[0-9]+")
  df5$TRX<-gsub("TRX-", "", df5$TRX, ignore.case = FALSE, perl = FALSE,
                fixed = FALSE, useBytes = FALSE)
  df5$BSC<-sapply(df5$BSC, as.numeric)
  df5$BCF<-sapply(df5$BCF, as.numeric)
  df5$BTS<-sapply(df5$BTS, as.numeric)
  df5$TRX<-sapply(df5$TRX, as.numeric)
  df5$Date<-as.Date(df5$event_time)
  df5$time<-format(as.POSIXct(df5$event_time),format="%H:%M")
  return(df5)
}
emailer<-function(destination_address,cc,alarm_text,Object_name,Instructions){
  from <- "x@gmail.com"
  to <- destination_address
  #to<-destination_address
  subject <-alarm_text
  body <- paste("Hi ", "\n", "kindly resolve  below alarm:","\t",Object_name,"\n","Resolution steps","\n",Instructions,"\n","Thanks in advance")                  
  mailControl=list(host.name ="",port=587,user.name="xxxx",passwd="!@$@#$",authenticate=TRUE,SSL=FALSE,send=TRUE)
  #sendmail(from=from,to=to,cc=cc,subject=subject,msg=body,control=mailControl1)
  send.mail(from, to, cc=cc,subject = subject, body = body, encoding = "iso-8859-1",
            html = FALSE, inline = FALSE, smtp = mailControl, authenticate = TRUE,
            send = TRUE, attach.files = NULL, debug = FALSE)
 
  
}
Cost<-function(resolved_alarms_count)
{
  basic_salary<-160
  weekday_bi_hourly_rate<-(basic_salary/(24*8)*2)
  weekend_bi_hourly_rate<-weekday_bi_hourly_rate*2
  evening_bi_hourly_rate<-weekday_bi_hourly_rate*1.5
  # security
  escort_fees<-1234
  
  weekend_savings<-(8/30)*weekday_bi_hourly_rate*resolved_alarms_count
  evening_savings<-(11/30*evening_bi_hourly_rate)*resolved_alarms_count
  daytime_savings<-11/30*weekday_bi_hourly_rate*resolved_alarms_count
  total_savings<-sum(weekend_savings,evening_savings,daytime_savings)
  escort_savings<-(11/30+4/30)*resolved_alarms_count*1234
  fuel_cost_per_litre<-100
  distance_to_site<-50
  #average_revenue_per_hour<-1000
  consumption_per_km<-0.1
  Mileage_savings<-(fuel_cost_per_litre*distance_to_site*2*consumption_per_km*resolved_alarms_count)
  total_savings<-(Mileage_savings+total_savings+escort_savings)
  return(total_savings)
}
Text_clean<-function(additional_text,sms_dict){
  
  
  df_corpus4 <- Corpus(VectorSource(additional_text))
  corpus_clean4 <- tm_map(df_corpus4, tolower)
  corpus_clean4 <- tm_map(corpus_clean4, removeNumbers)
  corpus_clean4 <- tm_map(corpus_clean4, removeWords, stopwords())
  corpus_clean4 <- tm_map(corpus_clean4, removePunctuation)
  corpus_clean4 <- tm_map(corpus_clean4, stripWhitespace)
  
  df4_dtm <- DocumentTermMatrix(corpus_clean4)
  df4_test  <- DocumentTermMatrix(corpus_clean4, list(dictionary = sms_dict))
  df4_actual <- df4_dtm %>%
    apply(MARGIN = 2, convert_counts)
  return( df4_actual)
}}

##spares module& BSC details
{BSC_info <- read_csv("BSC info.csv" )}
#{Spare_details<-Inventory_records("inventory.xml")}
## model training 
{df2 <- read_csv("cleaned data2.csv")
df <- subset(df2, select = c("additional_text","Resolution"))
#convert target classes to Categorical variables 
df$Resolution<-factor(df$Resolution)
#Get text data for training and testing 
df_corpus <- Corpus(VectorSource(df$additional_text))
#Text cleaning : Removal of stopwords and whitespaces... punctuations 
corpus_clean <- tm_map(df_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
#create train  matrix 
Train_dtm <- DocumentTermMatrix(corpus_clean)
#Extractions of key worrds 
sms_dict <- findFreqTerms(Train_dtm, 5)
sms_train <- DocumentTermMatrix(corpus_clean, list(dictionary = sms_dict))
train <- Train_dtm %>%
  apply(MARGIN = 2, convert_counts)
#prop.table(table(df$Resolution))
Text_classifier <- naiveBayes(train, df$Resolution)
print(Text_classifier)
}

#Fetch active alarms 

active_alarms<-Db(1)
Ceased<-Db_read()
#active_alarms<-Ceased


active_alarms$Resolution<-"dummy"
active_alarms$Instruction<-"dummy"
active_alarms<-Labeler(active_alarms,df2,0.1)
Missing_in_Alarm_DB<-active_alarms[active_alarms$Resolution == "dummy", ]
write_csv(Missing_in_Alarm_DB,"missing_alarms.csv")
active<-Text_clean(Missing_in_Alarm_DB$additional_text,sms_dict)
try(active_pred<-predict(Text_classifier, active),silent = TRUE)
try(Missing_in_Alarm_DB$Resolution<-active_pred,silent = TRUE)
try(active_alarms<-active_alarms[!active_alarms$Resolution=="dummy",],silent = TRUE)
try(active_alarms<-rbind(active_alarms,Missing_in_Alarm_DB),silent = TRUE)

#hashed


#Corr1<-read_csv("Correlared_alarms.csv" )
df5<-BSC_Details(active_alarms)
#df5<-BSC_Details(Corr1)
df5<-subset(df5, select=c("alarmid","additional_text","Resolution" ,"Instruction","symbolic_name", "event_time","alarm_name","probable_cause","specific_problem","alarm_number", 'BSC',"BCF","BTS", "TRX","Date" ,"time"  ))
df5<-ungroup(df5)
df3<-nest(group_by(df5,df5$BSC,df5$BCF,df5$Date,df5$time))
Correlared_alarms<-df5[FALSE,]
y=1
while (y<nrow(df3)) {
  dfx<-df3$data[[y]]
  #df3$sum1[y]<-min(dfx[,"alarm_number"]) 
  df9<-dfx[dfx$alarm_number==min(dfx[,"alarm_number"]),]
  df9
  Correlared_alarms[y,]<-df9[1,]
  #dft<-df3[,1:4]
  #dft[y,4:ncol(dfm)]<-cbind(dft[y,],dfm)

  y=y+1
}
#View(Correlared_alarms)
##Write correlated alarms to Database 

Correlared_alarms<-subset(Correlared_alarms, select=c("alarmid", "additional_text", "Resolution", "symbolic_name",
                                                      "event_time", "alarm_name", "probable_cause", "specific_problem",
                                                  "alarm_number", "BCF", "BTS", "TRX", "BSC","Date","time"))
# Ceased<-subset(Ceased, select=c("alarmid", "additional_text", "Resolution", "symbolic_name",
#                                                       "event_time", "alarm_name", "probable_cause", "specific_problem",
#                                                       "alarm_number", "BCF", "BTS", "TRX", "BSC","Date","time","Status","Responsible_Engineer","Site_ID", "Escalation_Date",))
require(data.table)
dt1 <- data.table(Correlared_alarms, key="alarmid")
dt2 <- data.table(Ceased, key="alarmid")
New<-dt1[!dt2]

  resolved<-dt2[!dt1]
dt1 <- data.table(Correlared_alarms, key="alarmid")
dt2 <- data.table(New, key="alarmid")
Pending<-dt1[!dt2]

New$Status<-"New"
Pending$Status<-"Pending"
resolved$Status<-"Resolved"
 New$Escalation_Date<- NULL
 Pending$Escalation_Date<- NULL
#final=rbindlist(New,Pending,resolved,fill =FALSE )
#final2<-rbindlist(final,resolved)

New$Responsible_Engineer<-"System Resolved"
#




row=1
while (row<nrow(New)) {
  if (New$Resolution[row]=="Configuration") {
    New$Responsible_Engineer[row]<-BSC_info$`Support engineer`[BSC_info$`BSC NAME`==New$specific_problem[row]]
    
  }
  if ((New$Resolution[row]=="F/O visit")||(New$Resolution=="Replace RF")||(New$Resolution=="Replace module")||(New$Resolution=="Replace EAC")) {
    New$Responsible_Engineer[row]<-BSC_info$`Field engineer`[BSC_info$`BSC NAME`==New$specific_problem[row]]
    
  }
  New$Site_ID[row]<-gsub("(.*)(.*)(.*)(.*)(.*)(.*)_.*","\\1",New$symbolic_name[row])
  New$Escalation_Date[row]<-Sys.time()
  
  row<-row+1
}
row=1
while (row <= nrow(New)) {
  y<-str_split(as.character(New$additional_text[row])," ")
  y<-data.frame(matrix(unlist(y), nrow=length(y), byrow=T))
  y <- data.frame(lapply(y, as.character), stringsAsFactors=FALSE)
  y<-y[1:(ncol(y)-6)]
  New$additional_text[row]<-apply(y, 1, paste, collapse=" ")
  nrow=0
  row<-row+1
}
Db_write(New)

row=1
while (row<nrow(Pending)) {
  if (Pending$Resolution[row]=="Configuration") {
    Pending$Responsible_Engineer[row]<-BSC_info$`Support engineer`[BSC_info$`BSC NAME`==Pending$specific_problem[row]]
    
  }
  if ((Pending$Resolution[row]=="F/O visit")||(Pending$Resolution=="Replace RF")||(Pending$Resolution=="Replace module")||(Pending$Resolution=="Replace EAC")) {
    Pending$Responsible_Engineer[row]<-BSC_info$`Field engineer`[BSC_info$`BSC NAME`==Pending$specific_problem[row]]
    
  }
  Pending$Site_ID[row]<-gsub("(.*)(.*)(.*)(.*)(.*)(.*)_.*","\\1",Pending$symbolic_name[row])
  Pending$Escalation_Date<-Sys.time()
  row<-row+1
}
row=1
while (row <= nrow(Pending)) {
  y<-str_split(as.character(Pending$additional_text[row])," ")
  y<-data.frame(matrix(unlist(y), nrow=length(y), byrow=T))
  y <- data.frame(lapply(y, as.character), stringsAsFactors=FALSE)
  y<-y[1:(ncol(y)-6)]
  Pending$additional_text[row]<-apply(y, 1, paste, collapse=" ")
  nrow=0
  row<-row+1
}
Db_write(Pending)





#write_csv(Correlared_alarms,"Correlared_alarms.csv")
df_replace_module<-Correlared_alarms[Correlared_alarms$Resolution=="Replace module",]
df_replace_RF<-Correlared_alarms[Correlared_alarms$Resolution=="Replace RF",]
df_Site_visit<-Correlared_alarms[Correlared_alarms$Resolution=="F/O visit",]
df_configuration<-Correlared_alarms[Correlared_alarms$Resolution=="Configuration",]
df_BCF_reset<-Correlared_alarms[Correlared_alarms$Resolution=="BCF Reset",]
df_BTS_reset<-Correlared_alarms[Correlared_alarms$Resolution=="Reset BTS",]
df_Replace_alarm_box<-Correlared_alarms[Correlared_alarms$Resolution=="Replace EAC",]
df_Hard_restart<-Correlared_alarms[Correlared_alarms$Resolution=="H/W reset",]
##Module escalations 
row=1
while (row <=nrow(df_replace_RF)){
  emailer("x@gmail.com","v@gmail.com",df_replace_RF$additional_text[row],df_replace_RF$symbolic_name[row])
  #df_replace_module$recurrency[row]<-count(df_replace_module$affected_NE[row])
  row<-row+1
  Sys.sleep(13)
}
row=1
while (row <=nrow(df_replace_module)){
  emailer("x@gmail.com","s@gmail.com",df_replace_module$additional_text[row],df_replace_module$symbolic_name[row])
  #df_replace_module$recurrency[row]<-count(df_replace_module$affected_NE[row])
  row<-row+1
  Sys.sleep(13)
}
row=1
  while (row <=nrow(df_Replace_alarm_box)){
  emailer(BSC_info$`Support engineer`[BSC_info$`BSC NAME`==df_Replace_alarm_box$specific_problem[row]],BSC_info$`Field engineer`[BSC_info$`BSC NAME`==df_Replace_alarm_box$specific_problem[row]],df_Replace_alarm_box$additional_text[row],df_Replace_alarm_box$symbolic_name[row],Spare_details[Spare_details$BCF=="52"&Spare_details$BSC=="395536"&Spare_details$p.text.5=="FXEB",c("p..attrs","p.text","p..attrs.3","p.text.3","p.text.5")])
  row<-row+1
  Sys.sleep(13)
}
##Support escalations
row=1
while (row <=nrow(df_configuration)){
  emailer(BSC_info$`Support engineer`[BSC_info$`BSC NAME`==df_configuration$specific_problem[row]],BSC_info$`Field engineer`[BSC_info$`BSC NAME`==df_configuration$specific_problem[row]],df_configuration$additional_text[row],df_configuration$symbolic_name[row])
  #df_replace_module$recurrency[row]<-count(df_replace_module$affected_NE[row])
  row<-row+1
  Sys.sleep(12)
}

## Field escalations 
row=1
while (row <=nrow(df_Site_visit)){

  row<-row+1
  Sys.sleep(12)
}



