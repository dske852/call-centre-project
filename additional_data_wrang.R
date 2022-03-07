unique(final_data_all$CALLREASON)

final_data_wrangling%>%group_by(CALLREASON_update)%>%summarise(c=n())%>%mutate(perc=(c/sum(c))*100)%>%arrange(desc(perc))
  
final_data_wrangling%>%filter(CALLREASON_update == "Storing: internet")%>%ggplot(.,aes(x=timestamp, y=as.integer(FCR_24H_CLI)))+geom_bar(stat="identity")


unique(final_data_all$CALLS_GROUP_ID)

view(filter(final_data_all, CALLS_GROUP_ID == "2021-40_18556791"))
d76ae24495cd4726c9b1f7e398f56a4710f95ff12944c988526b49c0221bb4e7
view(filter(final_data_all, AO_KRN == "d76ae24495cd4726c9b1f7e398f56a4710f95ff12944c988526b49c0221bb4e7"))

u_id_repeats<-unique(filter(final_data_all, FCR_24H_CLI==0)$CALLS_GROUP_ID)
length(u_id_repeats)

fff<-filter(final_data_all, CALLS_GROUP_ID %in% u_id_repeats)

f0<-filter(fff, FCR_24H_CLI==0)
f1<-filter(fff, FCR_24H_CLI==1)

abc<-tibble()
for(id in unique(fff$CALLS_GROUP_ID)[1:5]){
  a<-filter(f0, CALLS_GROUP_ID==id)
  b<-filter(f1, CALLS_GROUP_ID==id)
  if(nrow(a) > 0 & nrow(b) > 0){
  if(a$CALLREASON == b$CALLREASON){
    c<-c("SAME")
  }else{c<-c("DIFFERENT")}
  
  data<-tibble(reason_same = c, id_cust=id, id_call=a$AO_transcription_id)
  abc<-rbind(abc, data)
  }else{next}
}



length(final_data_all$CALLS_GROUP_ID[which(final_data_all$CALLS_GROUP_ID=="2021-38_29223300")])

KZ_data<-filter(final_data_all, KZ==1)
KPNEEN<-filter(final_data_all, KZ==0)

u_id_repeats<-unique(filter(final_data_all, FCR_24H_CLI==0)$CALLS_GROUP_ID)
length(u_id_repeats)

fff<-filter(KZ_data, CALLS_GROUP_ID %in% u_id_repeats)

f0<-filter(fff, FCR_24H_CLI==0)
f1<-filter(fff, FCR_24H_CLI==1)

abc<-tibble()
for(id in unique(fff$CALLS_GROUP_ID)[1:5]){
  a<-filter(f0, CALLS_GROUP_ID==id)
  b<-filter(f1, CALLS_GROUP_ID==id)
  if(nrow(a) > 0 & nrow(b) > 0){
    if(a$CALLREASON == b$CALLREASON){
      c<-c("SAME")
    }else{c<-c("DIFFERENT")}
    
    data<-tibble(reason_same = c, id_cust=id, id_call=a$AO_transcription_id)
    abc<-rbind(abc, data)
  }else{next}
}


if(a$FCR_24H_CLI[1]==0){
  if(a$CALLREASON[1]==a$CALLREASON[2]){
    a24h<-c("SAME")
  }else(24h<-c("DIFF"))
}else if(a$FCR_7D_CLI[1]==0){
  
} 

if 1 rows = no recall
if 24H = 0, 7D = 0 , 14D=0 -> 1 + 3 rows

24 h<- row 1 == row 2 
7d <- row 1 == row 3 OR row 2 == row 3
14d <- 

if 24H = 0, 7D =0, 14D =1 -> 1+2 rows
if 24H=0, 7D=1, 14D = 1 -> 1 + 1 rows

3 variables: 24h, 7d, 14d for each call id  


for each observation if 24h 0 then +1 row 

KZ_data<-filter(KZ_data, CALLS_GROUP_ID != "Anonymous")
abc<-tibble()
for(id1 in unique(KZ_data$AO_KRN)[1:20]){
  a<-filter(KZ_data, AO_KRN==id1)
  if(nrow(a)>1){
  if(a$FCR_24H_CLI[1]==0 & a$FCR_24H_CLI[2]==1){
    if(a$CALLREASON[1]==a$CALLREASON[2]){
      reason24<-c("SAME")
    }else if(a$FCR_24H_CLI[1]==0 & a$FCR_24H_CLI[2]==0){
      for(row in 2:nrow(a)){
        if(row<nrow(a)){
        if(a$CALLREASON[row]==a$CALLREASON[row+1]){
        reason24<-c("SAME") 
        }else{reason24<-c("DIFF")}}else{break}
      }}
  }else if(a$CALLREASON[1]==a$CALLREASON[2]){reason24<-c("DIFF")}}else{next}
  data<-tibble(reason_24_same = reason24, id_group=id1, id_call=a$AO_transcription_id)
  abc<-rbind(abc, data)  
  }

view(filter(KZ_data, CALLS_GROUP_ID == "2021-40_39345273"))


KZ_data<-filter(KZ_data, CALLS_GROUP_ID != "Anonymous")


abc<-tibble()
for(id1 in unique(KZ_data$CALLS_GROUP_ID)[1:50]){
  a<-filter(KZ_data, CALLS_GROUP_ID==id1)%>%arrange(timestamp)
  if(nrow(a)>1 & nrow(a)<3){
      if(a$CALLREASON[1]==a$CALLREASON[2]){
        reason24<-c("SAME")}
        else if(a$CALLREASON[1]!=a$CALLREASON[2]){
          reason24<-c("DIFF")
        }else if(nrow(a)>2){
          if()
        }
    }else{next}
  data<-tibble(reason_24_same = reason24, id_group=id1, id_call=a$AO_transcription_id[1], rows = nrow(a), dupl_id_group=sum(duplicated(a$CALLS_GROUP_ID)), dupl_id_cust=sum(duplicated(a$AO_KRN))) 
  abc<-rbind(abc, data)
}



(KZ_data$timestamp[1])+lubridate::hours(24)


t2<-tibble()
vecc<-c()
for(id in unique(final_data_all$CALLS_GROUP_ID)){
  
  a<-filter(final_data_all, CALLS_GROUP_ID==id)%>%arrange(timestamp)
  if(nrow(a)>1){
  vecc<-c()
  for(row in c(1:c(nrow(a)-1))){
     time1<-as.numeric(difftime(a$timestamp[row+1],a$timestamp[row]), units="hours")
     vecc<-c(vecc, time1)
    
  }
  }else{next}
  vecc<-c(0, vecc)
  tib<-tibble(group_id=id, call_id=a$AO_transcription_id,vecc)
  t2<-rbind(t2, tib)
  print("done")
}

true_recall_data<-filter(final_data_all, CALLS_GROUP_ID != "Anonymous")

t5<-tibble()
for(id in unique(true_recall_data$CALLS_GROUP_ID)){
a<-filter(true_recall_data, CALLS_GROUP_ID==id)%>%arrange(timestamp)
ab<-a[which(duplicated(a$CALLREASON)==T),]
a<-filter(a, CALLREASON %in% unique(ab$CALLREASON))
tib<-tibble(group_id=id, call_id=a$AO_transcription_id, reason=a$CALLREASON,Log_Level=a$LOG_LEVEL_2,product=a$SELECTED_PRODUCT,FCR_24H_CLI=a$FCR_24H_CLI, FCR_7D_CLI=a$FCR_7D_CLI, FCR_14D_CLI=a$FCR_14D_CLI, timestamp=a$timestamp)
t5<-rbind(t5, tib)
}

table(t5$FCR_24H_CLI)

t6<-tibble()
for(id in unique(true_recall_data$CALLS_GROUP_ID)){
  a<-filter(true_recall_data, CALLS_GROUP_ID==id)%>%arrange(timestamp)
  ab<-a[which(duplicated(a$LOG_LEVEL_2)==T),]
  a<-filter(a, LOG_LEVEL_2 %in% unique(ab$LOG_LEVEL_2))
  tib<-tibble(group_id=id, call_id=a$AO_transcription_id, reason=a$CALLREASON,Log_Level=a$LOG_LEVEL_2,product=a$SELECTED_PRODUCT,FCR_24H_CLI=a$FCR_24H_CLI, FCR_7D_CLI=a$FCR_7D_CLI, FCR_14D_CLI=a$FCR_14D_CLI, timestamp=a$timestamp)
  t6<-rbind(t6, tib)
}

table(t6$FCR_24H_CLI)

t7<-tibble()
for(id in unique(true_recall_data$CALLS_GROUP_ID)){
  a<-filter(true_recall_data, CALLS_GROUP_ID==id)%>%arrange(timestamp)
  ab<-a[which(duplicated(a$SELECTED_PRODUCT)==T),]
  a<-filter(a, SELECTED_PRODUCT %in% unique(ab$SELECTED_PRODUCT))
  tib<-tibble(group_id=id, call_id=a$AO_transcription_id, reason=a$CALLREASON,Log_Level=a$LOG_LEVEL_2,product=a$SELECTED_PRODUCT,FCR_24H_CLI=a$FCR_24H_CLI, FCR_7D_CLI=a$FCR_7D_CLI, FCR_14D_CLI=a$FCR_14D_CLI, timestamp=a$timestamp)
  t7<-rbind(t7, tib)
}

table(t7$FCR_24H_CLI)

t4<-tibble()
for(id in unique(true_recall_data$CALLS_GROUP_ID)){
  a<-filter(true_recall_data, CALLS_GROUP_ID==id)%>%arrange(timestamp)
  ab<-a[which(duplicated(a$CALLREASON)==T),]
  a<-filter(a, CALLREASON %in% unique(ab$CALLREASON) | CALLREASON == "nvt" | CALLREASON == "Overige")
  tib<-tibble(group_id=id, call_id=a$AO_transcription_id, reason=a$CALLREASON,Log_Level=a$LOG_LEVEL_2,product=a$SELECTED_PRODUCT,FCR_24H_CLI=a$FCR_24H_CLI, FCR_7D_CLI=a$FCR_7D_CLI, FCR_14D_CLI=a$FCR_14D_CLI, timestamp=a$timestamp)
  t4<-rbind(t4, tib)
}

t4<-tibble()
for(id in unique(true_recall_data$CALLS_GROUP_ID)){
  a<-filter(true_recall_data, CALLS_GROUP_ID==id)%>%arrange(timestamp)
  ab<-a[which(duplicated(a$CALLREASON)==T),]
  a<-filter(a, CALLREASON %in% unique(ab$CALLREASON) | CALLREASON == "nvt" | CALLREASON == "Overige")
  tib<-tibble(group_id=id, call_id=a$AO_transcription_id, reason=a$CALLREASON,Log_Level=a$LOG_LEVEL_2,product=a$SELECTED_PRODUCT,FCR_24H_CLI=a$FCR_24H_CLI, FCR_7D_CLI=a$FCR_7D_CLI, FCR_14D_CLI=a$FCR_14D_CLI, timestamp=a$timestamp)
  t4<-rbind(t4, tib)
}

table(t4$FCR_24H_CLI)

nrow(t6[which(unique(t6$call_id) %in% unique(t4$call_id)),])

fin<-filter(t4, Log_Level != "niet classificeerbaar" & Log_Level != "Niet Classificeerbaar")
table(fin$FCR_24H_CLI)

table(final_data_all$FCR_14D_CLI)

related_calls_dataset<-filter(final_data_all, AO_transcription_id %in% unique(t5$call_id))
table(related_calls_dataset$FCR_24H_CLI)

related_calls_dataset2<-tibble()
for(id in unique(related_calls_dataset$CALLS_GROUP_ID)){
  a<-filter(related_calls_dataset, CALLS_GROUP_ID==id)%>%arrange(timestamp)
  a<-a[nrow(a),]
  last_call<-1
  frame<-tibble(call_id=a$AO_transcription_id , last=last_call)
  related_calls_dataset2<-rbind(related_calls_dataset2, frame)
}

related_calls_dataset$FCR_24H_CLI <- ifelse(related_calls_dataset$AO_transcription_id %in% related_calls_dataset2$call_id, 1, related_calls_dataset$FCR_24H_CLI)
related_calls_dataset$FCR_7D_CLI <- ifelse(related_calls_dataset$AO_transcription_id %in% related_calls_dataset2$call_id, 1, related_calls_dataset$FCR_7D_CLI)
related_calls_dataset$FCR_14D_CLI <- ifelse(related_calls_dataset$AO_transcription_id %in% related_calls_dataset2$call_id, 1, related_calls_dataset$FCR_14D_CLI)

related_calls_dataset$back_office_repeated<-ifelse(related_calls_dataset$back_office_dummy==1 & (related_calls_dataset$FCR_24H_CLI == 0 | related_calls_dataset$FCR_7D_CLI == 0 
                                                                                           |related_calls_dataset$FCR_14D_CLI == 0),1,0 )
related_calls_dataset$mechanic_planned_repeated<-ifelse(related_calls_dataset$mechanic_planned==1 & (related_calls_dataset$FCR_24H_CLI == 0 | related_calls_dataset$FCR_7D_CLI == 0 
                                                                                           |related_calls_dataset$FCR_14D_CLI == 0),1,0 )
related_calls_dataset$solved_repeated<-ifelse(related_calls_dataset$solving==1 & (related_calls_dataset$FCR_24H_CLI == 0 | related_calls_dataset$FCR_7D_CLI == 0 
                                                                                                     |related_calls_dataset$FCR_14D_CLI == 0),1,0 )



#back office
agent$back_office<- ifelse(agent$`Dialogue Structure`=="back_office", 1, 0)
back <- as.data.frame(aggregate(back_office~GENESYS_CONVERSATION_ID, data = agent, FUN = sum))  
final_data_all <- left_join(final_data_all, back, "GENESYS_CONVERSATION_ID")
final_data_all$back_office <- ifelse(final_data_all$back_office==0, 0, 1)

#mechanic
agent$mechanic_planned<- ifelse(agent$`Dialogue Structure`=="mechanic_planned", 1, 0)
mechanic <- as.data.frame(aggregate(mechanic_planned~GENESYS_CONVERSATION_ID, data = agent, FUN = sum))
final_data_all <- left_join(final_data_all, mechanic, "GENESYS_CONVERSATION_ID")
final_data_all$mechanic_planned <- ifelse(final_data_all$mechanic_planned==0, 0, 1)

#problem solved
agent$solving<- ifelse(agent$`Dialogue Structure`=="solved", 1, 0)
solving <- as.data.frame(aggregate(solving~GENESYS_CONVERSATION_ID, data = agent, FUN = sum))
final_data_all <- left_join(final_data_all, solving, "GENESYS_CONVERSATION_ID")
final_data_all$solving <- ifelse(final_data_all$solving==0, 0, 1)

#back office on agent level
office <-aggregate(back_office~AO_GENESYS_EMPLOYEE_LOGIN, data = final_data_all, FUN = sum)
final_data_all <- full_join(final_data_all, office, "AO_GENESYS_EMPLOYEE_LOGIN")
final_data_all <- final_data_all %>% rename(back_office_total = back_office.y)
final_data_all <- final_data_all %>% rename(back_office_dummy = back_office.x)

calls <- as.data.frame(table(final_data_all$AO_GENESYS_EMPLOYEE_LOGIN))
calls <- calls %>% rename("AO_GENESYS_EMPLOYEE_LOGIN" = Var1)
final_data_all <- full_join(final_data_all, calls, "AO_GENESYS_EMPLOYEE_LOGIN")
final_data_all <- final_data_all %>% rename(total_call = Freq)
final_data_all$back_office_ratio <- final_data_all$back_office_total/final_data_all$total_call

 #consultation variable
  final_data_all$consultation <- ifelse(!(final_data_all$FIRST_CONSULTATED_NUMBER == "Onbekend"), 1, 0)
table(final_data_all$consultation)

#on agent level
consult <- aggregate(consultation~AO_GENESYS_EMPLOYEE_LOGIN, FUN = sum, data = final_data_all)
final_data_all <- full_join(final_data_all, consult, "AO_GENESYS_EMPLOYEE_LOGIN")
final_data_all <- final_data_all %>% rename(consultation_total = consultation.y)
final_data_all <- final_data_all %>% rename(consultation_dummy = consultation.x)
final_data_all$consultation_ratio <- final_data_all$consultation_total/final_data_all$total_call

#Transfer var (changed Iris' using FIRST_TRANSFER_VDN instead of IND_TRANSFER for consistency) #Could not see transfer var in final_wrangling scripts
final_data_all$transfer <- 0
final_data_all$transfer<- ifelse(!(final_data_all$FIFTH_TRANSFER_VDN== "Onbekend"), 5,
                     ifelse(!(final_data_all$FOURH_TRANSFER_VDN== "Onbekend"), 4, 
                            ifelse(!(final_data_all$THIRD_TRANSFER_VDN== "Onbekend"), 3,
                                   ifelse(!(final_data_all$SEC_TRANSFER_VDN== "Onbekend"), 2,
                                          ifelse(!final_data_all$FIRST_TRANSFER_VDN=="Onbekend", 1, 0)))))


#correct first transfer (I could add FCR conditions to measure if the transfer was actually successful, maybe FCR values for 24h being 1?)
#for call level first-call success:
final_data_all$first_transfer_success<- (ifelse(final_data_all$transfer==1, 1, 
                                    ifelse(final_data_all$transfer>1,0,-1))) #-1 means no transfer

table(df$first_transfer_success)
#merge
df <- df %>%
  dplyr::select(GENESYS_CONVERSATION_ID, first_transfer_success)

joined_data <- left_join(final_data_all, df, by = "GENESYS_CONVERSATION_ID")
