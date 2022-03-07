library(stringr)
library(lubridate)



final_data_select<-related_calls_dataset%>%dplyr::select(AO_transcription_id, BadEmotionsScore_Cust, change_start, change_start_end, suming, IND_TRANSFER, SERVICES_NAME, LAST_SKILL_NUM, NUM_SEC_CONTACT, NUM_SEC_ON_HOLD, NUM_SEC_WRAPUP,NUM_SEC_TALK_TIME,AGENT_WORDLOAD,CUSTOMER_WORDLOAD,PARTY_ENDING_CONNECTION, AO_GENESYS_EMPLOYEE_LOGIN, GENESYS_CONVERSATION_ID, AO_KRN, IND_BAD_PROBLEM_SOLVING, SERVICE_DESK, KPNEEN, KZ, NEGATIVITY, FCR_24H_CLI, FCR_7D_CLI, FCR_14D_CLI, timestamp, CALLREASON, negative_ratio, agent_independence, Skill_Emotion, agent_personal_score_happy, back_office_dummy, mechanic_planned, agent_personal_score_angry, back_office_ratio, consultation_dummy, consultation_ratio, mechanic_planned_repeated, back_office_repeated, solving, solved_repeated, transfer, first_transfer_success )
final_data_select<-final_data_all%>%dplyr::select(AO_transcription_id, BadEmotionsScore_Cust, change_start, change_start_end, suming, IND_TRANSFER, SERVICES_NAME, LAST_SKILL_NUM, NUM_SEC_CONTACT, NUM_SEC_ON_HOLD, NUM_SEC_WRAPUP,NUM_SEC_TALK_TIME,AGENT_WORDLOAD,CUSTOMER_WORDLOAD,PARTY_ENDING_CONNECTION, AO_GENESYS_EMPLOYEE_LOGIN, GENESYS_CONVERSATION_ID, AO_KRN, IND_BAD_PROBLEM_SOLVING, SERVICE_DESK, KPNEEN, KZ, NEGATIVITY, FCR_24H_CLI, FCR_7D_CLI, FCR_14D_CLI, timestamp, CALLREASON, negative_ratio, agent_independence, Skill_Emotion, agent_personal_score_happy, back_office_dummy, mechanic_planned, agent_personal_score_angry, back_office_ratio, consultation_dummy, consultation_ratio, solving, transfer, first_transfer_success )

#final_data_select<-final_data_all%>%dplyr::select(AO_transcription_id, BadEmotionsScore_Cust, change_start, change_start_end, suming, IND_TRANSFER, SERVICES_NAME, LAST_SKILL_NUM, NUM_SEC_CONTACT, NUM_SEC_ON_HOLD, NUM_SEC_WRAPUP,NUM_SEC_TALK_TIME,AGENT_WORDLOAD,CUSTOMER_WORDLOAD,PARTY_ENDING_CONNECTION, AO_GENESYS_EMPLOYEE_LOGIN, GENESYS_CONVERSATION_ID, AO_KRN, IND_BAD_PROBLEM_SOLVING, SERVICE_DESK, KPNEEN, KZ, NEGATIVITY, FCR_24H_CLI, FCR_7D_CLI, FCR_14D_CLI, timestamp, CALLREASON, negative_ratio, agent_independence, Skill_Emotion, agent_personal_score_happy, back_office_dummy, mechanic_planned, agent_personal_score_angry, back_office_ratio, consultation_dummy, consultation_ratio)

#final_data_join<-left_join(final_data_select, churn_orders_pseudonymized_2, by=c("AO_KRN"="CCKKR_krn_id") )
#final_data_join<-left_join(final_data_select, churn_orders_pseudonymized_2, by=c("AO_KRN"="krn") )

#saveRDS(final_data_select, "C:/Users/baumg/OneDrive/MsC DS&MA/KPN/outputs/final_datasets/final_data_select.RDS")
#saveRDS(final_data_all, "C:/Users/baumg/OneDrive/MsC DS&MA/KPN/outputs/final_datasets/final_data_all.RDS")
#saveRDS(related_calls_dataset, "C:/Users/baumg/OneDrive/MsC DS&MA/KPN/outputs/final_datasets/related_calls_dataset.RDS")

final_data_wrangling<-final_data_select%>%mutate_if(is.character, as.factor)
final_data_wrangling<-final_data_wrangling%>%rename(BadEmotionsScore_Agent = suming, Emotions_Start=change_start, Emotions_Start_End=change_start_end)

final_data_wrangling<-filter(final_data_wrangling, FCR_14D_CLI != -2)

final_data_wrangling$FCR_24H_CLI<-as.factor(ifelse(final_data_wrangling$FCR_24H_CLI==1,0,ifelse(
  final_data_wrangling$FCR_24H_CLI==0, 1, final_data_wrangling$FCR_24H_CLI)))

final_data_wrangling$FCR_24H_CLI<-relevel(final_data_wrangling$FCR_24H_CLI, "0")

final_data_wrangling$FCR_7D_CLI<-as.factor(ifelse(final_data_wrangling$FCR_7D_CLI==1,0,ifelse(
  final_data_wrangling$FCR_7D_CLI==0, 1, final_data_wrangling$FCR_7D_CLI)))
final_data_wrangling$FCR_7D_CLI<-relevel(final_data_wrangling$FCR_7D_CLI, "0")

final_data_wrangling$FCR_14D_CLI<-as.factor(ifelse(final_data_wrangling$FCR_14D_CLI==1,0,ifelse(
  final_data_wrangling$FCR_14D_CLI==0, 1, final_data_wrangling$FCR_14D_CLI)))
final_data_wrangling$FCR_14D_CLI<-relevel(final_data_wrangling$FCR_14D_CLI, "0")
str(final_data_wrangling)
#dropping unknown recalls

table(final_data_wrangling$FCR_24H_CLI)
table(final_data_wrangling$FCR_7D_CLI)
table(final_data_wrangling$FCR_14D_CLI)

#charactericts
summary(final_data_wrangling)
sd(final_data_wrangling$NUM_SEC_CONTACT)
sd(final_data_wrangling$NUM_SEC_ON_HOLD)
sd(final_data_wrangling$NEGATIVITY)
#what is minus in num_sec_on_hold

#replacing weird durations values
final_data_wrangling$NUM_SEC_ON_HOLD<-ifelse(final_data_wrangling$NUM_SEC_ON_HOLD<0,0,final_data_wrangling$NUM_SEC_ON_HOLD)
final_data_wrangling$NUM_SEC_CONTACT<-ifelse(final_data_wrangling$NUM_SEC_CONTACT<0,0,final_data_wrangling$NUM_SEC_CONTACT)
final_data_wrangling$NUM_SEC_WRAPUP<-ifelse(final_data_wrangling$NUM_SEC_WRAPUP<0,0,final_data_wrangling$NUM_SEC_WRAPUP)
final_data_wrangling$NUM_SEC_TALK_TIME<-ifelse(final_data_wrangling$NUM_SEC_TALK_TIME<0,0,final_data_wrangling$NUM_SEC_TALK_TIME)

summary(final_data_wrangling)
#what is minus in num_sec_on_hold ????

#checking outliers
boxplot(final_data_wrangling$NUM_SEC_ON_HOLD)
summary(final_data_wrangling$NUM_SEC_ON_HOLD)
length(final_data_wrangling$NUM_SEC_ON_HOLD[which(final_data_wrangling$NUM_SEC_ON_HOLD>500)])
view(filter(final_data_wrangling, NUM_SEC_ON_HOLD>500))
#new variable for ON HOLD, replacing >500sec & NO RECALL with NAs
final_data_wrangling$NUM_SEC_ON_HOLD_update<-ifelse(final_data_wrangling$NUM_SEC_ON_HOLD>500 & final_data_wrangling$FCR_24H_CLI == 0,NA,final_data_wrangling$NUM_SEC_ON_HOLD)

#distribution call duration
boxplot(final_data_wrangling$NUM_SEC_CONTACT)

#Creating CALL REASON levels
final_data_wrangling%>%group_by(CALLREASON)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))


final_data_wrangling$CALLREASON_update<-ifelse(final_data_wrangling$CALLREASON == "Overige" | final_data_wrangling$CALLREASON == "Betaling & factuur" |
                                                 final_data_wrangling$CALLREASON == "Opzeggen"| final_data_wrangling$CALLREASON == "Verkoop" | 
                                                 final_data_wrangling$CALLREASON == "Order capture & fulfillment: overige" |  final_data_wrangling$CALLREASON == "Storing: internet"|
                                                 final_data_wrangling$CALLREASON == "Storing: telefonie",as.character(final_data_wrangling$CALLREASON), "Other" )

final_data_wrangling$CALLREASON_update<-as.factor(final_data_wrangling$CALLREASON_update)

#party ending relevel
final_data_wrangling$PARTY_ENDING_CONNECTION<-relevel(as.factor(final_data_wrangling$PARTY_ENDING_CONNECTION), "Onbekend")


#Preparing time/date data

class(final_data_wrangling$timestamp)
class(as.character(final_data_wrangling$timestamp))
time<- str_split(as.character(final_data_wrangling$timestamp), " " , simplify = T)
final_data_wrangling$hour<-time[,2]
final_data_wrangling$day<-time[,1]

final_data_wrangling$day_name<-as.Date(final_data_wrangling$day)
final_data_wrangling$day_name<-weekdays(final_data_wrangling$day_name)

time2<- str_split(as.character(final_data_wrangling$day), "-" , simplify = T)
time2<-as.data.frame(time2)
unique(filter(time2, V2==12))

final_data_wrangling$month<-time2[,2] #December until 16th

final_data_wrangling$day_num<-time2[,3]

str(final_data_wrangling)


hours_lubri<-lubridate::hms(final_data_wrangling$hour)
unique(final_data_wrangling$hour)
final_data_wrangling$periods<-ifelse(hours_lubri >= "8H 30M 0S" & hours_lubri <= "11H 0M 0S", "8.30AM-11AM",
                                  ifelse(hours_lubri > "11H 0M 0S" & hours_lubri <= "14H 00M 0S", "11AM-2PM",
                                         ifelse(hours_lubri > "14H 0M 0S" & hours_lubri <= "17H 0M 0S", "2PM-5PM",
                                                ifelse( hours_lubri > "17H 0M 0S" & hours_lubri <= "20H 0M 0S", "5PM-8PM",
                                                        ifelse(hours_lubri > "20H 0M 0S" & hours_lubri <= "23H 0M 0S", "late-evening-night",
                                                               ifelse(hours_lubri >= "6H 0M 0S" & hours_lubri < "8H 30M 0S", "6AM-8.30AM","late-evening-night"))))))

final_data_wrangling$periods<-as.factor(final_data_wrangling$periods)
final_data_wrangling$day_name<-as.factor(final_data_wrangling$day_name)

##calculating proportions
summary(as.factor(final_data_wrangling$IND_TRANSFER))
summary(as.factor(final_data_wrangling$IND_BAD_PROBLEM_SOLVING))
summary(as.factor(final_data_wrangling$FCR_24H_CLI))

final_data_wrangling%>%group_by(periods)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))
final_data_wrangling%>%group_by(day_name)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))
final_data_wrangling%>%group_by(CALLREASON_update)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))
final_data_wrangling%>%group_by(Emotion)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))



#####PLOTS#####

final_data_wrangling%>%ggplot(., aes(x = factor(periods), fill = factor(FCR_24H_CLI))) +
  geom_bar(position="fill")+geom_text(aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
                                      stat="count",
                                      position=position_fill(vjust=0.5)) +
  labs(y="Proportion", x="Time Period", title ="24H Recall vs Time Period")+ scale_fill_discrete(name = "24H Recall")

final_data_wrangling%>%ggplot(., aes(x = factor(CALLREASON_update), fill = factor(FCR_24H_CLI))) +
  geom_bar(position="fill")+geom_text(aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
                                      stat="count",
                                      position=position_fill(vjust=0.5))+
  labs(y="Proportion", x="Call Reason", title ="24H Recall vs Call Reason")+coord_flip()+scale_fill_discrete(name = "24H Recall")

final_data_wrangling%>%ggplot(., aes(x = factor(IND_TRANSFER), fill = factor(FCR_24H_CLI))) +
  geom_bar(position="fill")+geom_text(aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
                                      stat="count",
                                      position=position_fill(vjust=0.5))+
  labs(y="Proportion", x="Transfer", title ="24H Recall vs Transfer")+scale_fill_discrete(name = "24H Recall")

final_data_wrangling%>%ggplot(., aes(x = factor(IND_BAD_PROBLEM_SOLVING), fill = factor(FCR_24H_CLI))) +
  geom_bar(position="fill")+geom_text(aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
                                      stat="count",
                                      position=position_fill(vjust=0.5))+
  labs(y="Proportion", x="Bad_Problem_Solving", title ="24H Recall vs Bad_Problem_Solving")+scale_fill_discrete(name = "24H Recall")



final_data_wrangling%>%group_by(FCR_24H_CLI)%>%summarise(median_time=median(NUM_SEC_CONTACT))%>%ggplot(.,aes(x=as.factor(FCR_24H_CLI),y=median_time,fill=as.factor(FCR_24H_CLI)))+geom_bar(stat="identity")+geom_label(aes(label=median_time))+  labs(y="Median of Call Duration in Sec.", x="24H Recall", title ="24H Recall vs Call Duration")+theme(legend.position="none")
final_data_wrangling%>%group_by(FCR_24H_CLI)%>%summarise(median_time=mean(NEGATIVITY))%>%ggplot(.,aes(x=as.factor(FCR_24H_CLI),y=round(median_time,2),fill=as.factor(FCR_24H_CLI)))+geom_bar(stat="identity")+geom_label(aes(label=round(median_time,2)))+  labs(y="Mean Count of Negative Words", x="24H Recall", title ="24H Recall vs Negative Words")+theme(legend.position="none")
##########################

str(final_data_wrangling)
summary(final_data_wrangling)

saveRDS(final_data_wrangling, "C:/Users/baumg/OneDrive/MsC DS&MA/KPN/outputs/final_datasets/final_data_wrangling_all_calls.RDS")
