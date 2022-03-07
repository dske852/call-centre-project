#selectin variables of the main focus for expralatory purposes
call_data_summary<-call_data_pseudonymized_1_%>%select("NUM_SEC_CONTACT", "IND_TRANSFER", "NUM_SEC_ON_HOLD", "timestamp", "CALLREASON", "SERVICE_DESK", 
                                                       "LAST_SKILL_NUM", "SELECTED_PRODUCT", "FCR_24H_CLI", "FCR_7D_CLI", "FCR_14D_CLI", "NEGATIVITY", "IND_BAD_PROBLEM_SOLVING")


call_data_summary<-call_data_correct%>%mutate_if(is.character, as.factor)

#dropping unknown recalls
call_data_summary<-filter(call_data_summary, FCR_24H_CLI != -2)

#charactericts
summary(call_data_summary)
sd(call_data_summary$NUM_SEC_CONTACT)
sd(call_data_summary$NUM_SEC_ON_HOLD)
sd(call_data_summary$NEGATIVITY)
#what is minus in num_sec_on_hold

#replacing weird on hold values
call_data_summary$NUM_SEC_ON_HOLD<-ifelse(call_data_summary$NUM_SEC_ON_HOLD<0,0,call_data_summary$NUM_SEC_ON_HOLD)
summary(call_data_summary)
#what is minus in num_sec_on_hold

#Creating CALL REASON levels
call_data_summary%>%group_by(CALLREASON)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))


call_data_summary$CALLREASON_update<-ifelse(call_data_summary$CALLREASON == "Overige" | call_data_summary$CALLREASON == "Betaling & factuur" |
                                              call_data_summary$CALLREASON == "Opzeggen"| call_data_summary$CALLREASON == "Verkoop" | 
                                              call_data_summary$CALLREASON == "Order capture & fulfillment: overige" |  call_data_summary$CALLREASON == "Storing: internet"|
                                              call_data_summary$CALLREASON == "Storing: telefonie",as.character(call_data_summary$CALLREASON), "Other" )

call_data_summary$CALLREASON_update<-as.factor(call_data_summary$CALLREASON_update)


#Preparing time/date data

library(stringr)
class(call_data_summary$timestamp)
class(as.character(call_data_summary$timestamp))
time<- str_split(as.character(call_data_summary$timestamp), " " , simplify = T)
call_data_summary$hour<-time[,2]
call_data_summary$day<-time[,1]

call_data_summary$day_name<-as.Date(call_data_summary$day)
call_data_summary$day_name<-weekdays(call_data_summary$day_name)

time2<- str_split(as.character(call_data_summary$day), "-" , simplify = T)
time2<-as.data.frame(time2)
unique(filter(time2, V2==12))

call_data_summary$month<-time2[,2] #December until 16th

call_data_summary$day_num<-time2[,3]

str(call_data_summary)

library(lubridate)
hours_lubri<-hms(call_data_summary$hour)
unique(call_data_summary$hour)
call_data_summary$periods<-ifelse(hours_lubri >= "8H 30M 0S" & hours_lubri <= "11H 0M 0S", "8.30AM-11AM",
                                  ifelse(hours_lubri > "11H 0M 0S" & hours_lubri <= "14H 00M 0S", "11AM-2PM",
                                         ifelse(hours_lubri > "14H 0M 0S" & hours_lubri <= "17H 0M 0S", "2PM-5PM",
                                                ifelse( hours_lubri > "17H 0M 0S" & hours_lubri <= "20H 0M 0S", "5PM-8PM",
                                                        ifelse(hours_lubri > "20H 0M 0S" & hours_lubri <= "23H 0M 0S", "late-evening-night",
                                                               ifelse(hours_lubri >= "6H 0M 0S" & hours_lubri < "8H 30M 0S", "6AM-8.30AM","late-evening-night"))))))


##calculating proportions
summary(as.factor(call_data_summary$IND_TRANSFER))
summary(as.factor(call_data_summary$IND_BAD_PROBLEM_SOLVING))
summary(as.factor(call_data_summary$FCR_24H_CLI))

call_data_summary%>%group_by(periods)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))
call_data_summary%>%group_by(day_name)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))
call_data_summary%>%group_by(CALLREASON_update)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))
turn_data_summary%>%group_by(Emotion)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))

negative_ratio

#####PLOTS#####

call_data_summary%>%ggplot(., aes(x = factor(periods), fill = factor(FCR_24H_CLI))) +
  geom_bar(position="fill")+geom_text(aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
                                      stat="count",
                                      position=position_fill(vjust=0.5)) +
  labs(y="Proportion", x="Time Period", title ="24H Recall vs Time Period")+ scale_fill_discrete(name = "24H Recall")

call_data_summary%>%ggplot(., aes(x = factor(CALLREASON_update), fill = factor(FCR_24H_CLI))) +
  geom_bar(position="fill")+geom_text(aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
                                      stat="count",
                                      position=position_fill(vjust=0.5))+
  labs(y="Proportion", x="Call Reason", title ="24H Recall vs Call Reason")+coord_flip()+scale_fill_discrete(name = "24H Recall")

call_data_summary%>%ggplot(., aes(x = factor(IND_TRANSFER), fill = factor(FCR_24H_CLI))) +
  geom_bar(position="fill")+geom_text(aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
                                      stat="count",
                                      position=position_fill(vjust=0.5))+
  labs(y="Proportion", x="Transfer", title ="24H Recall vs Transfer")+scale_fill_discrete(name = "24H Recall")

call_data_summary%>%ggplot(., aes(x = factor(IND_BAD_PROBLEM_SOLVING), fill = factor(FCR_24H_CLI))) +
  geom_bar(position="fill")+geom_text(aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
                                      stat="count",
                                      position=position_fill(vjust=0.5))+
  labs(y="Proportion", x="Bad_Problem_Solving", title ="24H Recall vs Bad_Problem_Solving")+scale_fill_discrete(name = "24H Recall")



call_data_summary%>%group_by(FCR_24H_CLI)%>%summarise(median_time=median(NUM_SEC_CONTACT))%>%ggplot(.,aes(x=as.factor(FCR_24H_CLI),y=median_time,fill=as.factor(FCR_24H_CLI)))+geom_bar(stat="identity")+geom_label(aes(label=median_time))+  labs(y="Median of Call Duration in Sec.", x="24H Recall", title ="24H Recall vs Call Duration")+theme(legend.position="none")
call_data_summary%>%group_by(FCR_24H_CLI)%>%summarise(median_time=mean(NEGATIVITY))%>%ggplot(.,aes(x=as.factor(FCR_24H_CLI),y=round(median_time,2),fill=as.factor(FCR_24H_CLI)))+geom_bar(stat="identity")+geom_label(aes(label=round(median_time,2)))+  labs(y="Mean Count of Negative Words", x="24H Recall", title ="24H Recall vs Negative Words")+theme(legend.position="none")

########
#####

turn_data_summary<-turn_data_correct%>%mutate_if(is.character, as.factor)%>%rename(Dial_Str=`Dialogue Structure`)
#filtering_data<-filter(turn_data_summary, Dial_Str != "nds" & Dial_Str != "missing" & Emotion != "nvt")
turn_data_summary<-filter(turn_data_summary, Dial_Str != "missing")

newdata <- mltools::one_hot(as.data.table(turn_data_summary), cols=c("Dial_Str", "Emotion"))

customer_index_data<-filter(newdata, Person == "customer")

customer_index_data$Dial_Str_nds<-ifelse(customer_index_data$Dial_Str_nds==1, 0, 0)
customer_index_data$Dial_Str_back_office<-ifelse(customer_index_data$Dial_Str_back_office==1, 2, 0)
customer_index_data$Dial_Str_consultation <-ifelse(customer_index_data$Dial_Str_consultation==1, 1, 0)
customer_index_data$Dial_Str_customer_recognition <-ifelse(customer_index_data$Dial_Str_customer_recognition==1, 0, 0)
customer_index_data$Dial_Str_farewell  <-ifelse(customer_index_data$Dial_Str_farewell ==1, 4, 0)
customer_index_data$Dial_Str_follow_procedure  <-ifelse(customer_index_data$Dial_Str_follow_procedure ==1, 0, 0)
customer_index_data$Dial_Str_greeting  <-ifelse(customer_index_data$Dial_Str_greeting ==1, 0, 0)
customer_index_data$Dial_Str_mechanic_planned  <-ifelse(customer_index_data$Dial_Str_mechanic_planned ==1, 2, 0)
customer_index_data$Dial_Str_my_problem_is   <-ifelse(customer_index_data$Dial_Str_my_problem_is  ==1, 1, 0)
customer_index_data$Dial_Str_solved   <-ifelse(customer_index_data$Dial_Str_solved  ==1, 4, 0)
customer_index_data$Dial_Str_suggest_solution    <-ifelse(customer_index_data$Dial_Str_suggest_solution   ==1, 3, 0)
customer_index_data$Dial_Str_transfer    <-ifelse(customer_index_data$Dial_Str_transfer   ==1, 2, 0)
customer_index_data$Emotion_angry    <-ifelse(customer_index_data$Emotion_angry   ==1, 2, 0)
customer_index_data$Emotion_fear    <-ifelse(customer_index_data$Emotion_fear   ==1, 1, 0)
customer_index_data$Emotion_happy    <-ifelse(customer_index_data$Emotion_happy   ==1, -1, 0)
customer_index_data$Emotion_sad    <-ifelse(customer_index_data$Emotion_sad   ==1, 1, 0)
customer_index_data$Emotion_nvt   <-ifelse(customer_index_data$Emotion_nvt   ==1, 0, 0)

customer_index_data$w.Dial_Str_nds<-customer_index_data$Dial_Str_nds*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_nds*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_nds*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_nds*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_back_office<-customer_index_data$Dial_Str_back_office*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_back_office*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_back_office*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_back_office*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_consultation<-customer_index_data$Dial_Str_consultation*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_consultation*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_consultation*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_consultation*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_customer_recognition<-customer_index_data$Dial_Str_customer_recognition*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_customer_recognition*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_customer_recognition*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_customer_recognition*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_farewell<-customer_index_data$Dial_Str_farewell*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_farewell*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_farewell*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_farewell*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_follow_procedure<-customer_index_data$Dial_Str_follow_procedure*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_follow_procedure*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_follow_procedure*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_follow_procedure*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_greeting<-customer_index_data$Dial_Str_greeting*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_greeting*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_greeting*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_greeting*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_mechanic_planned<-customer_index_data$Dial_Str_mechanic_planned*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_mechanic_planned*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_mechanic_planned*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_mechanic_planned*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_my_problem_is<-customer_index_data$Dial_Str_my_problem_is*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_my_problem_is*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_my_problem_is*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_my_problem_is*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_solved<-customer_index_data$Dial_Str_solved*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_solved*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_solved*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_solved*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_suggest_solution<-customer_index_data$Dial_Str_suggest_solution*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_suggest_solution*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_suggest_solution*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_suggest_solution*customer_index_data$Emotion_sad
customer_index_data$w.Dial_Str_transfer<-customer_index_data$Dial_Str_transfer*customer_index_data$Emotion_angry +customer_index_data$Dial_Str_transfer*customer_index_data$Emotion_fear +customer_index_data$Dial_Str_transfer*customer_index_data$Emotion_happy+customer_index_data$Dial_Str_transfer*customer_index_data$Emotion_sad

customer_index_data$index<- rowSums(customer_index_data[,c(24:35)])
groups<-customer_index_data%>%group_by(AO_transcription_id)%>%summarise(BadEmotionsScore_Cust=sum(index))
unique(groups$suming)

###agent

agent_index_data<-filter(newdata, Person == "agent")

agent_index_data$Dial_Str_nds<-ifelse(agent_index_data$Dial_Str_nds==1, 0, 0)
agent_index_data$Dial_Str_back_office<-ifelse(agent_index_data$Dial_Str_back_office==1, 2, 0)
agent_index_data$Dial_Str_consultation <-ifelse(agent_index_data$Dial_Str_consultation==1, 3, 0)
agent_index_data$Dial_Str_customer_recognition <-ifelse(agent_index_data$Dial_Str_customer_recognition==1, 0, 0)
agent_index_data$Dial_Str_farewell  <-ifelse(agent_index_data$Dial_Str_farewell ==1, 4, 0)
agent_index_data$Dial_Str_follow_procedure  <-ifelse(agent_index_data$Dial_Str_follow_procedure ==1, 0, 0)
agent_index_data$Dial_Str_greeting  <-ifelse(agent_index_data$Dial_Str_greeting ==1, 1, 0)
agent_index_data$Dial_Str_mechanic_planned  <-ifelse(agent_index_data$Dial_Str_mechanic_planned ==1, 2, 0)
agent_index_data$Dial_Str_my_problem_is   <-ifelse(agent_index_data$Dial_Str_my_problem_is  ==1, 1, 0)
agent_index_data$Dial_Str_solved   <-ifelse(agent_index_data$Dial_Str_solved  ==1, 4, 0)
agent_index_data$Dial_Str_suggest_solution    <-ifelse(agent_index_data$Dial_Str_suggest_solution   ==1, 3, 0)
agent_index_data$Dial_Str_transfer    <-ifelse(agent_index_data$Dial_Str_transfer   ==1, 2, 0)
agent_index_data$Emotion_angry    <-ifelse(agent_index_data$Emotion_angry   ==1, 2, 0)
agent_index_data$Emotion_fear    <-ifelse(agent_index_data$Emotion_fear   ==1, 2, 0)
agent_index_data$Emotion_happy    <-ifelse(agent_index_data$Emotion_happy   ==1, -1, 0)
agent_index_data$Emotion_sad    <-ifelse(agent_index_data$Emotion_sad   ==1, 1, 0)
agent_index_data$Emotion_nvt   <-ifelse(agent_index_data$Emotion_nvt   ==1, 0, 0)

agent_index_data$w.Dial_Str_nds<-agent_index_data$Dial_Str_nds*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_nds*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_nds*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_nds*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_back_office<-agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_back_office*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_consultation<-agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_consultation*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_customer_recognition<-agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_customer_recognition*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_farewell<-agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_farewell*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_follow_procedure<-agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_follow_procedure*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_greeting<-agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_greeting*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_mechanic_planned<-agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_mechanic_planned*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_my_problem_is<-agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_my_problem_is*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_solved<-agent_index_data$Dial_Str_solved*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_solved*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_solved*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_solved*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_suggest_solution<-agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_suggest_solution*agent_index_data$Emotion_sad
agent_index_data$w.Dial_Str_transfer<-agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_angry +agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_fear +agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_happy+agent_index_data$Dial_Str_transfer*agent_index_data$Emotion_sad

agent_index_data$index<- rowSums(agent_index_data[,c(24:35)])
groups2<-agent_index_data%>%group_by(AO_transcription_id)%>%summarise(BadEmotionsScore_Empl=sum(index))
unique(groups2$suming)


#Logistic Regression
levels(call_data_summary$FCR_24H_CLI)<-c(0,1)
class(call_data_summary$FCR_24H_CLI)
set.seed(25892)
trainIndex <- createDataPartition(call_data_summary$FCR_24H_CLI, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train <- call_data_summary[ trainIndex,]
test  <- call_data_summary[-trainIndex,]

basic_logistic<-glm(as.factor(FCR_24H_CLI) ~ NUM_SEC_CONTACT+ as.factor(IND_BAD_PROBLEM_SOLVING)+as.factor(CALLREASON_update)+as.factor(IND_TRANSFER)+ NUM_SEC_ON_HOLD+ as.factor(day_name)+ as.factor(periods)+ NEGATIVITY, family="binomial", data=train)
summary(basic_logistic)
exp(basic_logistic$coefficients)-1

p<-predict(basic_logistic2, test, type="response")
confusionMatrix(data=as.factor(ifelse(p>0.5,1,0)),as.factor(test$FCR_24H_CLI))

down<-caret::downSample(train[,c(1:8,10:19)],as.factor(train$FCR_24H_CLI))
basic_logistic2<-glm(as.factor(Class) ~ NUM_SEC_CONTACT+ as.factor(IND_BAD_PROBLEM_SOLVING)+as.factor(CALLREASON_update)+as.factor(IND_TRANSFER)+ NUM_SEC_ON_HOLD+ as.factor(day_name)+ as.factor(periods)+ NEGATIVITY, family="binomial", data=down)
summary(basic_logistic2)
exp(basic_logistic2$coefficients)-1



########

joinik<-right_join(groups2, call_data_correct, "AO_transcription_id")
final_data_filip_iris2<-right_join(groups, final_data_filip_iris , by=c("AO_transcription_id"="ID"))


#########


call_data_summary%>%filter(FCR_24H_CLI != -2)%>%ggplot(., aes(x = factor(day_num), fill = factor(FCR_24H_CLI))) +
  geom_bar(position="fill")+geom_text(aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
                                      stat="count",
                                      position=position_fill(vjust=0.5))+
  labs(y="Proportion", x="Transfer", title ="24H Recall vs Transfer")+scale_fill_discrete(name = "24H Recall")+coord_flip()

call_data_summary%>%filter(FCR_24H_CLI != -2)%>%ggplot(., aes(x = factor(day_num), fill = factor(FCR_24H_CLI))) +
  geom_bar(position="fill")+geom_text(aes(label=signif(..count.. / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)),
                                      stat="count",
                                      position=position_fill(vjust=0.5))+
  labs(y="Proportion", x="Transfer", title ="24H Recall vs Transfer")+scale_fill_discrete(name = "24H Recall")+coord_flip()

call_data_summary%>%filter(month == 10 | month ==11)%>%group_by(day_num)%>%summarise(c=n())%>%mutate(perc=c/sum(c))%>%arrange(desc(perc))


view(call_data_pseudonymized_1_%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(n=n())%>%arrange())

###############################emotion data
str(turn_data_correct)
agent <- filter(turn_data_correct, Person == "agent")
customer <- filter(turn_data_correct, Person == "customer")



time_emotions<- str_split(as.character(agent$Timestamp), " " , simplify = T)
agent$hour<-time_emotions[,2]

a1<-filter(agent, AO_transcription_id == "20211004_084144_BFSU_t3")
class(a1$hour)
class(agent$Timestamp)
library(hms)
agent$hour<-as_hms(agent$Timestamp)
class(agent$hour)

sort(a1$hour)
a1$Emotion<-as.factor(a1$Emotion)
a11 <- mltools::one_hot(as.data.table(a1), cols=c("Emotion"))
a11<-cbind(a11,Emotion=a1$Emotion)
a11$sum<- rowSums(a11[,c(3:6)])
a11%>%ggplot(.,aes(x=hour, y=sum, colour=as.factor(Emotion)))+geom_point(size=5, alpha=0.5)

turn_data_correct$hour<-as_hms(turn_data_correct$Timestamp)
turn_data_correct%>%filter(AO_transcription_id == "20211004_084144_BFSU_t3")%>%ggplot(.,aes(x=`Dialogue Structure`,y=as.factor(Person), colour=as.factor(Emotion)))+geom_point(size=5, alpha=0.5)
turn_data_correct%>%filter(AO_transcription_id == "20211004_084144_BFSU_t3" & Emotion != "nvt")%>%ggplot(.,aes(x=hour,y=as.factor(Person), colour=as.factor(Emotion)))+geom_point(size=5, alpha=0.5)

view(turn_data_correct%>%filter(AO_transcription_id == "20211004_084144_BFSU_t3")%>%arrange(hour))


joinicek<-full_join(call_data_correct, agent, "AO_transcription_id")
frequent_employees<-call_data_correct%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(n=n())%>%arrange(desc(n))
only_frequent<-joinicek%>%filter(AO_GENESYS_EMPLOYEE_LOGIN %in% c(frequent_employees$AO_GENESYS_EMPLOYEE_LOGIN))

only_frequent_agent<-joinicek
only_frequent_agent$angry<-ifelse(only_frequent_agent$Emotion == "angry", 1, 0)
only_frequent_agent$happy<-ifelse(only_frequent_agent$Emotion == "happy", 1, 0)
only_frequent_agent$sad<-ifelse(only_frequent_agent$Emotion == "sad", 1, 0)
only_frequent_agent$fear<-ifelse(only_frequent_agent$Emotion == "fear", 1, 0)
only_frequent_agent$nvt<-ifelse(only_frequent_agent$Emotion == "nvt", 1, 0)

agent_angry<-only_frequent_agent%>%group_by( `Dialogue Structure` ,AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(s=sum(angry))

#weights to update
agent_angry$w<-ifelse(agent_angry$`Dialogue Structure`=="greeting",0.4,ifelse(agent_angry$`Dialogue Structure`=="follow_procedure",0.1,ifelse(agent_angry$`Dialogue Structure`=="customer_recognition", 0.1,ifelse(agent_angry$`Dialogue Structure`=="my_problem_is",0.3,ifelse(agent_angry$`Dialogue Structure`=="consultation",0.8,ifelse(agent_angry$`Dialogue Structure`=="transfer",0.4, ifelse(agent_angry$`Dialogue Structure`=="suggest_solution",0.8,ifelse(agent_angry$`Dialogue Structure`=="back_office",0.2, ifelse(agent_angry$`Dialogue Structure`=="mechanic_planed",0.3, ifelse(agent_angry$`Dialogue Structure`=="solved",1,ifelse(agent_angry$`Dialogue Structure`=="farewell",1,0.1)))))))))))
agent_angry$frequency<-right_join(agent_angry, frequent_employees, "AO_GENESYS_EMPLOYEE_LOGIN")$n
agent_angry$agent_r<-(agent_angry$w*agent_angry$s)
agent_angry$agent_rk<-((agent_angry$w*agent_angry$s)/agent_angry$frequency)*100
agent_score_anger<-agent_angry%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(m=mean(agent_rk))
agent_angry%>%group_by(AO_GENESYS_EMPLOYEE_LOGIN)%>%summarise(m=mean(agent_rk))%>%ggplot(.,aes(x=AO_GENESYS_EMPLOYEE_LOGIN,y=m))+geom_bar(stat="identity")+coord_flip()

all_data_emotions<-right_join(agent_score_anger,test_join, "AO_GENESYS_EMPLOYEE_LOGIN")
