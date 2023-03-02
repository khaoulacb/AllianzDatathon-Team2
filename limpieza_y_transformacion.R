source("config.R")

# cargamos los datos
address<-data.table(read.csv("address.csv"))
main<-read.csv("main.csv")
paid_record<-read.csv("paid_record.csv")

main.o<-main
paid_record.o<-paid_record

# primera limpieza del historico, quitamos la X al no significar "nada
main<-data.table(main)
paid_record<-data.table(paid_record%>%filter(STATUS!="X"))

# limpiamos NAs a grosso modo y forzamos que el valor de FLAG OWN CAR tenga valor SI o NO
# quitamos duplicados
main<-na.omit(main)
main<-main[ FLAG_OWN_CAR %in% c("Y","N"),]
main.test<-main%>%group_by(Main_ID)%>%summarise(count=n())%>%filter(count>1)
main<-main%>%filter(!(Main_ID %in% main.test$Main_ID))

omited.main<-na.omit(main)
length(omited.main$CODE_GENDER)/length(main$CODE_GENDER)

status.paid<-paid_record%>%group_by(ID)%>%
  summarise(count=n())
ID.paid<-paid_record%>%group_by(ID)%>%
  summarise(count=n())

# decididimos si son morosos o no, segun el tiempo de impagos y si al final
# de su tiempo no restan deudas
test.paid<-paid_record[,last_month:=min(MONTHS_BALANCE)]
test.paid<-test.paid[,months_non_paid:=fifelse(
  STATUS %in% c("0","1","2","3","4","5"),1,0
)]
id_non_paid<-test.paid[,.(months_non_paid=sum(months_non_paid)),.(ID)]
range(id_non_paid$months_non_paid)
median(id_non_paid$months_non_paid)
id_non_paid%>%group_by(months_non_paid)%>%summarise(count=n())%>%
  ungroup()%>%filter(count==max(count))
mean(id_non_paid$months_non_paid)
hist(id_non_paid$months_non_paid)
test.paid<-test.paid%>%left_join(id_non_paid,by=c("ID"))
test.paid<-test.paid[,.(estado_moroso=tidyfast::dt_case_when(
    STATUS %in% c("0","1","2","3","4","5") & MONTHS_BALANCE==last_month~1,
    months_non_paid.y>11~1,
    TRUE~0
  )), .(ID)]
test.paid<-test.paid[,.(estado_moroso=max(estado_moroso)), .(ID)]

# cruzamos con una left_join el main con los datos del historico i el address
main<-main%>%mutate(New_ID:=paste0(Main_ID,Letter))
export1<-main%>%left_join(test.paid, by=c("Main_ID"="ID"))%>%
  left_join(address,  by=c("New_ID"="New_ID"))%>%
  filter(!is.na(estado_moroso))

# transformacion de datos
export1<-export1%>%rowwise()%>%
  mutate(zip_code:=str_sub(ADDRESS,
                           as.numeric(unlist(gregexpr("\\d{5}",ADDRESS))[1]),
                           as.numeric(unlist(gregexpr("\\d{5}",ADDRESS))[1])+4))
export1<-export1%>%ungroup()%>%
  mutate(DAYS_BIRTH_CLEAN=as.Date(DAYS_BIRTH_CLEAN,"%d/%m/%Y"))%>%
  mutate(age=(today()-DAYS_BIRTH_CLEAN))%>%
  mutate(age=as.numeric(age/365))
  
# exportacion de datos
write.csv2(export1, "datos_merged1.csv",row.names = FALSE)
write.xlsx(export1, "datos_merged1.xlsx",rowNames = FALSE)

# post evaluation
# verificamos se mantiene la dispersion de los datos
test.paid%>%group_by(estado_moroso)%>%
  summarise(count=n())
29510/(29510+11939)*100
11939/(29510+11939)*100

export1%>%group_by(estado_moroso)%>%
  summarise(count=n())
19418/(19418+7007)*100
7007/(19418+7007)*100

length(main$CODE_GENDER)/length(main.o$CODE_GENDER)*100
length(paid_record$ID)/length(paid_record.o$ID)*100

#################################################################

