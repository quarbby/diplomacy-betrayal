library(readr)

library(plyr)
names(alldata)
conf<-subset(alldata,select=names(alldata)[c(1:50,52,54,56,58,60,62,64,66)])


conf$Answer.1gamemove.yes[which(conf$Answer.1gamemove.yes==FALSE)]<-0
conf$Answer.2reasoning.yes[which(conf$Answer.2reasoning.yes==FALSE)]<-0
conf$Answer.3rapport.yes[which(conf$Answer.3rapport.yes==FALSE)]<-0
conf$Answer.3a_apologies.yes[which(conf$Answer.3a_apologies.yes==FALSE)]<-0
conf$Answer.3a_compliment.yes[which(conf$Answer.3a_compliment.yes==FALSE)]<-0
conf$Answer.3a_personalthoughts.yes[which(conf$Answer.3a_personalthoughts.yes==FALSE)]<-0
conf$Answer.3a_reassurance.yes[which(conf$Answer.3a_reassurance.yes==FALSE)]<-0
conf$Answer.4shareinformation.yes[which(conf$Answer.4shareinformation.yes==FALSE)]<-0

conf$Answer.1gamemove.yes[which(conf$Answer.1gamemove.yes==TRUE)]<-1
conf$Answer.2reasoning.yes[which(conf$Answer.2reasoning.yes==TRUE)]<-1
conf$Answer.3rapport.yes[which(conf$Answer.3rapport.yes==TRUE)]<-1
conf$Answer.3a_apologies.yes[which(conf$Answer.3a_apologies.yes==TRUE)]<-1
conf$Answer.3a_compliment.yes[which(conf$Answer.3a_compliment.yes==TRUE)]<-1
conf$Answer.3a_personalthoughts.yes[which(conf$Answer.3a_personalthoughts.yes==TRUE)]<-1
conf$Answer.3a_reassurance.yes[which(conf$Answer.3a_reassurance.yes==TRUE)]<-1
conf$Answer.4shareinformation.yes[which(conf$Answer.4shareinformation.yes==TRUE)]<-1


names(conf)
conf$count<-1
conf_good<-conf[conf$WorkTimeInSeconds>=80,]
conf_agg<-aggregate(conf[c(51:58,59)],by=list(conf$HITId),FUN=sum)
conf_good_agg<-aggregate(conf_good[c(51:58,59)],by=list(conf_good$HITId),FUN=sum)
conf_good_agg<-conf_good_agg[conf_good_agg$count>=3,]

conf_data<-conf[c(1,28:50)]
conf_data<-unique(conf_data)
names(conf_agg)
names(conf_agg)[1]<-c("HITId")
names(conf_good_agg)[1]<-c("HITId")
conf_merge<-merge(conf_data,conf_agg,by="HITId")
conf_good_merge<-merge(conf_data,conf_good_agg,by="HITId")

write.csv(conf_merge,"E:/Dropbox/Datasets/CLAFF2021/CLAFF 2021/Diplomacy codebook oct 3/AMT Oct 14 Conf/kokil dec 6 reprepare/conf_agg.csv",row.names = FALSE)
write.csv(conf_good_merge,"E:/Dropbox/Datasets/CLAFF2021/CLAFF 2021/Diplomacy codebook oct 3/AMT Oct 14 Conf/kokil dec 6 reprepare/conf_good_agg.csv",row.names = FALSE)

library(gdata)
keep(conf_good_merge,conf_merge,sure=TRUE)


###################### calc pc agreements
conf_agg2<-conf_good_agg
#conf_agg2<-conf_agg
names(conf_agg2)
conf_agg2[c(25:32)]<-conf_agg2[c(25:32)]/conf_agg2$count
names(conf_agg2)
conf_agg2<-conf_agg2[c(1,25:32)]
conf_agg2<-cbind(conf_agg2,conf_agg2[c(2:9)])
names(conf_agg2)[10:17]<-paste(names(conf_agg2)[2:9],"label",sep="_")
names(conf_agg2)[2:9]<-paste(names(conf_agg2)[2:9],"pc_agree",sep="_")


conf_agg2[c(10:17)]<-NA
conf_agg2$Answer.1gamemove.yes_label[which(conf_agg2$Answer.1gamemove.yes_pc_agree>=.6)]<-1
conf_agg2$Answer.1gamemove.yes_label[which(conf_agg2$Answer.1gamemove.yes_pc_agree>=.6)]<-1
conf_agg2$Answer.2reasoning.yes_label[which(conf_agg2$Answer.2reasoning.yes_pc_agree>=.6)]<-1
conf_agg2$Answer.3rapport.yes_label[which(conf_agg2$Answer.3rapport.yes_pc_agree>=.6)]<-1
conf_agg2$Answer.3a_apologies.yes_label[which(conf_agg2$Answer.3a_apologies.yes_pc_agree>=.6)]<-1
conf_agg2$Answer.3a_compliment.yes_label[which(conf_agg2$Answer.3a_compliment.yes_pc_agree>=.6)]<-1
conf_agg2$Answer.3a_personalthoughts.yes_label[which(conf_agg2$Answer.3a_personalthoughts.yes_pc_agree>=.6)]<-1
conf_agg2$Answer.3a_reassurance.yes_label[which(conf_agg2$Answer.3a_reassurance.yes_pc_agree>=.6)]<-1
conf_agg2$Answer.4shareinformation.yes_label[which(conf_agg2$Answer.4shareinformation.yes_pc_agree>=.6)]<-1

conf_agg2$Answer.1gamemove.yes_label[which(conf_agg2$Answer.1gamemove.yes_pc_agree<=.4)]<-0
conf_agg2$Answer.2reasoning.yes_label[which(conf_agg2$Answer.2reasoning.yes_pc_agree<=.4)]<-0
conf_agg2$Answer.3rapport.yes_label[which(conf_agg2$Answer.3rapport.yes_pc_agree<=.4)]<-0
conf_agg2$Answer.3a_apologies.yes_label[which(conf_agg2$Answer.3a_apologies.yes_pc_agree<=.4)]<-0
conf_agg2$Answer.3a_compliment.yes_label[which(conf_agg2$Answer.3a_compliment.yes_pc_agree<=.4)]<-0
conf_agg2$Answer.3a_personalthoughts.yes_label[which(conf_agg2$Answer.3a_personalthoughts.yes_pc_agree<=.4)]<-0
conf_agg2$Answer.3a_reassurance.yes_label[which(conf_agg2$Answer.3a_reassurance.yes_pc_agree<=.4)]<-0
conf_agg2$Answer.4shareinformation.yes_label[which(conf_agg2$Answer.4shareinformation.yes_pc_agree<=.4)]<-0
#


################# merge
#conf_agg_m<-merge(conf_agg,conf_agg2,by="HITId")
conf_good_agg_m<-merge(conf_good_agg,conf_agg2,by="HITId")
write.csv(conf_agg_m,"E:/Dropbox/Datasets/CLAFF2021/CLAFF 2021/Diplomacy codebook oct 3/AMT Oct 14 Conf/kokil dec 6 reprepare/conf_agg_withpc.csv",row.names = FALSE)

write.csv(conf_good_agg_m,"E:/Dropbox/Datasets/CLAFF2021/CLAFF 2021/Diplomacy codebook oct 3/AMT Oct 14 Conf/kokil dec 6 reprepare/conf_good_agg_withpc.csv",row.names = FALSE)


