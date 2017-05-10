library(doParallel); library(parallel)
cores=detectCores()
clust = makeCluster(cores)
registerDoParallel(clust)
library(caret); library(kernlab); library(ISLR); library(RANN);library("plyr");
library(DMwR);library(MASS); library(randomForest); library(rpart);
library(httr);library(iptools)

library(iptools)
library("CORElearn")

library(xml2)
library(scrapeR)
library(RCulr)
library(XML)


#API-KEY: ODT-API-5T7B96ZHNF
#API-SECRET: 2BM6ELS09WTAQICRHZ7U5JY1G3F4N8DOKXVP


#Constants
net_info = "https://stat.ripe.net/data/network-info/data.json?resource"
as_info = "https://stat.ripe.net/data/as-overview/data.json?resource"
geoloc_info = "https://stat.ripe.net/data/geoloc/data.json?resource"
ip_api_com_url = "http://ip-api.com/json"

#Functions



reverse_IP = function(IP)
{
    reversed_IP = system(paste("dig -x",IP,"| egrep '^;.*PTR$' | cut -c 2-", sep = ""),
                         intern = TRUE, ignore.stderr = TRUE)
    ifelse(ip_classify(as.character(IP)) == "IPv6",gsub("ip6\\.arpa\\.\\s+IN\\s+PTR.*$", "", reversed_IP, ignore.case = TRUE),
     gsub("in-addr\\.arpa\\.\\s+IN\\s+PTR.*$","", reversed_IP, ignore.case = TRUE))
}


RBL_AAA_resolver = function(df,source)
{
    command = "host "
    if(source == "rf.senderbase.org"){command = paste(command, "-t txt ")}
    dns_query = paste(command, reverse_IP(df$ipaddress), source, " ns1.proline.net.ua", sep="")
    dns_response = system(dns_query, intern = TRUE)
    print(paste("ID:",df$id,"IP:",df$ipaddress, 
                "city:",df$city, "target:",df$target,
                "response:", dns_response[6]))
    return(dns_response[6])
    
}

getAS = function(url,x){
    paste(url,x,sep="=")
}


mirror_IP_API_COM = function(url,x){
    paste(url,x,sep="/")
}

strip_rbl_response = function(x)
{
    grep(".+has address.+(127\\.0\\.0\\.[0-9]{1,3}).*", x, ignore.case = TRUE, value = FALSE)
   
}

setwd("Schreibtisch/Data_Science_ROOT/JOB-Interviews/CREAM_Finance/")

train = read.csv(file="dataset.csv", header = TRUE, stringsAsFactors = TRUE, fill=TRUE, sep = ';', na.strings = c(""))
train$target2 = NULL

summary(train)
which(duplicated(train))


#Gray IPs. https://tools.ietf.org/html/rfc1918
#10.0.0.0        -   10.255.255.255  (10/8 prefix)
#172.16.0.0      -   172.31.255.255  (172.16/12 prefix)
#192.168.0.0     -   192.168.255.255 (192.168/16 prefix)
train_v2 = train[!grepl("10(\\.[0-9]{1,3}){3}", train$ipaddress),]

#No presents of 127(localhost), 172, 192, and IPv6
train[grepl("^127\\.", train$ipaddress),]
train[grepl("^172\\.", train$ipaddress),]
train[grepl("^192\\.168\\.", train$ipaddress),]
train[grepl("fc00", train$ipaddress),]

#So, the BAD/GOOD ratio has changed. However, the goodies are still dominant
#It means that those IPs were just lost, not hidden on purpose
summary(train_v2[is.na(train_v2$ipaddress),])

#Splitting now in two datasets. Ignoring records with no IP available.
train_v2_noIP = dplyr::select(train_v2,-ipaddress)
train_v2_IP=train_v2[!is.na(train_v2$ipaddress),]

#JSON Playground

write.table(train_v2_IP, file="train_v2_IP.csv", quote = FALSE, sep=";")
train_v2_IP_backup = train_v2_IP
train_v2_IP = train_v2_IP_backup
train_v2_IP$AS = 0



#Suggested source of RIPE

for(i in 5596:nrow(train_v2_IP))
{
    net = GET(getAS(net_info,train_v2_IP$ipaddress[i]))
    print(paste("ID:",i,"STATUS:",status_code(net), "IP:",train_v2_IP$ipaddress[i], "AS:",content(net, "parsed")$data$asns))
    train_v2_IP$AS[i] = content(net, "parsed")$data$asns
}

train_v2_IP$AS = as.factor(train_v2_IP$AS)

#Some unknown AS
#ID:1768:95.178.102.3-PL. ip-api.com says it is AS39091
#ID:2153:85.31.248.112-PL. whois says it is AS43939
#ID:2180:194.246.98.224-PL. whois says it is AS20960
#ID:3861:95.178.96.13-PL. ip-api.com says it is AS39091
#ID:5737:185.28.122.35-PL. whois says it is AS60608

write.table(train_v2_IP, file="train_v2_IP_RIPE.csv", quote = FALSE, sep=";")

train_v2_IP[train_v2_IP$AS == 0,]
#Imputing manually
train_v2_IP[train_v2_IP$id == 1768,]$AS = "39091"
train_v2_IP[train_v2_IP$id == 2153,]$AS = "43939"
train_v2_IP[train_v2_IP$id == 2180,]$AS = "20960"
train_v2_IP[train_v2_IP$id == 3861,]$AS = "39091"
train_v2_IP[train_v2_IP$id == 5737,]$AS = "60608"

#Suggested source of RIPE. Step 2: AS => details

train_v2_RIPE_backup = train_v2_IP
train_v2_RIPE = train_v2_IP
#initialisation
train_v2_RIPE$resource= ""
train_v2_RIPE$holder = ""

for(i in 3994:nrow(train_v2_RIPE))
{
    answer = GET(getAS(as_info,train_v2_RIPE$AS[i]))
    print(paste("ID:",i,"STATUS:",status_code(answer), "IP:",train_v2_RIPE$ipaddress[i],
                "resource:",content(answer,"parsed")$data$block$resource,
                "holder:",content(answer,"parsed")$data$holder))
    train_v2_RIPE$resource[i] = content(answer,"parsed")$data$block$resource
    train_v2_RIPE$holder[i] = content(answer,"parsed")$data$holder
}

write.table(train_v2_RIPE, file="train_v2_IP_RIPE2.csv", quote = FALSE, sep=";")


#Suggested source of RIPE. Step 3: IP => Geoloc

train_v3_RIPE_backup = train_v2_RIPE
train_v3_RIPE = train_v2_RIPE

#initialisation
train_v3_RIPE$city = ""
train_v3_RIPE$prefixes = ""
train_v3_RIPE$country = ""
train_v3_RIPE$lon = ""
train_v3_RIPE$lat = ""
train_v3_RIPE$cover = ""


for(i in 1151:nrow(train_v3_RIPE))
{
    answer = GET(getAS(geoloc_info,train_v3_RIPE$ipaddress[i]))
    train_v3_RIPE$city[i] = content(answer,"parsed")$data$locations[[1]]$city
    train_v3_RIPE$prefixes[i] = content(answer,"parsed")$data$locations[[1]]$prefixes
    train_v3_RIPE$country[i] = content(answer,"parsed")$data$locations[[1]]$country
    train_v3_RIPE$lon[i] = content(answer,"parsed")$data$locations[[1]]$longitude
    train_v3_RIPE$lat[i] = content(answer,"parsed")$data$locations[[1]]$latitude
    train_v3_RIPE$cover[i] = content(answer,"parsed")$data$locations[[1]]$covered_percentage
    print(paste("ID:",i,"STATUS:",status_code(answer), "IP:",train_v3_RIPE$ipaddress[i],
                "country:",train_v3_RIPE$country[i], "city:",train_v3_RIPE$city[i], 
                "TARGET:",train_v3_RIPE$target[i], "prefixes:", train_v3_RIPE$prefixes[i],
                "covered:",train_v3_RIPE$cover[i]))
}

write.table(train_v3_RIPE, file="train_v3_IP_RIPE.csv", quote = FALSE, sep=";")


#Let's see whether ip-api.com gives any better


train_v2_IP_IP_API_COM = train_v2_IP_backup
#initialisation
train_v2_IP_IP_API_COM$AS = ""
train_v2_IP_IP_API_COM$city = ""
train_v2_IP_IP_API_COM$country = ""
train_v2_IP_IP_API_COM$isp = ""
train_v2_IP_IP_API_COM$lat = ""
train_v2_IP_IP_API_COM$lon = ""
train_v2_IP_IP_API_COM$zip = ""



for(i in 1:nrow(train_v2_IP_IP_API_COM))
{
    answer = GET(mirror_IP_API_COM(ip_api_com_url,train_v2_IP_IP_API_COM$ipaddress[i]))
    train_v2_IP_IP_API_COM$AS[i] = content(answer, "parsed")$as
    train_v2_IP_IP_API_COM$city[i] = content(answer, "parsed")$city
    train_v2_IP_IP_API_COM$country[i] = content(answer, "parsed")$countryCode
    train_v2_IP_IP_API_COM$isp[i] = content(answer, "parsed")$isp
    train_v2_IP_IP_API_COM$lat[i] = content(answer, "parsed")$lat
    train_v2_IP_IP_API_COM$lon[i] = content(answer, "parsed")$lon
    train_v2_IP_IP_API_COM$zip[i] = content(answer, "parsed")$zip
    print(paste("ID:",i,"STATUS:",status_code(answer), "IP:",train_v2_IP_IP_API_COM$ipaddress[i], 
                "country:",content(answer, "parsed")$countryCode,"city:",content(answer, "parsed")$city, "zip:",content(answer, "parsed")$zip))
    Sys.sleep(0.5)
}

#cosmetic cleanup
train_v2_IP_IP_API_COM$AS=gsub("^(AS[0-9]{1,10}).*","\\1",train_v2_IP_IP_API_COM$AS)
train_v2_IP_IP_API_COM$city = gsub("( \\(.+\\))","",train_v2_IP_IP_API_COM$city)

train_v3_IP_IP_API_COM = cbind(train_v2_IP_IP_API_COM, dplyr::select(train_v3_RIPE,holder,resource,prefixes))
train_v3_IP_IP_API_COM$IPv4 = as.numeric(ip_classify(as.vector(train_v3_IP_IP_API_COM$ipaddress)) == "IPv4")
train_v3_IP_IP_API_COM$prefixes = as.character(train_v3_IP_IP_API_COM$prefixes)

write.table(train_v3_IP_IP_API_COM, file="train_v3_IP_API_COM.csv", quote = FALSE, sep=";")

blended = read.csv(file="train_v3_IP_API_COM.csv", header = TRUE, fill=TRUE, sep = ';', na.strings = c(""))
blended$IPv4 = as.factor(blended$IPv4)


#B.Barracudacentral.org resolving
blended$barracuda_response = ""
blended$bit_nl_response = ""
blended$rbl_dns_ru_response = ""
blended$sorbs_response = ""
blended$spamhaus_response = ""
blended$spamrats_response = ""
blended$monkey_response = ""
blended$senderbase_response = ""
blended$tor_dan_me_uk_response = ""
blended$apews_org_response = ""
blended$hostname_response = ""

#Barracuda
blended$barracuda_response[i]  = RBL_AAA_resolver(blended$ipaddress[i],"b.barracudacentral.org")[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$barracuda_response[i]))
#rbl.rbldns.ru
blended$rbl_dns_ru_response[i]  = RBL_AAA_resolver(blended$ipaddress[i],"rbl.rbldns.ru")[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$rbl_dns_ru_response[i]))
#dnsbl.sorbs.net
blended$sorbs_response[i]  = RBL_AAA_resolver(blended$ipaddress[i],"dnsbl.sorbs.net")[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$sorbs_response[i]))
#pbl.spamhaus.org
blended$spamhaus_response[i]  = RBL_AAA_resolver(blended$ipaddress[i],"pbl.spamhaus.org")[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$spamhaus_response[i]))
#all.spamrats.com
blended$spamrats_response[i]  = RBL_AAA_resolver(blended$ipaddress[i],"all.spamrats.com")[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$spamrats_response[i]))
#BIT.nl
blended$bit_nl_response[i]  = RBL_AAA_resolver(blended$ipaddress[i],"all.ascc.dnsbl.bit.nl")[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$bit_nl_response[i]))
#rf.senderbase.org
blended$senderbase_response[i]  = RBL_AAA_resolver(blended$ipaddress[i],"rf.senderbase.org")[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$senderbase_response[i]))
#origin.asn.spameatingmonkey.net
blended$monkey_response[i]  = RBL_AAA_resolver(blended$ipaddress[i],"origin.asn.spameatingmonkey.net")[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$monkey_response[i]))
#tor.dan.me.uk
blended$tor_dan_me_uk_response[i]  = RBL_AAA_resolver(blended$ipaddress[i],"tor.dan.me.uk")[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$tor_dan_me_uk_response[i]))
#l2.apews.org
blended$apews_org_response[i]  = RBL_AAA_resolver(blended$ipaddress[i],"l2.apews.org")[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$apews_org_response[i]))
#hostname
blended$hostname_response[i]  = as.character(system(paste("host",blended$ipaddress[i], "ns1.proline.net.ua", sep=" "), intern=TRUE))[6]
print(paste("ID:",i,"IP:",blended$ipaddress[i], 
            "city:",blended$city[i], "target:",blended$target[i],
            "response:", blended$hostname_response[i]))

still_missing = which(!complete.cases(dplyr::select(blended,-zip, -isp)))
for(i in still_missing)
{
    #Barracuda
    blended$barracuda_response[i]  = RBL_AAA_resolver(blended[i,],"b.barracudacentral.org")
    #rbl.rbldns.ru
    blended$rbl_dns_ru_response[i]  = RBL_AAA_resolver(blended[i,],"rbl.rbldns.ru")
    #dnsbl.sorbs.net
    blended$sorbs_response[i]  = RBL_AAA_resolver(blended[i,],"dnsbl.sorbs.net")
    #pbl.spamhaus.org
    blended$spamhaus_response[i]  = RBL_AAA_resolver(blended[i,],"pbl.spamhaus.org")
    #all.spamrats.com
    blended$spamrats_response[i]  = RBL_AAA_resolver(blended[i,],"all.spamrats.com")
    #BIT.nl
    blended$bit_nl_response[i]  = RBL_AAA_resolver(blended[i,],"all.ascc.dnsbl.bit.nl")
    #rf.senderbase.org IN TXT
    blended$senderbase_response[i]  = RBL_AAA_resolver(blended[i,],"rf.senderbase.org")
    #origin.asn.spameatingmonkey.net
    blended$monkey_response[i]  = RBL_AAA_resolver(blended[i,],"origin.asn.spameatingmonkey.net")
    #tor.dan.me.uk Tor Net
    blended$tor_dan_me_uk_response[i]  = RBL_AAA_resolver(blended[i,],"tor.dan.me.uk")
    #l2.apews.org
    blended$apews_org_response[i]  = RBL_AAA_resolver(blended[i,],"l2.apews.org")
    
    #hostname
    blended$hostname_response[i]  = as.character(system(paste("host",blended$ipaddress[i], "ns1.proline.net.ua", sep=" "), intern=TRUE))[6]
    print(paste("ID:",i,"IP:",blended$ipaddress[i], 
                "city:",blended$city[i], "target:",blended$target[i],
                "response:", blended$hostname_response[i]))
    Sys.sleep(0.5)
}

write.table(blended, file="train_v3_blended.csv", quote = FALSE, sep=";")



blended_v2 = blended

for(n in names(blended_v2)[18:27])
{
    new_field = paste(n,"score", sep="_")
    matches = strip_rbl_response(blended_v2[,n])
    blended_v2[new_field] = "NOT listed"
    blended_v2[matches,new_field] = "listed"
    blended_v2[,new_field] = as.factor(blended_v2[,new_field])
}

blended_v2$senderbase_response_score = "NOT scored"

pattern = ".+(descriptive text).+\"(.*)\".*"
got_it = grep(pattern, blended_v2$senderbase_response, value = FALSE)
blended_v2[got_it,]$senderbase_response_score = gsub(pattern, "\\2", blended_v2[got_it,]$senderbase_response)
blended_v2$senderbase_response_score=as.factor(blended_v2$senderbase_response_score)



write.table(blended_v2, file="train_v3_blended_v2.csv", quote = FALSE, sep=";")

setwd("Schreibtisch/Data_Science_ROOT/JOB-Interviews/CREAM_Finance/")

qq = read.csv(file="train_v3_blended_v2.csv", header = TRUE, fill=TRUE, sep = ';', na.strings = c(""))

qq = qq[qq$Income < 4700.0 & qq$Income > 500.0 & qq$IPv4 == 1,]

qq$DA = qq$Database_negative*qq$Age
qq$DAA = qq$Database_negative*qq$Age*qq$Age
qq$DDA = qq$Database_negative*qq$Database_negative*qq$Age
qq$DDAA = qq$Database_negative*qq$Database_negative*qq$Age*qq$Age
qq$DI = qq$Database_negative*qq$Income
qq$AI = qq$Age*qq$Income
qq$DA_1 = qq$Age*qq$Database_negative
qq$DI_1 = qq$Income*qq$Database_negative
qq$AI_1 = qq$Income*qq$Age





#Downsampling
qq_down = downSample(qq[,!names(qq) %in% "target"], qq$target, yname = "target")
qq_down = dplyr::select(qq_down,Database_negative,Age,Income,DA,DAA,DDA,DDAA,DI,AI,DA_1,DI_1,AI_1,
                        barracuda_response_score,spamrats_response_score,sorbs_response_score,apews_org_response_score,target)

qq_down = dplyr::select(qq_down,Database_negative,Age,Income,DA,DAA,DDA,DDAA,DI,AI,DA_1,DI_1,AI_1,
                    target)


qq_down = dplyr::select(qq_down,Database_negative,Age,Income,
                        barracuda_response_score,spamrats_response_score,sorbs_response_score,apews_org_response_score,target)

qq_down = dplyr::select(qq_down,Database_negative,Age,Income,DA,DI,AI,
                        barracuda_response_score,spamrats_response_score,sorbs_response_score,apews_org_response_score,target)

#Outliers


md <- CoreModel(target ~ ., qq_down, model="rf", rfNoTrees=30, maxThreads=1)
outliers <- rfOutliers(md, qq_down)
plot(abs(outliers), col=qq_down$target)

borderline = abs(outliers) < 10

qq_down_v2 = qq_down[borderline,]

iTrain = createDataPartition(y=qq_down_v2$target, p=0.8, list = FALSE)

qq_train = qq_down_v2[iTrain,]
qq_test = qq_down_v2[-iTrain,]

control <- trainControl(method="cv", number=10, repeats=5, summaryFunction = twoClassSummary, classProbs = TRUE)

set.seed(666)
model_RF = train(target~., data=qq_train, method="rf", preProcess="range", trControl=control, tuneGrid = expand.grid(.mtry = 2:4),  n.tree=200, maxit=2000, importance=TRUE, proximity=TRUE)

PREDICTION_RF = predict(model_RF, newdata=qq_test, type="prob")

confusionMatrix(PREDICTION_RF,qq_test[,"target"])

set.seed(666)
model_NN = train(target~., data=qq_train, method="nnet", preProcess="range", tuneLength=3, maxit = 2000, trControl=control, importance=TRUE)

PREDICTION_NN = predict(model_NN, newdata=qq_test, type="prob")
PREDICTION_NN <- ifelse(PREDICTION_NN > 0.5,'BAD','GOOD')
confusionMatrix(PREDICTION_NN,qq_test[,"target"])


##GLM

set.seed(666)

model_glm <- glm(target ~.,family=binomial(link='logit'),data=qq_train)

model_GLM = train(target~., data=qq_train, method="glmnet", preProcess="range", trControl=control)
PREDICTION_glm = predict(model_GLM, newdata=qq_test)
PREDICTION_glm = predict(model_glm, newdata=qq_test)
PREDICTION_glm <- ifelse(PREDICTION_glm > 0.5,'BAD','GOOD')
confusionMatrix(PREDICTION_glm,qq_test[,"target"])



knnGrid <- expand.grid(.k=c(2:20))

model_knn <- train(target ~ ., data = qq_train,
                   method = "knn",                  preProcess = c("center", "scale"),
                   tuneLength = 20, tuneGrid =knnGrid,
                   trControl=control)

PREDICTION_knn = predict(model_knn, newdata=qq_test, type="prob")
confusionMatrix(PREDICTION_knn,qq_test[,"target"])

importance <- varImp(model_knn, scale=TRUE)
# summarize importance
print(importance)
# plot importance
plot(importance)



library(ggmap)

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)


#Some unknown AS
#ID:1768:95.178.102.3-PL. ip-api.com says it is AS39091
#ID:2153:85.31.248.112-PL. whois says it is AS43939
#ID:2180:194.246.98.224-PL. whois says it is AS20960
#ID:3861:95.178.96.13-PL. ip-api.com says it is AS39091
#ID:5737:185.28.122.35-PL. whois says it is AS60608





#Network request
net <- GET(getAS(net_info,"187.237.14.0"))
status_code(net)
headers(net)
str(content(net, "parsed"))
content(net, "parsed")$data$asns


#AS
AS <- GET("https://stat.ripe.net/data/as-overview/data.json?resource=6453")
status_code(AS)
headers(AS)
str(content(AS, "parsed"))
str(content(AS, "parsed")$data)
content(AS, "parsed")$data

#Geoloc
Geo <- GET("https://stat.ripe.net/data/geoloc/data.json?resource=187.237.14.0")
status_code(Geo)
headers(Geo)
str(content(Geo, "parsed"))
content(Geo, "parsed")$data

