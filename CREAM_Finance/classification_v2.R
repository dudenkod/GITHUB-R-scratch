library(doParallel); library(parallel)
cores=detectCores()-1
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

library(cluster)
library(graphics)
library(ggplot2)



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

#No presents of 127(localhost), 172, 192, and locahost-IPv6
train[grepl("^127\\.", train$ipaddress),]
train[grepl("^172\\.", train$ipaddress),]
train[grepl("^192\\.168\\.", train$ipaddress),]
train[grepl("fc00", train$ipaddress),]

#So, the BAD/GOOD ratio has changed. However, the goodies are still dominant
summary(train_v2[is.na(train_v2$ipaddress),])
#It means that those absent IPs were just lost, not on purpose


#Splitting now in two datasets. Ignoring records with no IP available.
#As it turns out, Database_negative, Income, and Age do not help much
#to distinguish BADs from GOODs
#Therefore, the rest of task is devoted to IPs scoring
train_v2_noIP = dplyr::select(train_v2,-ipaddress)
train_v2_IP=train_v2[!is.na(train_v2$ipaddress),]

#snapshot dump
write.table(train_v2_IP, file="train_v2_IP.csv", quote = FALSE, sep=";")
train_v2_IP_backup = train_v2_IP
train_v2_IP = train_v2_IP_backup


#Collecting ASs
train_v2_IP$AS = 0
#Suggested official source of RIPE

for(i in 1:nrow(train_v2_IP))
{
    net = GET(getAS(net_info,train_v2_IP$ipaddress[i]))
    print(paste("ID:",i,"STATUS:",status_code(net), "IP:",train_v2_IP$ipaddress[i], "AS:",content(net, "parsed")$data$asns))
    train_v2_IP$AS[i] = content(net, "parsed")$data$asns
}

#snapshot dump
write.table(train_v2_IP, file="train_v2_IP_RIPE.csv", quote = FALSE, sep=";")

train_v2_IP$AS = as.factor(train_v2_IP$AS)

#Some unknown AS. Probably, those providers forgot to insert it into the DB.
#Manually fixing...
#ID:1768:95.178.102.3-PL. ip-api.com says it is AS39091
#ID:2153:85.31.248.112-PL. whois says it is AS43939
#ID:2180:194.246.98.224-PL. whois says it is AS20960
#ID:3861:95.178.96.13-PL. ip-api.com says it is AS39091
#ID:5737:185.28.122.35-PL. whois says it is AS60608

train_v2_IP[train_v2_IP$AS == 0,]
#Imputing manually
train_v2_IP[train_v2_IP$id == 1768,]$AS = "39091"
train_v2_IP[train_v2_IP$id == 2153,]$AS = "43939"
train_v2_IP[train_v2_IP$id == 2180,]$AS = "20960"
train_v2_IP[train_v2_IP$id == 3861,]$AS = "39091"
train_v2_IP[train_v2_IP$id == 5737,]$AS = "60608"

#Suggested source of RIPE. Step 2: AS => profiling

#safety copies
train_v2_RIPE_backup = train_v2_IP
train_v2_RIPE = train_v2_IP

#initialisation
train_v2_RIPE$resource= ""
train_v2_RIPE$holder = ""

for(i in 1:nrow(train_v2_RIPE))
{
    answer = GET(getAS(as_info,train_v2_RIPE$AS[i]))
    print(paste("ID:",i,"STATUS:",status_code(answer), "IP:",train_v2_RIPE$ipaddress[i],
                "resource:",content(answer,"parsed")$data$block$resource,
                "holder:",content(answer,"parsed")$data$holder))
    train_v2_RIPE$resource[i] = content(answer,"parsed")$data$block$resource
    train_v2_RIPE$holder[i] = content(answer,"parsed")$data$holder
}

#snapshot dump
write.table(train_v2_RIPE, file="train_v2_IP_RIPE2.csv", quote = FALSE, sep=";")

#Suggested source of RIPE. Step 3: IP => Geoloc

train_v2_RIPE = read.csv(file="train_v2_IP_RIPE2.csv", header = TRUE, stringsAsFactors = TRUE, fill=TRUE, sep = ';', na.strings = c(""))

#safety copies
train_v3_RIPE_backup = train_v2_RIPE
train_v3_RIPE = train_v2_RIPE

#initialisation
train_v3_RIPE$city = ""
train_v3_RIPE$prefixes = ""
train_v3_RIPE$country = ""
train_v3_RIPE$lon = ""
train_v3_RIPE$lat = ""
train_v3_RIPE$cover = ""

for(i in 1:nrow(train_v3_RIPE))
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


train_v3_RIPE$prefixes = as.character(train_v3_RIPE$prefixes)

#snapshot dump
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
    #It is important to delicately resolve our IPs in those RBL
    #in order to avoid spontaneous bans
    Sys.sleep(0.5)
}

#cosmetic cleanup
train_v2_IP_IP_API_COM$AS=gsub("^(AS[0-9]{1,10}).*","\\1",train_v2_IP_IP_API_COM$AS)
train_v2_IP_IP_API_COM$city = gsub("( \\(.+\\))","",train_v2_IP_IP_API_COM$city)

#I find useful to collect holders, resource, and prefixes.
#The rest of info? Not sure whether it can be helpful for classification
train_v3_IP_IP_API_COM = cbind(train_v2_IP_IP_API_COM, dplyr::select(train_v3_RIPE,holder,resource,prefixes))
#It is also useful to distinguish IPv4 and IPv6 as there are still 
#problems to treat IPv6 correctly and also not many DBs have IPv6 support
train_v3_IP_IP_API_COM$IPv4 = as.numeric(ip_classify(as.vector(train_v3_IP_IP_API_COM$ipaddress)) == "IPv4")
train_v3_IP_IP_API_COM$prefixes = as.character(train_v3_IP_IP_API_COM$prefixes)

#snapshot dump
write.table(train_v3_IP_IP_API_COM, file="train_v3_IP_API_COM.csv", quote = FALSE, sep=";")

#Now, safety copy of that dataframe blended from ip-api.com and official RIPE
blended = read.csv(file="train_v3_IP_API_COM.csv", header = TRUE, fill=TRUE, sep = ';', na.strings = c(""))
blended$IPv4 = as.factor(blended$IPv4)

#Now we shall see what most used blacklists have on our IPs

#initialisation. Constantly adding some others, which come to my mind
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


#we need it as not all IPs got properly resolved after first iteration
#DNS resolving of large IP sets should be done with gentle care.
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

#snapshot dump
write.table(blended, file="train_v3_blended.csv", quote = FALSE, sep=";")


#safety copying
blended_v2 = blended

#Creating score columns for every blacklist
for(n in names(blended_v2)[18:27])
{
    new_field = paste(n,"score", sep="_")
    matches = strip_rbl_response(blended_v2[,n])
    blended_v2[new_field] = "NOT listed"
    blended_v2[matches,new_field] = "listed"
    blended_v2[,new_field] = as.factor(blended_v2[,new_field])
}

#Senderbase is a bit different. 
blended_v2$senderbase_response_score = "NOT scored"
#Therefore, treating individually
pattern = ".+(descriptive text).+\"(.*)\".*"
got_it = grep(pattern, blended_v2$senderbase_response, value = FALSE)
blended_v2[got_it,]$senderbase_response_score = gsub(pattern, "\\2", blended_v2[got_it,]$senderbase_response)
blended_v2$senderbase_response_score=as.factor(blended_v2$senderbase_response_score)

#snapshot dump
write.table(blended_v2, file="train_v3_blended_v2.csv", quote = FALSE, sep=";")

#Now I feel like our dataframe is ready to play.
#Let's do it in R-markdown





