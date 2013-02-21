library(statnet)
library(ggplot2)
library(XML)
library(RCurl)
library(plyr)
library(maps)

setwd('106')
senbills = read.csv('senate_bills.txt',header=F)
names(senbills) = c('Congress','BillType','BillNum','Private')
senbills = subset(senbills, Congress == 106)
rownames(senbills) = NULL

housebills = read.csv('house_bills.txt',header=F)
names(housebills) = c('Congress','BillType','BillNum','Private')
housebills = subset(housebills, Congress == 106)
rownames(housebills) = NULL

senmtx = read.csv('106_senmatrix.txt',header=F)
cols = ncol(senmtx)
names(senmtx) = seq(1,cols)

senators = read.csv('106_senators.txt',header=F)
names(senators) = c('Name','Thomas','ICPSR')

billsspons = apply(senmtx == 1,1,sum)
senators$billsSponsored = billsspons

billscospons = apply(senmtx == 2 | senmtx == 3,1,sum)
senators$billsCoSponsored = billscospons

billswithdrawn = apply(senmtx == 5,1,sum)
senators$billsWithdrawn = billswithdrawn

#Get states and parties of all senators
url = getURL('http://projects.washingtonpost.com/congress/106/senate/members/')
senTable = readHTMLTable(url)
names(senTable)[4] = 'SenatorList'
senTable = senTable$SenatorList
senTable = senTable[,-3] 
names(senTable) = c('Party','Name','State','VotesWithParty')
senNames = senTable$Name
senNames = as.character(senNames)
senNames = sapply(senNames,function(x) tail(strsplit(x,' ')[[1]],1))
names(senNames) = c(1:102)
senTable$Name = senNames
senTable = senTable[order(senTable$Name),]
senators$Party = senTable$Party
senators$State = senTable$State

r = nrow(senators)
#cols = nrow(senbills)
spons.mtx <- matrix(0,r,r)

getCosponsors <- function(x){
  # Find out which bills were sponsored by Senator x
  sponsored <- which(senmtx[x,] == 1)
  # Get the ids of all senators who co-sponsored
  # the bill sponsored by 1.
  cospons.mtx = senmtx[,sponsored] == 2 | senmtx[,sponsored] == 3
  # cospons.mtx need not be a matrix, say if only one bill was sponsored.
  if (! is.matrix(cospons.mtx))
    return(as.numeric(cospons.mtx))
  bills.cosponsored =  apply(cospons.mtx,1,sum)
  bills.cosponsored
}

spons.edgelist = data.frame()
for (x in seq(1,r)){
  bills.cospons <- getCosponsors(x)
  if (! length(bills.cospons) == 0){
    avg = mean(bills.cospons)
    spons.mtx[x,] <- bills.cospons
    filtered.cosponsors = which(bills.cospons > 3*avg)
    for (co in filtered.cosponsors){
      spons.edgelist <- rbind(spons.edgelist,c(co,x,bills.cospons[co]))
    }
  }
}
names(spons.edgelist) = c("Senator1","Senator2","Weight")

# colsums = apply(spons.mtx,2,sum)
# which(colsums == max(colsums))
# senators$sponsors = colsums
# 
# rowsums = apply(spons.mtx,1,sum)
# which(rowsums == max(rowsums))
# senators$cosponsors = rowsums

senators = senators[,-c(2,3)]
net = network(spons.edgelist[,1:2])
set.edge.attribute(net,"weight",spons.edgelist[,3])
set.vertex.attribute(net,"State",as.character(senators$State))
set.vertex.attribute(net,"Party",as.character(senators$Party))
sen.party = senators$Party
party.cols = rep(0,102)
party.cols[sen.party=='D']<-"blue"
party.cols[sen.party=='R']<-"red"
plot(net,displaylabels=T, vertex.col = party.cols)

# Get rid of the withdrawn bills
senmtx[senmtx > 3] = 0
dates = read.csv('106_sendatematrix.txt',stringsAsFactors = F)
#as.Date(x,"%m/%d/%Y")
date.list = NULL
r = nrow(dates)
c = ncol(dates)

colsums = apply(senmtx,2,function(x) sum(x > 0))
x = sort(colsums,decreasing=T)
bills.pop = which(colsums == 100)

# for (i in 1:r){
#   for (j in 1:c){
#     x = dates[i,j]
#     if (! is.na(x)){
#       x = as.Date(x,"%m/%d/%Y")
#       date.list = rbind(date.list,x) 
#     }
#   }
# }
# date.list = as.Date(date.list, origin = "1970-01-01")
# x = dates[1,]
# z = sapply(x,function(y) if (!is.na(y)) as.Date(y,"%m/%d/%Y"))
# z = unlist(z)
# z = as.Date(z, origin = "1970-01-01")
# a = sapply(senmtx[1,], function(y) if (y != 0) return(y))
# a= unlist(a)
# sen.activity = data.frame(dates = z, billstatus = a)
# sen.activity$billstatus = factor(sen.activity$billstatus)
# rownames(sen.activity) = NULL
# ggplot(sen.activity, aes(x=dates)) + geom_histogram() + facet_grid(billstatus~.)
# ggplot(sen.activity, aes(x=dates,fill=billstatus)) + geom_density(alpha = 0.3)
# 
# sen.dist = dist(spons.mtx)
# mds = as.data.frame(cmdscale(sen.dist))
# colnames(mds) = c("x","y")
# senators$x = mds$x
# senators$y = mds$y
# billsGroup = cut(senators$billsSponsored, breaks = c(0,100,200,400), 
#                  labels=c("<= 100","> 100 & <= 200","> 200"))
# ggplot(senators, aes(x=x,y=y,colour=Party, shape=billsGroup)) + geom_point(size=4) + 
#   ggtitle('Clustering senators based on party') + 
#   scale_color_manual(values=c("blue","red"))

state.coords = data.frame(state=state.abb,long = state.center$x, lat = state.center$y)
senators = merge(senators,state.coords,by.x="State",by.y="state")
# Y U sort by state merge!
senators = arrange(senators,Name)

# library(igraph)
# el = spons.edgelist[,1:2]
# el = as.matrix(el)
# g = graph.edgelist(el)
# V(g)$Color = party.cols
# V(g)$Party = as.character(sen.party)
# V(g)$Size = 6
# V(g)$State = as.character(senators$State)
# V(g)$Label = as.character(1:102)
# V(g)$Name = senTable$Name
# E(g)$weight = spons.edgelist[,3]
# V(g)$longitude = senators$long
# V(g)$latitude = senators$lat
# write.graph(g, "senators.graphml", format = "graphml")
#plot(g, layout=layout.fruchterman.reingold)
