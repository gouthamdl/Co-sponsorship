source('Sponsorship.R')

bills.states = ddply(senators,.(State), function(x) sum(x$billsSponsored))
names(bills.states) = c("Abb","BillsSponsored")
state.names = data.frame(State=tolower(state.name),Abb = state.abb)
bills.states = merge(bills.states,state.names)
bills.states = arrange(bills.states,BillsSponsored,decreasing=T)

# Plot the map
states_map <- map_data("state")
bills_map = merge(states_map,bills.states,by.x='region',by.y='State')
#bills_map <- arrange(bills_map,group,order)
# ggplot(bills_map,aes(x=long,y=lat,group=group,fill=BillsSponsored)) +
#   geom_polygon(colour="black") + coord_map("polyconic")
billmap = ggplot(bills.states,aes(map_id=State,fill=BillsSponsored)) + 
  geom_map(map = states_map,colour="black") + 
  scale_fill_gradient2(low="#559999",mid="grey90",high="#BB650B",midpoint=median(bills.states$BillsSponsored)) + 
  expand_limits(x = states_map$long,y=states_map$lat) +
  coord_map("polyconic")

bills.parties = ddply(senators,.(Party), function(x) sum(x$billsSponsored))
names(bills.parties)[2] = "BillsSponsored"
bills.parties = arrange(bills.parties,BillsSponsored,decreasing=T)

senators = arrange(senators,billsSponsored,decreasing=T)
sub = senators[1:30,]
sub = arrange(sub,Party)
sub$Name = factor(sub$Name, levels = sub$Name, ordered = T)

ggplot(sub, aes(x=billsSponsored, y = reorder(Name,billsSponsored))) + 
  geom_segment(aes(yend = Name),xend = 0,colour="grey50") + 
  geom_point(size = 3, aes(colour=Party)) + 
  scale_colour_brewer(palette="Set1", limits=c("R","D"), guide = F) + 
  theme_bw() + theme(panel.grid.major.y = element_blank()) + 
  facet_grid(Party~.,scales = "free_y",space="free_y") + xlab('Bills Sponsored') + ylab('Senator') + ggtitle('Bills Sponsored By Party')

dates = read.csv('106_sendatematrix.txt',stringsAsFactors = F)
#as.Date(x,"%m/%d/%Y")

# Distances between the senators in the network
dists = geodist(net)$gdist
apply(dists, 1 , function(x) sum(x > 2))
melt.dists = melt(dists)
names(melt.dists) = c("Senator1,Senator2,Distance")
melt.dists$Distance = factor(melt.dists$Distance)
p = ggplot(melt.dists,aes(x=Senator1,y=Senator2,fill=Distance))
p + geom_tile() + xlab("Senator") + ylab("Senator")