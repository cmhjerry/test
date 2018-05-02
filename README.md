# test

raw_data = read.csv('inventory_clean.csv')

dim(raw_data)
head(raw_data)
summary(raw_data)

colnames(raw_data)
t = subset(raw_data,select = -c(sku,rev_stop,X,deck_risk,oe_constraint,ppap_risk,stop_auto_buy,potential_issue))
colnames(t)

t1 <- na.omit(t)
dim(t1)
summary(t1)

for (i in 1:length(title)){
  sub[is.na(sub[title[i]]),title[i]] <- median(sub[[title[i]]],na.rm = T)
}

attach(t1)
par(mfrow=c(2,2))
fit1 = lm(lead_time~in_transit_qty,data = t1)
plot(fit1)
summary(fit1)
newdata = data.frame(in_transit_qty=80)
predict(fit1,newdata)

model <- glm (deck_risk ~ ., data = dresstrain, family = binomial)
model

