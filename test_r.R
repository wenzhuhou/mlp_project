
setwd('../mlp_interview_project/')
library(reshape2)  # for data wrangling
library(quadprog)  # for QP optimize

# ---------- Maximum Overlap ----------------
data = data.frame(id=seq(10,80,by=10),
              anest=c("baker","baker",rep("dow",6)), 
              start=c("08:00","09:00","09:00","08:00",
                      "10:00","12:30","13:30","18:00"),
              end=c("11:00","13:00","15:30","13:30",
                    "11:30","13:30","14:30","19:00"))


# (i)	Write a query to list out the ids each row is intersecting with.
# Append answer to the last column as w

data$start_t <- sapply(data$start, as.POSIXct, format='%H:%M')
data$end_t <- sapply(data$end, as.POSIXct, format='%H:%M')


# to record the repetative id
# method 1
temp <- list()
for (i in 1:nrow(data)){
    temp <- c(temp, list(data$id[i]))
}

for (i in 1:(nrow(data)-1)){
    for (j in (i+1):nrow(data)){
        if(data[i, 'anest']==data[j, 'anest']){
            start1 = (data[j, 'start_t'] >= data[i, 'start_t'])
            start2 = (data[j, 'start_t'] < data[i, 'end_t'])
            end1 = (data[j, 'end_t'] > data[i, 'start_t'])
            end2 = (data[j, 'end_t'] <= data[i, 'end_t'])
            if ((start1&start2)|(end1&end2)){
                    temp[[i]] = c(temp[[i]], data[j, 'id'])
                    temp[[j]] = c(temp[[j]], data[i, 'id'])
            }
        }
        
    }
}

# method 2
data = data[order(data$anest, data$start_t),]

temp <- list()
for (i in 1:nrow(data)){
    temp <- c(temp, list(data$id[i]))
}

for (i in 1:(nrow(data)-1)){
    for (j in (i+1):nrow(data)){
            if((data[i, 'anest']==data[j, 'anest'])&
               (data[j, 'start_t'] < data[i, 'end_t'])){
                    temp[[i]] = c(temp[[i]], data[j, 'id'])
                    temp[[j]] = c(temp[[j]], data[i, 'id'])
            }
    }
}

data$w <- temp
head(data)
# (ii)	Write a query to determine for each anest the max number
# intersecting ids over periods of intersection. 
# Append answer as the last column as s

data$s <- 1
# method 1: naive approach O(n(1+n)/2)
n <- nrow(data)

for (i in 1:(n-1)){
    for(j in (i+1):n){
        if((data[i, 'anest'] == data[j, 'anest'])&
           (data[i, 'id'] %in%  data[j, 'w'][[1]])){
                data[i, 's'] <- data[i, 's']  + 1
                data[j, 's'] <- data[j, 's']  + 1
        }
            
    }
}

# method 2

get_overlap <- function(m_data){
    # record the event of starting and ending
    event <- data.frame(c(m_data[,'id'], m_data[,'id']), 
                          c(m_data[,'start_t'], m_data[,'end_t']), 
                          c(rep(1, nrow(m_data)), rep(-1,nrow(m_data))))
    colnames(event) <- c('id', 'time', 'pos')
    event <- event[order(event$time),]
    
    # aggregate event number at the same time
    event_agg <- aggregate(event$pos, by=list(event$time), FUN=sum)
    event_agg$cum <- cumsum(event_agg$x)
    colnames(event_agg) <- c('time', 'pos', 'cum')
    return(event_agg)
}

get_max_overlap <-function(all_data, m_data, m_overlap){
    for (i in 1:nrow(m_data)){
        all_data[all_data[, 'id']==m_data[i, 'id'], 's'] <- max(
            subset(m_overlap, time>=m_data[i,'start_t'] & 
                              time < m_data[i, 'end_t'],
                    select=cum))
    }
    return(all_data)
}

m_data1 <- subset(data, anest=='baker')
overlap_1 <- get_overlap(m_data1)

m_data2 <- subset(data, anest=='dow')
overlap_2 <- get_overlap(m_data2)

data <- get_max_overlap(data, m_data1, overlap_1)
data <- get_max_overlap(data, m_data2, overlap_2)
head(data)

# --------------- Pascal Triangle ------------------ 

n <- 3
record <- list()

get_next_pascal<-function(m_arr){
    m_len <- length(m_arr)
    new_arr <- rep(1, m_len+1)
    if(m_len > 1){
        for (i in 2:(m_len)) {
            new_arr[i] <- m_arr[i-1] + m_arr[i]
        }
    }
    return(new_arr)
}

get_pascal<-function(m_level){
    arr_now <- c(1)
    m_pascal <- list(arr_now)
    if (m_level > 1){
        for (j in 1:(m_level)){
            arr_now <- get_next_pascal(arr_now)
            m_pascal <- c(m_pascal, list(arr_now))
        }
    }
    return(m_pascal)
}

get_pascal(10)

# ----------- Portfolio VaR & CVaR -----------------


# TICKER PERMNO
# XOM  11850, 10%
# IBM  12490, 20%
# AAPL  14593, 15% 
# BP  29890, 15%
# GS  86868, 5%
# COST  87055, 15%
# GOOG  90319, 20%

# read in
stk <- read.csv('stocks.csv')
stk$date <- as.Date(as.character(stk$date), format='%Y%m%d')

# check nan
print(sum(stk$RET[stk$RET %in% c(-66.0, -77.0, -88.0, -99.0)])) 
print(sum(is.na(stk$RET)))

# pivot table
stk_md <- melt(stk, id=c('PERMNO', 'date'), measure='RET')
stk_pivot <- dcast(stk, date~PERMNO)

# portfolio weight
pf <- data.frame(c(11850, 12490, 14593, 29890, 86868, 87055, 90319),
                 c(0.1, 0.2, 0.15, 0.15, 0.05,  0.15, 0.20))
colnames(pf) <- c('PERMNO', 'weight')
pf_t <- t(pf[,2])
colnames(pf_t) <- as.character(pf[,1])

# get the return for holding each stocks starting 2016-01
m_weight <- pf_t[,colnames(stk_pivot)[-1]]
stk_select <- stk_pivot[stk_pivot$date>as.Date('2016-01-01', 
                                               format='%Y-%m-%d'), 
                        colnames(pf_t)]

# get the price evolvment of each stocks
stk_select_p <- apply(stk_select+1, 2, cumprod)
stk_select_p <- data.frame(stk_select_p)
colnames(stk_select_p) <- colnames(pf_t)
# suppose we distribute one dollar according to weights
stk_idx_p <- as.matrix(stk_select_p) %*% as.matrix(m_weight)

# get the portfolio return
pf_ret <- data.frame(diff(stk_idx_p)/stk_idx_p[-length(stk_idx_p)])

date_select <- stk_pivot[stk_pivot$date>as.Date('2016-01-01', 
                                                format='%Y-%m-%d'),
                         'date']
pf_ret$date <- date_select[-1]
colnames(pf_ret) <- c('ret', 'date')

# 1. Using historical data from 2016/01/01 to 2016/12/31
# assumptions:
# 1) loss is normally distributed
# 2) loss is iid
mu <- mean(-pf_ret[,'ret'])
std <- sd(-pf_ret[,'ret'])

# percentile
x <- 0.95

cal_VaR <- function(m_mu, m_sd, x){
    # calculate the VaR based on loss (negative of profit)
    return(m_mu + m_sd*qnorm(x))
}

cal_CVaR <- function(m_mu, m_sd, x){
    # calculate the CVaR based on loss (negative of profit)
    return(m_mu + m_sd*exp(-qnorm(x)**2/2.0)/(sqrt(2.0*pi)*(1.0-x)))
}

# 1-day VaR and CVaR
VaR <- cal_VaR(mu, std, x)
CVaR <- cal_CVaR(mu, std, x)
print(c(VaR, CVaR))

# portfolio at the end of 2016
capital <-  prod(1.0 + pf_ret[pf_ret[,'date'] > as.Date('2016-01-01', 
                                   format='%Y-%m-%d'), 'ret'])
# suppose the initial capital is $1000
# then the one-day VaR for the portfolio at the end of 2016:
print(c('VaR','CVaR'))
print(1000 * capital*c(VaR, CVaR))

# 2. Using Model-based estimation of VaR
m_cov <- cov(stk_select, stk_select)
var_model <- t(as.matrix(m_weight)) %*% as.matrix(m_cov) %*% as.matrix(m_weight)
sd_model <- sqrt(var_model)
m_VaR <- cal_VaR(mu, sd_model, x)
m_CVaR <- cal_CVaR(mu, sd_model, x)
print(c('Model-VaR','Model-CVaR'))
print(1000 * capital*c(m_VaR, m_CVaR))


# 3. Rebalance weight at the end of each month
stk_pivot$month <- format(stk_pivot$date, '%m')

# get monthly time stamp
mth_end <- c()
mth_start <- c(stk_pivot[1, 'date'])
for (i in 2:nrow(stk_pivot)){
    if (stk_pivot[i, 'month']!=stk_pivot[i-1, 'month']){
        mth_end <- c(mth_end, stk_pivot[i-1, 'date'])
        mth_start <- c(mth_start, stk_pivot[i, 'date'])
    }
}
mth_end <- c(mth_end, stk_pivot[nrow(stk_pivot), 'date'])
mth_end <- as.Date(mth_end)


# Cal weight
weight_record <- ts(data.frame(matrix(0,nrow=(length(mth_end)-121+1), ncol=7)),
                    start=c(2016, 1), end=c(2016, 12), frequency = 12)
colnames(weight_record) <- colnames(pf_t)
for(i in 121:length(mth_end)){
    # using t-12 to t-1 data to avoid short-term reversals
    data_mth <- subset(stk_pivot, 
                       date >= mth_start[i-12] & date <=mth_end[i-1],
                       select=colnames(pf_t))
    
    # Quardratic Programming
    covar <- cov(data_mth)
    ret_vec <- colMeans(data_mth) * 0.03
    Amat <- matrix(1, nrow=nrow(covar))
    b0 <- 1
    meq <- 1
    
    qp <- solve.QP(covar, ret_vec, Amat, b0, meq)
    
    # record solution
    weight_record[i-120, ] <- qp$solution
}

print(round(weight_record, digits=3))

# ------------- Position Calculator --------------
pos <- read.csv('pos.csv')
trd <- read.csv('trd.csv')

# i. net position per user
net_pos <- dcast(pos, sym~user, sum, value.var = 'pos')
head(net_pos)

write.table(net_pos, file='output4-1.txt', append=TRUE)

# ii. list out all boxed positions 
unique(pos$pb)
# first, aggregate positions for each pb
boxed_pos <- dcast(pos, user+pb~sym, sum, value.var = 'pos')
# second, check for boxed
boxed_pos_m <- melt(boxed_pos, id=c('user', 'pb'))
colnames(boxed_pos_m)[3] <- 'sym'
boxed_pos_c <- dcast(boxed_pos_m, user~sym, prod)

# all the negative value means the position is boxed
result <- list()
for (i in 1:nrow(boxed_pos_c)){
    temp <- boxed_pos_c[i, ]
    boxed_sym <- colnames(temp[,-1])[temp[,-1] < 0 ]
    result <- c(result, list(subset(boxed_pos_m, 
                                    user==temp[,1] & sym %in% boxed_sym,
                                    select = c('user', 'sym', 'value')
                                    )
                             )
                )
}

print(result)
temp_result <- c()
for (i in 1:length(result)){
    temp_result <- rbind(temp_result, result[[i]])
}
write.table(temp_result, 'output4-2.txt')

# iii. Find all the potential crossings
trd <- trd[order(trd$sym), ]
trd <- transform(trd, jrnl=ifelse((qty>=0), 0, -qty))
trd <- transform(trd, trd=ifelse((qty>=0), qty, 0))
# or use lambda function in R
# trd$jrnl <- sapply(trd$qty,FUN=function(x) if(x>=0) 0 else -x)
head(trd)

write.table(trd, 'output4-3.txt')

# iv. Find the total quantity to trade
trd_total<-aggregate(trd[,c('jrnl','trd')],  by=list(trd$sym), FUN=sum)
colnames(trd_total)[1] <- 'sym'
head(trd_total)

write.table(trd_total, 'output4-4.txt')

# v. Find the final position per user, per sym
net_pos <- dcast(pos, sym~user, sum, value.var = 'pos')
trd_pivot <- dcast(trd, sym~user, value.var = 'qty', sum, fill=0.0)

agg_pos <- rbind(melt(net_pos, id='sym'), melt(trd_pivot, id='sym'))
final_pos <- dcast(agg_pos, sym~variable, sum)
head(final_pos)

write.table(final_pos, 'output4-5.txt')

# vi. Unit test
## test i, net pos
# input

if(sum(pos$pos)==sum(net_pos[,-1])){
    print('Net position mathced')
} else {
    print('Net position not right')
}

## test ii. boxed
sum(boxed_pos_m$value) == sum(net_pos[,-1])

## test iii. jrnl and trd
sum(trd$qty + trd$jrnl - trd$trd) == 0

## test iv. total quantity to trade
sum(trd$trd) == sum(trd[trd$qty>0,'qty'])
sum(trd$jrnl) == -sum(trd[trd$qty<0,'qty'])

## test v. final pos
sum(final_pos[,-1]) == sum(pos$pos) + sum(trd$qty)

# -- end ---


