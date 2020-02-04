data<-read.csv(file="/Users/beta/desktop/data2.csv",header = T)

couponrate<-c(0.015,0.035,0.0075,0.0075,0.0075,0.005,0.0175,0.0225,0.015,0.0125)
fv<-100
cleanprice<-data[5:14,2:11]
n<-c(152,61,152,152,152,152,152,152,152,152)
ai<-n/365*couponrate*fv
ma<-c(0.082191781,0.334246575,0.58630137,1.082191781,1.58630137,2.082191781,3.082191781,4.084931507,4.589041096,5.084931507)

accruedinterest<-matrix(nrow = 10,ncol = 10)
dirtyprice<-matrix(nrow = 10,ncol = 10)

for (i in 1:10){
  accruedinterest[i,]<-ai
}

for (i in 1:10){for (m in 1:10){
  dirtyprice[m,i]<-as.numeric(as.character(cleanprice[m,i]))+accruedinterest[m,i]
}
}

coupon<-fv*couponrate

install.packages("rootSolve")
library(rootSolve)

#4a
uni<-matrix(nrow = 10,ncol = 10)
for (m in 1:10){
  for (a in 1:10){
    fun<-function(x){
      if (a==1) {
        total<-(fv+0.5*coupon[a])*(1+x/2)^(-ma[1]*2)-dirtyprice[m,a]
      }
      else if (a==2) {
        total<-(fv+0.5*coupon[a])*(1+x/2)^(-ma[2]*2)-dirtyprice[m,a]
      }
      else if (a==7){
        total<-0
        for (i in 1:6){
          total<-total+0.5*coupon[a]*(1+x/2)^(-2*ma[7]+i)
        }
        total<-total+(fv+0.5*coupon[a])*(1+x/2)^(-ma[7]*2)-dirtyprice[m,a]
      }
      else if (a >= 8){
        total<-0
        for (i in 1:a){
          total<-total+0.5*coupon[a]*(1+x/2)^(-2*ma[a]+i)
        }
        total<-total+(fv+0.5*coupon[a])*(1+x/2)^(-ma[a]*2)-dirtyprice[m,a]
      }
      else{
        total<-0
        for (i in 1:(a-2)){
          total<-total+0.5*coupon[a]*(1+x/2)^(-2*ma[a]+i)
        }
        total<-total+(fv+0.5*coupon[a])*(1+x/2)^(-ma[a]*2)-dirtyprice[m,a]
      }
    }
    uni[m,a]<-uniroot(fun, lower=0, upper=5, extendInt = "yes")$root
  }
}

print(uni)

matplot(t(uni),type = "l")
axis(1,at=c(1:10),labels=ma)
title("5-Year Yield Curve",xlab="Maturity(year)")

#4b

sr<-matrix(nrow = 9,ncol = 9)
for (m in 1:10){
  sr[m,1]<-(uni[m,1]*122/184)+(uni[m,3]*62/184)
}

for (m in 1:10){
  sr[m,1]<-(uni[m,1]*122/184)+(uni[m,3]*62/184)
  for (i in 3:10){
    if (i==3) {
      fun<-function(x){
        total<-0.5*coupon[i]*(1+sr[m,1])^(-2*ma[1])
        total<-total+(fv+0.5*coupon[i])*(1+x)^(-2*ma[1]-1)-dirtyprice[m,i]
      }
      sr[m,2]<-uniroot(fun, lower=0, upper=5, extendInt = "yes")$root
    }
    else if (i<=6) {
      fun<-function(x){
        total<-0.5*coupon[i]*(1+sr[m,1])^(-2*ma[1])+0.5*coupon[i]*(1+sr[m,2])^(-2*ma[1]-1)
        for (a in 4:i-1){
            total<-total+0.5*coupon[i]*(1+sr[m,a-1])^(-2*(ma[1]+0.5*(a-2)))
        }
        total<-total+(fv+0.5*coupon[i])*(1+x)^(-2*(ma[1]+0.5*(i-2)))-dirtyprice[m,i]
      }
      sr[m,i-1]<-uniroot(fun, lower=0, upper=5, extendInt = "yes")$root
    }
    else if (i==7){
      fun<-function(x){
        total<-0.5*coupon[i]*(1+sr[m,1])^(-2*ma[1])+0.5*coupon[i]*(1+sr[m,2])^(-2*ma[1]-1)
        for (p in 4:6){
          total<-total+0.5*coupon[i]*(1+sr[m,p-1])^(-2*(ma[1]+0.5*(p-2)))
        }
        total<-total+(fv+0.5*coupon[i])*(1+x)^(-2*(ma[1]+0.5*6))-dirtyprice[m,i]
      }
      sr[m,i-1]<-uniroot(fun, lower=0, upper=5, extendInt = "yes")$root
    }
    else if ((8<=i)&(i<=10)){
      fun<-function(x){
        total<-0.5*coupon[i]*(1+sr[m,1])^(-2*ma[1])+0.5*coupon[i]*(1+sr[m,2])^(-2*ma[1]-1)
        for (p in 4:6){
          total<-total+0.5*coupon[i]*(1+sr[m,p-1])^(-2*(ma[1]+0.5*(p-2)))
        }
        total<-total+0.5*coupon[i]*(1+sr[m,6])^(-2*(ma[1]+0.5*6))
        for (p in 8:i-1){
          total<-total+0.5*coupon[i]*(1+sr[m,p-1])^(-2*(ma[1]+0.5*p))
        }  
        total<-total+(fv+0.5*coupon[i])*(1+x)^(-2*(ma[1]+0.5*i))-dirtyprice[m,i]
      }
      sr[m,i-1]<-uniroot(fun, lower=0, upper=5, extendInt = "yes")$root
    }
  }
}


print(sr)
spotrate<-matrix(nrow = 9,ncol = 10)
for (i in 1:10){
  spotrate[i,1]<-sr[i,1]                          #2020/7/1
  spotrate[i,2]<-122/181*sr[i,2]+59/181*sr[i,3]   #2021/1/1
  spotrate[i,3]<-122/184*sr[i,3]+62/184*sr[i,4]   #2021/7/1
  spotrate[i,4]<-122/181*sr[i,4]+59/181*sr[i,5]   #2022/1/1
  spotrate[i,5]<-122/365*sr[i,5]+243/365*sr[i,6]  #2022/7/1
  spotrate[i,6]<-306/365*sr[i,5]+59/365*sr[i,6]   #2023/1/1
  spotrate[i,7]<-122/366*sr[i,6]+60/366*sr[i,7]   #2023/7/1
  spotrate[i,8]<-306/366*sr[i,6]+60/366*sr[i,7]   #2024/1/1
  spotrate[i,9]<-122/184*sr[i,7]+62/184*sr[i,8]    #2024/7/1
  spotrate[i,10]<-122/181*sr[i,8]+59/181*sr[i,9]  #2025/1/1
}

print(spotrate)
matplot(t(spotrate),type = "l")
axis(1,at=c(1:10),labels=c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5))
title("5-Year Spot Curve",xlab="Year")

#4c
m<-0.920547945
spotrate2<-matrix(nrow = 9,ncol = 5)
for (i in 1:9){
  spotrate2[i,1]<-spotrate[i,2]
  spotrate2[i,2]<-spotrate[i,4]
  spotrate2[i,3]<-spotrate[i,6]
  spotrate2[i,4]<-spotrate[i,8]
  spotrate2[i,5]<-spotrate[i,10]
}
print(spotrate2)
forwardrate<-matrix(nrow = 9,ncol = 4)

for (i in 1:4){
  for (a in 1:9){
    if (i==1){
      forwardrate[a,i]=(1+spotrate2[a,2])^(m+1)*(1+spotrate2[a,1])^(-m)-1
    }
    else{
      forwardrate[a,i]=(1+spotrate2[a,i+1])^(m+i)*(1+spotrate2[a,i])^(-m-i+1)-1
    }
  }
}
print(forwardrate)
matplot(t(forwardrate),type = "l")
title("5-Year Forward Curve",xlab="Year")

#5
#yield:
Xy<-matrix(nrow = 8,ncol = 5)
for (i in 1:8){
  for (a in 1:5){
    if (a ==1){
      Xy[i,1]<-log(uni[i+1,1]/uni[i,1])
    }
    if (a==2){
      Xy[i,2]<-log(uni[i+1,4]/uni[i,4])
    }
    if (a==3){
      Xy[i,3]<-log(uni[i+1,6]/uni[i,6])
    }
    if (a==4){
      Xy[i,4]<-log(uni[i+1,7]/uni[i,7])
    }
    if (a==5){
      Xy[i,5]<-log(uni[i+1,8]/uni[i,8])
    }
  }
}
print(Xy)  
Cy<-cov(Xy)
print(Cy)
#forward:
Xf<-matrix(nrow = 8,ncol = 4)
for (i in 1:4){
  for (a in 1:8){
    Xf[a,i]<-log(forwardrate[a+1,i]/forwardrate[a,i])
  }
}
print(Xf)  
Cf<-cov(Xf)
print(Cf)

#6
#yield:
eigenvaluey<-eigen(Cy)
print(eigenvaluey)
#forward:
eigenvaluef<-eigen(Cf)
print(eigenvaluef)
