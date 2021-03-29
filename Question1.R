library(survey)
library(sampling)
library(ggplot2)

cor <- corona330[,c(1,2,3,6)]
colnames(cor) <- c("Province.State","Country.Region","Cases","Deaths")
N <- nrow(cor)
cor$fpc <- N
n <- 58

totx_a <-rep(NA,100)
bias_a <-rep(NA,100)
re_a <- rep(NA,100)
bias_re <- rep(NA,100)
thatr_a <- data.frame(1:100,1:2)
colnames(thatr_a)<-c("REtotest","SE")

#actual death count
truetotdeath <- sum(cor$Deaths)

for(i in 101:200){
  set.seed(i)
  corsamp <- cor[sort(sample(N,n)),]
  corsampdes <- svydesign(id = ~1, strata = NULL, data = corsamp, fpc = ~fpc)
  sampdeathtot <- svytotal(~Deaths, design=corsampdes, na.rm=TRUE)
  bias <- sampdeathtot - truetotdeath
  bias_a[i-100] <- bias
  totx_a[i-100] <- sampdeathtot
  ratestdeath <- svyratio(~Deaths, ~Cases, corsampdes)
  re_a[i-100] <- ratestdeath$ratio
  thatr <- predict(ratestdeath,total = sum(cor$Cases),se=TRUE)
  retotal <- as.numeric(thatr$total)
  thatr_a[i-100,1] <- retotal
  thatr_a[i-100,2] <- as.numeric(thatr$se)
  bias_ratio <- retotal - truetotdeath
  bias_re[i-100] <- bias_ratio
}

totx_a<-data.frame(totx_a)
bias_a<-data.frame(bias_a)
bias_re<-data.frame(bias_re)
re_a<-data.frame(re_a)

totaplot <- ggplot(totx_a,aes(totx_a)) + geom_histogram(bins=30)
totaplot

biasaplot <- ggplot(bias_a,aes(bias_a)) + geom_histogram(bins=30)
biasaplot

var_a<-var(totx_a)
var_a


ratestdeath <- svyratio(~Deaths, ~Cases, corsampdes)
ratestdeath
thatr <- predict(ratestdeath,total = sum(cor$cases),se=TRUE)
thatr
