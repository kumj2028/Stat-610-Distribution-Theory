### birthday.r
### compute and simulate probability that no 2 people have same birthday in a group of size k
### solution is 1 - P_{365,k}/365^k

### computing the probabilities
npeople = seq(10,60,by=5)  # vector of choices for k
probs = 1-exp(lfactorial(365)-lfactorial(365-npeople)-npeople*log(365))  # compute probability
cbind(npeople,probs)  # print result (cbind puts the two vectors into columns)

npeople = 20:30  # vector of choices for k
probs = 1-exp(lfactorial(365)-lfactorial(365-npeople)-npeople*log(365))  # compute probability
cbind(npeople,probs)  # print result


### simulating to estimate the probability with k = 23
npeople = 23
nsample = 100000
dup.true = rep(0,nsample)  # vector to hold simulation results
for (i in 1:nsample){
  birthdays = sample(1:365,npeople,replace=TRUE)  # random sample with replacement
  dup.true[i] = (length(unique(birthdays))!=npeople)  # return TRUE if there is a duplication
}  # end for
prob = sum(dup.true)/nsample  # estimate the probability that at least one birthday is duplicated
prob

###

