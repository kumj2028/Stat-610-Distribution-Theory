### quordle.r
### compute probability that no 2 games have same words in k games
### solution is 2309!/((4!)^k*(2309-4k)!*(2309 choose 4)^k)

### computing the probabilities
kgames = seq(1,20)  # vector of choices for k
probs = exp(lfactorial(2309)-kgames*lfactorial(4)-
lfactorial(2309-4*kgames)-kgames*lchoose(2309, 4))
probs_c = 1-probs
cbind(kgames,probs,probs_c) 