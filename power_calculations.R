
# power calculations
# epi 514
# olivia schultes
# created: april 20 2022
# last updated: april 26 2022

# description: perform power calculations using a range of assumptions
  # for the project proposal


# packages
library(epiR)


# calculate sample sizes of exposure groups
  # using prevalence estimates of 5%, 10%, 15%, and 20% 
  # for each exposure category
ssize=vector()
for (i in c(.05,.1,.15,.2)) {
  ssize=c(ssize,(i*125122))
  round(ssize)
}

# calculate minimum detectable prevalence ratio

# test function
epi.ssxsectn(
  pdexp1=NA,
  pdexp0=.09,
  n=55679,
  power=.8,
  r=7507/48172
)

# put test function into for() loop and run through exposure sample sizes
  # assuming prevalence of outcome is 9%, 11%, or 13%

# calculate prevalence 9%
prev09=list()

for (i in ssize) {
  tmp=epi.ssxsectn(
    pdexp1=NA,
    pdexp0=.09,
    n=i+48172,
    power=.8,
    r=i/48172
  )
  prev09=c(prev09,tmp$pr[2])
}

prev09

# calculate prevalence 11%
prev11=list()

for (i in ssize) {
  tmp=epi.ssxsectn(
    pdexp1=NA,
    pdexp0=.11,
    n=i+48172,
    power=.8,
    r=i/48172
  )
  prev11=c(prev11,tmp$pr[2])
}

prev11

# calculate prevalence 13%
prev13=list()

for (i in ssize) {
  tmp=epi.ssxsectn(
    pdexp1=NA,
    pdexp0=.13,
    n=i+48172,
    power=.8,
    r=i/48172
  )
  prev13=c(prev13,tmp$pr[2])
}

prev13

