### Representativeness values


#### Packages 
library(tidyverse)
library(readr)


setwd("/Users/hannahwood/Desktop/R 5 sep/")

#############################################################################

####### Stats from trip which are complete and used to do the KDEs

### read in colony trip summaries

bar <- read_csv("BAR_Kittiwake_KDE_state_ALL_repr_assessment.csv")
bem <- read_csv("BEM_Kittiwake_KDE_state_ALL_repr_assessment.csv")
coq <- read_csv("COQ_Kittiwake_KDE_state_ALL_repr_assessment.csv")
csy <- read_csv("CSY_Kittiwake_KDE_state_ALL_repr_assessment.csv")
# fai <- read_csv("FAI_Kittiwake_KDE_state_ALL_repr_assessment.csv")
fil <- read_csv("FIL_Kittiwake_KDE_state_ALL_repr_assessment.csv")
fow <- read_csv("FOW_Kittiwake_KDE_state_ALL_repr_assessment.csv")
iom <- read_csv("IOM_Kittiwake_KDE_state_ALL_repr_assessment.csv")
lam <- read_csv("LAM_Kittiwake_KDE_state_ALL_repr_assessment.csv")
cop <- read_csv("ORK_COP_Kittiwake_KDE_state_ALL_repr_assessment.csv")
mks <- read_csv("ORK_MKS_Kittiwake_KDE_state_ALL_repr_assessment.csv")
puf <- read_csv("PUF_Kittiwake_KDE_state_ALL_repr_assessment.csv")
# rat <- read_csv("RAT_Kittiwake_KDE_state_ALL_repr_assessment.csv")
sab <- read_csv("SAB_Kittiwake_KDE_state_ALL_repr_assessment.csv")
sci <- read_csv("SCI_STM_Kittiwake_KDE_state_ALL_repr_assessment.csv")
win <- read_csv("WIN_Kittiwake_KDE_state_ALL_repr_assessment.csv")


head(bem)

### bind into one file
all_rep <- rbind(bar, 
                 bem,
                 coq,
                        csy,
                        fil,
                        fow,
                        iom ,
                        lam ,
                        cop ,
                        mks ,
                        puf ,
                        sab ,
                        sci ,
                        win)

## save megafile

write.csv(all_rep, "all_repr.csv")

## Means
mean(all_rep$out)
## 91.70473
sd(all_rep$out)
## 9.980364


##############################################################
#### repeat for other states


bar <- read_csv("BAR_Kittiwake_KDE_state1_repr_assessment.csv")
bem <- read_csv("BEM_Kittiwake_KDE_state1_repr_assessment.csv")
coq <- read_csv("COQ_Kittiwake_KDE_state1_repr_assessment.csv")
csy <- read_csv("CSY_Kittiwake_KDE_state1_repr_assessment.csv")
# fai <- read_csv("FAI_Kittiwake_KDE_state_ALL_repr_assessment.csv")
fil <- read_csv("FIL_Kittiwake_KDE_state1_repr_assessment.csv")
fow <- read_csv("FOW_Kittiwake_KDE_state1_repr_assessment.csv")
iom <- read_csv("IOM_Kittiwake_KDE_state1_repr_assessment.csv")
lam <- read_csv("LAM_Kittiwake_KDE_state1_repr_assessment.csv")
cop <- read_csv("ORK_COP_Kittiwake_KDE_state1_repr_assessment.csv")
mks <- read_csv("ORK_MKS_Kittiwake_KDE_state1_repr_assessment.csv")
puf <- read_csv("PUF_Kittiwake_KDE_state1_repr_assessment.csv")
# rat <- read_csv("RAT_Kittiwake_KDE_state_ALL_repr_assessment.csv")
sab <- read_csv("SAB_Kittiwake_KDE_state1_repr_assessment.csv")
sci <- read_csv("SCI_STM_Kittiwake_KDE_state1_repr_assessment.csv")
win <- read_csv("WIN_Kittiwake_KDE_state1_repr_assessment.csv")


head(rat)

### bind into one file
state1_rep <- rbind(bem,
                 coq,
                 csy,
                 fil,
                 fow,
                 iom ,
                 lam ,
                 cop ,
                 mks ,
                 puf ,
                 sab ,
                 sci ,
                 win)

## save megafile

write.csv(state1_rep, "state1_repr.csv")

## Means
mean(state1_rep$out)
## 78.90255
sd(state1_rep$out)
## 15.94012

#### STATE 2 #######

bar <- read_csv("BAR_Kittiwake_KDE_state2_repr_assessment.csv")
bem <- read_csv("BEM_Kittiwake_KDE_state2_repr_assessment.csv")
coq <- read_csv("COQ_Kittiwake_KDE_state2_repr_assessment.csv")
csy <- read_csv("CSY_Kittiwake_KDE_state2_repr_assessment.csv")
# fai <- read_csv("FAI_Kittiwake_KDE_state_ALL_repr_assessment.csv")
fil <- read_csv("FIL_Kittiwake_KDE_state2_repr_assessment.csv")
fow <- read_csv("FOW_Kittiwake_KDE_state2_repr_assessment.csv")
iom <- read_csv("IOM_Kittiwake_KDE_state2_repr_assessment.csv")
lam <- read_csv("LAM_Kittiwake_KDE_state2_repr_assessment.csv")
cop <- read_csv("ORK_COP_Kittiwake_KDE_state2_repr_assessment.csv")
mks <- read_csv("ORK_MKS_Kittiwake_KDE_state2_repr_assessment.csv")
puf <- read_csv("PUF_Kittiwake_KDE_state2_repr_assessment.csv")
# rat <- read_csv("RAT_Kittiwake_KDE_state_ALL_repr_assessment.csv")
sab <- read_csv("SAB_Kittiwake_KDE_state2_repr_assessment.csv")
sci <- read_csv("SCI_STM_Kittiwake_KDE_state2_repr_assessment.csv")
win <- read_csv("WIN_Kittiwake_KDE_state2_repr_assessment.csv")


head(bar)

## excluding BAR and SAB

### bind into one file
state2_rep <- rbind(bem,
                    coq,
                    csy,
                    fil,
                    fow,
                    iom ,
                    lam ,
                    cop ,
                    mks ,
                    puf ,
                    sci ,
                    win)

## save megafile

write.csv(state2_rep, "state2_repr.csv")

## Means
mean(state2_rep$out)
## 83.37707
sd(state2_rep$out)
## 25.09249


########### STATE 3 ##############



bar <- read_csv("BAR_Kittiwake_KDE_state3_repr_assessment.csv")
bem <- read_csv("BEM_Kittiwake_KDE_state3_repr_assessment.csv")
coq <- read_csv("COQ_Kittiwake_KDE_state3_repr_assessment.csv")
csy <- read_csv("CSY_Kittiwake_KDE_state3_repr_assessment.csv")
# fai <- read_csv("FAI_Kittiwake_KDE_state_ALL_repr_assessment.csv")
fil <- read_csv("FIL_Kittiwake_KDE_state3_repr_assessment.csv")
fow <- read_csv("FOW_Kittiwake_KDE_state3_repr_assessment.csv")
iom <- read_csv("IOM_Kittiwake_KDE_state3_repr_assessment.csv")
lam <- read_csv("LAM_Kittiwake_KDE_state3_repr_assessment.csv")
cop <- read_csv("ORK_COP_Kittiwake_KDE_state3_repr_assessment.csv")
mks <- read_csv("ORK_MKS_Kittiwake_KDE_state3_repr_assessment.csv")
puf <- read_csv("PUF_Kittiwake_KDE_state3_repr_assessment.csv")
# rat <- read_csv("RAT_Kittiwake_KDE_state_ALL_repr_assessment.csv")
sab <- read_csv("SAB_Kittiwake_KDE_state3_repr_assessment.csv")
sci <- read_csv("SCI_STM_Kittiwake_KDE_state3_repr_assessment.csv")
win <- read_csv("WIN_Kittiwake_KDE_state3_repr_assessment.csv")


head(bar)

## excluding BAR, FOW and LAM

### bind into one file
state3_rep <- rbind(bem,
                    coq,
                    csy,
                    fil,
                    iom ,
                    cop ,
                    mks ,
                    puf ,
                    sab,
                    sci ,
                    win)

## save megafile

write.csv(state3_rep, "state3_repr.csv")

## Means
mean(state3_rep$out)
## 95.27799
sd(state3_rep$out)
## 4.45061