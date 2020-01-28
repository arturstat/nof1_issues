# Written by Artur Araujo

# artur.stat@gmail.com
# aamarinhodearaujo1@sheffield.ac.uk

# October-November 2019

# These data were kindly supplied by Dr. Michael Yelland.
# Yelland MJ, Poulos CJ, Pillans PI, et al.
# N-of-1 randomized trials to assess the efficacy of gabapentin for chronic neuropathic pain.
# Pain medicine 2009; 10: 754-761. DOI: 10.1111/j.1526-4637.2009.00615.x.

# R program H.1

gaba_placebo_raw <- data.frame(
  "id"=factor( c(751, 752, 768, 772, 773, 804, 805,806, 807, 808, 809, 811, 812, 813, 828,
              829, 831, 832, 833, 850, 851, 852, 853, 862, 863, 864, 865, 866, 867,869,
              870, 898, 900, 922, 923, 924, 925, 926, 927, 928,929, 931, 932, 934, 935, 936) ),
  "rfs"=factor( c(1, 1, 0, 1,1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,1, 1, 1, 1, 1, 1, 1, 1, 1, 1) ),
  "pain_p1"=c(3.29, 6.14, 1.43, 4.57, 8, 0, 4.57, 7.2, 3.43, 1, 4.33, 2.57, 6.43, 5.43, 2.71,
              3.71, 6.83, 10, 6.57, 6.29, 7.86, 4.33, 6, 4, 5.71, 6.43, 5.83, 6.86, 3.14, 3.71,
              3.29, 4.57, 7.14, 7.29, 5.57, 4.14, 2.57, 7.71, 0.29, 0.86, 6.14, 2.5, 4, 5.57, 1.71, 6.43),
  "pain_p2"=c(3.14, NA, 3.5, 2.71, 6, 3, 5.14, 4.14, 4, 2.67, 4.43, 5, 3.86, 5.5, 1.43,
              3.29, 8.43, 8.71, 8.17, 3.86, 6.57, 2, 6.14, 3, 5.71, 5.86, 7.29, 5.57, 4.14, 4,
              3.57, 1.43, 6.29, 8, 4.57, 3.5, 1, 6.4, 1.43, 1.57, 1.14, 5.14, 1.33, 5.86, 0.6, NA),
  "pain_p3"=c(NA, NA, 2.57, 4, 6, 2.86, 6, 3.57, 5.29, 4.57, 2, 3.14, 2.57, 5.57, 3,
              3.14, 8.57, 7.14, 7.86, 2.29, 6.86, 2, 6, 3.29, 7, NA, 7.57, 2.14, 4, 2,
              4.57, 5, 6.43, 7.57, NA, 6.6, 7, 9.14, 2, 1.29, 3.43, 6, 5.29, 5.29, 3.6, NA),
  "pain_g1"=c(0, 3.86, 3.33, 4.14, 8, 2.71, 3.57, 6, 1, 2.14, 2.86, 3, 5.29, 5.86, 2,
              1.57, 5.57, 8.29, 3.57, 3.71, 7.29, 4, 6.57, 4.67, 6, 6.17, 5.57, 6.71, 5.57, 1.14,
              3.29, 2.86, 5.57, 4.57, 3.43, 5, 2.43, 8.14, 0.71, 1, 1.33, 7.29, 0.71, 5.29, 2.57, 6),
  "pain_g2"=c(NA, NA, 0, 4.86, 6, 2.57, 3.71, 8.57, 1.5, 2.57, 1, 2.14, 3.86, 5.57, 2,
              2, 4.14, 7, 1.29, 2.2, 6.67, 2, 5, 4.57, 6, 5.71, 6.33, 3.71, 3.71, 1.29,
              4.14, 8, 6, 6.57, 4.43, 2.43, 3.43, 5.71, 1.43, 0.17, 6.17, 1, 2, 4.57, 2.14, NA),
  "pain_g3"=c(NA, NA, 3.43, 2.71, 6, 7.57, 3.14, 7.71, 3.14, 7.17, 0.71, 2.43, 2, 5.57, 2.71,
              3.57, 4.86, 6.57, 1.29, 0.29, 7.57, 2, 5, 5.29, 7, NA, 3.43, 7, 4.14, 2.14,
              4.86, 3.71, 7.14, 3.43, NA, 4.86, 1.71, 8.43, 1.43, 0.17, 2.29, 4.43, 0, 4.29, 0.2, NA),
  "sleep_p1"=c(0.14, 9, 2.86, 3.57, 5.43, 0.86, 4.14, 6.6, 3.43, 0, 0.2, 0, 6.29, 4.86, 0,
               5.71, 5.5, 8.5, 8, 0, 7.43, 5, 5.5, 2.43, 9, 5.86, 4.33, 7, 1.43, 3.86,
               3.14, 5.71, 7, 0, 5.14, 4.14, 1.14, 3.29, 0.43, 0, 3, 6.5, 0, 3.57, 0.29, 8.43),
  "sleep_p2"=c(3.14, NA, 2.86, 1.71, 6, 3.43, 5.86, 4, 4.86, 1.8, 1.2, 2.67, 4, 4.4, 0,
               5.29, 7.71, 5.29, 9.33, 1.71, 6.14, 0, 5, 2.86, 9, 5.57, 5.83, 5.43, 1.71, 3.86,
               1.14, 2.14, 6.43, 0, 2.14, 3, 0.86, 1.4, 0, 0, 0.14, 6.4, 0, 2.71, 0.2, NA),
  "sleep_p3"=c(NA, NA, 1, 3, 6, 2.86, 6.29, 2.86, 5.14, 2.71, 1.83, 0.71, 2.57, 4.29, 0,
               4.71, 7.86, 4.57, 9.29, 0.29, 5.67, 0, 6, 2, 9, NA, 6, 1.14, 1.14, 1.57,
               2, 5.57, 6.57, 0, NA, 4.8, 4.71, 5, 0.71, 0, 0.57, 4.25, 0, 2.86, 0.86, NA),
  "sleep_g1"=c(0, 9, 3.14, 2.43, 6.29, 1.14, 3.71, 5.17, 1.86, 2, 1, 0, 4.29, 4.86, 0,
               3, 3.71, 4.86, 4.57, 0, 6.5, NA, 6.14, 2.33, 9, 5.33, 3.71, 6.14, 2.57, 1.43,
               7, 3.86, 6, 0, 2.86, 4, 1.29, 5.71, 0, 0, 0, 6.71, 0, 2.43, 1.14, 9),
  "sleep_g2"=c(NA, NA, 2.29, 2.86, 6, 2.43, 3.57, 8.14, 2, 1.17, 0.14, 0.71, 4, 4.29, 0,
               3.86, 3.43, 4.86, 1.57, 0, 4.83, 0, 4.29, 3.71, 9, 5.57, 4.29, 3.14, 1.29, 0.86,
               1.86, 9, 5.86, 0, 2.43, 2.71, 2.14, 2.57, 0.43, 0, 1.5, 7, 0, 1.71, 1.14, NA),
  "sleep_g3"=c(NA, NA, 4.71, 2, 6, 7.71, 2.86, 7.43, 2.14, 5.67, 0.29, 0, 2, 4.67, 0,
               4.71, 4.14, 4.29, 1.29, 0.29, 5.71, 0, 4, 4.14, 9.29, NA, 2, 6.29, 1.71, 1.86,
               2, 4.71, 6.57, 0, NA, 3.43, 0.86, 3.57, 0.71, 0, 0, 6.29, 0, 2, 0, NA),
  "func_p1"=c(3.4, 8.2, 5, 9.5, 8, 4, 5, 5, 3, 0.67, 3.5, 3.5, 4.5, 5.67, 4.6,
              2.5, 6.67, NA, 7, 8.2, 8, 8, 6.2, NA, 7.6, 5.4, 3.4, 8.6, 5.6, 6.2,
              3.4, 6.4, 7.6, 7.2, 2.6, 4.2, 3, 7, 2.2, 1.6, 6.4, NA, 3, 5.8, 1.33, 9.2),
  "func_p2"=c(9.4, NA, 0, 8.67, 7, 5.67, 6, 4, 3.75, 2, 2.67, 4, NA, 4.67, 3.2,
              3.25, 8, 6.67, 7.67, 7, 7.8, 6.2, 6.2, 5, 8, 4.4, 5, 6, 6.4, 5.6,
              4, 6.6, 7.2, 8, 7.4, 6.2, 2.2, 8.6, 4.4, 0.8, 4, 7.6, 2, 5.2, 3, NA),
  "func_p3"=c(NA, NA, 0, NA, NA, NA, 5.67, 3, 6.33, 7, 10, 3.5, 4, NA, 4,
              3.75, 8, 4, 6.33, 1.6, 7.8, 3.4, 6.2, 5.75, 7.8, NA, 5.4, 2.4, 6.6, 4.8,
              4.2, 7, 7.6, 8, NA, NA, 7.4, 7, 2.6, 0.2, 4, 4.8, 8, 5.6, 1.75, NA),
  "func_g1"=c(0, 6.6, 0, 9, 8, 3.33, 4.66, 6.5, 1.67, 2, 2.67, 3.5, 3.5, 5.33, 3.4,
              7.75, 3.67, 5.33, 2.33, 0, 7.6, 1, 7.6, 9.25, 7.8, 4.8, 3.2, 6.8, 6.4, 1.6,
              5, 5.2, 4.6, 5, 4, 5.8, 3.2, 7.6, 2.2, 1.8, 6, 10, 3.4, 6.6, 0, 10),
  "func_g2"=c(NA, NA, 0, 8.5, 7, NA, 4, 8.5, 3, 6, 1.33, 3.75, 3.5, 4.33, 2.5,
              3.5, 3.67, 5.33, 1, 0.8, 7.2, 5, 6.4, 5, 8, 5.2, 6, 3.6, 7, 2.4,
              5, 8.6, 6, 6.4, 8.4, 5.2, 3.4, 6.4, 4.2, 0, 5.6, NA, 4, 3.8, 6.33, NA),
  "func_g3"=c(NA, NA, 0, 8, 6, NA, 4, 8.5, 3.33, 7, 1, 2.5, 2, 5, 4.6,
              3.75, 4, 4, 4, 1, 7.4, 2, 5.8, 6.5, 7.8, NA, 3.8, 4.8, 6.4, 5,
              4, 7.4, 7.6, 3.6, NA, 5.2, 2, 8.4, 4, 0, 3.8, NA, 1.5, 4.4, 1.4, NA)
);

# R program H.2

# Convert dataset with one observation per subject
# to a dataset with several observations per subject
SubjectNumber <- nlevels(gaba_placebo_raw$id);
CycleNumber <- 3;
treatA <- "Placebo";
treatB <- "Gabapentin";

gaba_placebo <- data.frame(
  "id"=factor(
    x=character(length=2*CycleNumber*SubjectNumber),
    levels=levels(gaba_placebo_raw$id)
  ),
  "rfs"=factor(
    x=character(length=2*CycleNumber*SubjectNumber),
    levels=levels(gaba_placebo_raw$rfs)
  ),
  "Treatment"=factor(
    x=character(length=2*CycleNumber*SubjectNumber),
    levels=c(treatA, treatB)
  ),
  "Cycle"=factor(
    x=character(length=2*CycleNumber*SubjectNumber),
    levels=1:CycleNumber
  ),
  "Period"=numeric(length=2*CycleNumber*SubjectNumber),
  "Pain"=numeric(length=2*CycleNumber*SubjectNumber),
  "Sleep"=numeric(length=2*CycleNumber*SubjectNumber),
  "Function"=numeric(length=2*CycleNumber*SubjectNumber)
);

gaba_placebo <- within(
  data=gaba_placebo,
  expr={
    index <- 1
    for (i in 1:SubjectNumber) {
      for (j in 1:CycleNumber) {
        id[index:(index+1)] <- gaba_placebo_raw$id[i];
        rfs[index:(index+1)] <- gaba_placebo_raw$rfs[i];
        Treatment[index:(index+1)] <- c(treatB, treatA);
        Cycle[index:(index+1)] <- j;
        Period[index:(index+1)] <- 2*(j-1)+c(1, 2);
        Pain[index] <- gaba_placebo_raw[i, paste0("pain_g", j)];
        Pain[index+1] <- gaba_placebo_raw[i, paste0("pain_p", j)];
        Sleep[index] <- gaba_placebo_raw[i, paste0("sleep_g", j)];
        Sleep[index+1] <- gaba_placebo_raw[i, paste0("sleep_p", j)];
        Function[index] <- gaba_placebo_raw[i, paste0("func_g", j)];
        Function[index+1] <- gaba_placebo_raw[i, paste0("func_p", j)];
        index <- index+2;
      }
    }
    rm(index, i, j);
  }
);
rm(SubjectNumber, CycleNumber);

# R program H.3

# Simulate random order of treatment administration
set.seed(314159);
gaba_placebo <- within(
  data=gaba_placebo,
  expr={
    for ( subj in levels(id) ) {
      for ( c in levels(Cycle) )  {
        index <- which(id==subj & Cycle==c);
        Period[index] <- sample(
          x=Period[index],
          size=2,
          prob=c(0.5, 0.5)
        );
      }
    }
    rm(subj, c, index);
  }
);

# order dataset by id, Cycle and Period
o <- with( data=gaba_placebo, expr=order(id, Cycle, Period) );
gaba_placebo <- gaba_placebo[o,];
rm(o);

# R program H.4

# Create new dataset by differencing the outcome
# variable under treatB and under treatA
# while taking old dataset as input
nCycle <- nrow( unique(gaba_placebo[,c("id", "Cycle")]) );

gaba_placebo_diff <- eval(
  expr=parse(
    text=paste0('
      data.frame(
        "id"=factor(
          x=character(length=nCycle),
          levels=levels(gaba_placebo$id)
        ),
        "rfs"=factor(
          x=character(length=nCycle),
          levels=levels(gaba_placebo$rfs)
        ),
        "Cycle"=factor(
          x=character(length=nCycle),
          levels=levels(gaba_placebo$Cycle)
        ),
        "Pain_', treatA, '"=numeric(nCycle),
        "Sleep_', treatA, '"=numeric(nCycle),
        "Function_', treatA, '"=numeric(nCycle),
        "Pain_', treatB, '"=numeric(nCycle),
        "Sleep_', treatB, '"=numeric(nCycle),
        "Function_', treatB, '"=numeric(nCycle),
        "deltaPeriod"=numeric(nCycle),
        "deltaPain"=numeric(nCycle),
        "deltaSleep"=numeric(nCycle),
        "deltaFunction"=numeric(nCycle)
      )'
    )
  )
);

gaba_placebo_diff <- with(
  data=gaba_placebo,
  expr={
    index <- 1;
    for ( subj in levels(id) ) { # subjects loop
      for ( c in levels(Cycle) ) { # cycles loop
        indexA <- which(
          id==subj & Cycle==c
          & Treatment==levels(Treatment)[1]
        );
        indexB <- which(
          id==subj & Cycle==c
          & Treatment==levels(Treatment)[2]
        );
        if (length(indexB)==0 & length(indexA)==0) next; # unbalanced data
        if (length(indexB)==0) {
          warning(
            "An observation under subject ",
            subj,
            ", cycle ",
            c,
            " and treatment ",
            levels(Treatment)[2],
            " is not available to define a point!\nIgnoring."
          );
          next; # next loop
        }
        else if (length(indexA)==0) {
          warning(
            "An observation under subject ",
            subj,
            ", cycle ",
            c,
            " and treatment ",
            levels(Treatment)[1],
            " is not available to define a point!\nIgnoring."
          );
          next; # next loop
        }
        gaba_placebo_diff$id[index] <- subj;
        gaba_placebo_diff$rfs[index] <- rfs[indexA];
        gaba_placebo_diff$Cycle[index] <- c;
        eval( parse( text=paste0('gaba_placebo_diff$Pain_',
          treatA, '[index] <- Pain[indexA]') ) );
        eval( parse( text=paste0('gaba_placebo_diff$Pain_',
          treatB, '[index] <- Pain[indexB]') ) );
        eval( parse( text=paste0('gaba_placebo_diff$Sleep_',
          treatA, '[index] <- Sleep[indexA]') ) );
        eval( parse( text=paste0('gaba_placebo_diff$Sleep_',
          treatB, '[index] <- Sleep[indexB]') ) );
        eval( parse( text=paste0('gaba_placebo_diff$Function_',
          treatA, '[index] <- Function[indexA]') ) );
        eval( parse( text=paste0('gaba_placebo_diff$Function_',
          treatB, '[index] <- Function[indexB]') ) );
        gaba_placebo_diff$deltaPeriod[index] <-
          Period[indexB]-Period[indexA];
        gaba_placebo_diff$deltaPain[index] <-
          Pain[indexB]-Pain[indexA];
        gaba_placebo_diff$deltaSleep[index] <-
          Sleep[indexB]-Sleep[indexA];
        gaba_placebo_diff$deltaFunction[index] <-
          Function[indexB]-Function[indexA];
        index <- index+1; # increase index for next loop
      }
    }
    gaba_placebo_diff; # data.frame must be evaluated last
  }
);
rm(nCycle);
rm(treatA, treatB);

# R program H.5

# remove missing observations
gaba_placebo_miss <- gaba_placebo[
  !is.na(gaba_placebo$Pain),
  !( names(gaba_placebo) %in% c("rfs", "Sleep", "Function") )
];

# remove missing observations
gaba_placebo_diff_miss <- gaba_placebo_diff[
  !is.na(gaba_placebo_diff$deltaPain),
  c("id", "Cycle", "deltaPeriod", "deltaPain")
];
