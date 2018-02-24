
source("./functions/PlotMissing.R")
source("./functions/DetectDup.R")
source("./functions/SetNa.R")


source("./functions/ContinuousBox.R")
source("./functions/ContinuousHisto.R")
source("./functions/ContinuousSummary.R")
source("./functions/ContinuousTests.R")
source("./functions/ContinuousCorrelation.R")
source("./functions/SplitColType.r")
source("./functions/DiscreteBar.R")
source("./functions/DiscreteSummary.R")
source("./functions/DiscreteSummary.R")
source("./functions/DiscreteCorrelation.R")

source("./functions/DiscreteMethod.R")
source("./functions/ContinuousMethod.R")
source("./functions/TablestatMethod.R")

source("./render/render_continuous.R")
source("./render/render_discrete.R")
source("./render/render_tablestat.R")



lib_list <- c("ggplot2","Hmisc","stringr",
              "rmarkdown","data.table","scales","dplyr")

for (i in (seq_along(1:length(lib_list))))
  if (!(lib_list[i] %in% installed.packages())){
    install.packages(lib_list[i],
                     repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
  }

library(ggplot2)
library(data.table)
library(scales)
library(rmarkdown)
library(Hmisc)
library(reshape2)
library(dplyr)

