# Prerequired 
#
# > install.packages("data.table")
# > install.packages("reticulate")
# > install.packages("arrow")
# > arrow::install_pyarrow()
#
# restart R studio or R session
old_wd = getwd()
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("../getOrderBook.R")
testDataDir <- "../../../test_data/orderbooks"
df <- getOneDayOrderBook(testDataDir, "2020-02-03")
df2 <- getAllOrderBook(testDataDir)
setwd(old_wd)
