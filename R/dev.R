# ---- did_imputation ----------------------------------------------------------
#

library(fixest)
library(data.table)

data(df_hom, package = "didimputation")
data = df_hom
data$y2 = data$dep_var + rnorm(nrow(data), 0, 1)
yname = "c(dep_var, y2)"
# yname = "dep_var"
idname = "unit"; gname = "g"; tname = "year"
first_stage = ~ error | unit + year + state^year
horizon = T; pretrends = TRUE
cluster_var = NULL; wtr = NULL; wname = NULL

data = df_hom; yname = "dep_var"; gname = "g"
tname = "year"; idname = "unit"
first_stage = NULL; wname = NULL; wtr = NULL; horizon = NULL
pretrends = NULL; cluster_var = NULL

# did_imputation(
# 	data = data, yname = "c(dep_var, y2)", 
#   gname = gname, tname = tname, idname = idname,
#   first_stage =  first_stage, horizon = horizon, pretrends = TRUE
# )
# did_imputation(
# 	data = data, yname = "c(dep_var, y2)", 
#   gname = gname, tname = tname, idname = idname,
#   first_stage =  first_stage, horizon = horizon, pretrends = FALSE
# )
# did_imputation(
# 	data = data, yname = "c(dep_var, y2)", 
#   gname = gname, tname = tname, idname = idname,
#   first_stage =  first_stage, horizon = horizon, pretrends = -5:-2
# )
# did_imputation(
# 	data = data, yname = "c(dep_var, y2)", 
#   gname = gname, tname = tname, idname = idname,
#   first_stage =  first_stage, horizon = 0:5, pretrends = -5:-2
# )

# did_imputation(
# 	data = data, yname = "dep_var", 
#   gname = gname, tname = tname, idname = idname,
#   first_stage =  first_stage, horizon = horizon, pretrends = TRUE
# )
# did_imputation(
# 	data = data, yname = "dep_var", 
#   gname = gname, tname = tname, idname = idname,
#   first_stage =  first_stage, horizon = horizon, pretrends = FALSE
# )
# did_imputation(
# 	data = data, yname = "dep_var", 
#   gname = gname, tname = tname, idname = idname,
#   first_stage =  first_stage, horizon = horizon, pretrends = -5:-2
# )
# did_imputation(
# 	data = data, yname = "dep_var", 
#   gname = gname, tname = tname, idname = idname,
#   first_stage =  first_stage, horizon = 0:5, pretrends = -5:-2
# )

# data(df_hom, package = "didimputation")
# data = df_hom
# yname = "dep_var"; idname = "unit"; gname = "g"; tname = "year"
# first_stage = ~ error | unit + year; 
# horizon = T; pretrends = TRUE; 
# cluster_var = NULL; wtr = NULL; wname = NULL;
# 
# # static
# did_imputation(
# 	data=data, yname=yname, gname=gname, tname=tname, idname=idname
# )
#
# # all horizon
# did_imputation(
# 	data=data, yname=yname, gname=gname, tname=tname, idname=idname,
# 	horizon=TRUE
# )
#
# # all horizon and allpretrends
# did_imputation(
# 	data=data, yname=yname, gname=gname, tname=tname, idname=idname,
# 	horizon=TRUE, pretrends=TRUE
# )
#
# # select subsets of horizons/pretrends
# did_imputation(
# 	data=data, yname=yname, gname=gname, tname=tname, idname=idname,
# 	horizon=0:5, pretrends=-5:-1
# )
#
# # weighted
# did_imputation(
# 	data=data, yname=yname, gname=gname, tname=tname, idname=idname,
# 	horizon=TRUE, weights="weight"
# )
