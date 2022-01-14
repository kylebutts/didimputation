
# ---- did_imputation ----------------------------------------------------------
#
# library(tidyverse)
#
# data(df_hom, package="did2s")
# data = df_hom
# yname = "dep_var"; idname = "unit"; gname = "g"; tname = "year"
# first_stage = NULL; horizon=T; pretrends=NULL;
# wtr = NULL; weights = NULL;

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
