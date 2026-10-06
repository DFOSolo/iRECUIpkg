
#' Run iRec User Interface
#'
#' Start a shiny server with the user interface for the iRecUI package
#'
#' @param port Port number to run shiny server on
#'
#'
#' @import shiny
#' @importFrom utils packageName
#'
#' @export
run_iRECUI <- function(port = 9997) {

  print((.packages()))
     appDir <- system.file("iRECUI", package = "iRECUIpkg")
     if (appDir == "") {
       stop(
         glue(
           "Could not find example directory. Try re-installing `{packageName()}`."
         ),
         call. = FALSE
       )
     }

 cy <- as.numeric(format(Sys.Date(), "%Y"))
 cy4 <- cy-4
 last5yrs <<- as.list(cy4:cy)
 SQLOutput <<- ''

 month_nms <<- c("January", "February","March","April","May", "June", "July",
                "August","September","October", "November", "December")
 month_nms_shrt <<- c("Jan", "Feb","Mar","Apr","May", "Jun", "Jul",
                     "Aug","Sept","Oct", "Nov", "Dec")


 # this is the framework for storing usernames/passwords - if actually implemented it would need to be better than this.
 user_base <<- tibble::tibble(
   user = c("user1", "user2"),
   password = c("pass1", "pass2"),
   permissions = c("admin", "standard"),
   name = c("User One", "User Two")
 )
 #browser()

 tryCatch({
   lapply(DBI::dbListConnections(ROracle::Oracle()), DBI::dbDisconnect)
 },

 error = function(e) {
   # Error Handler
   message(paste("DBI failed:", e$message))
   rws <- 0
 },

 warning = function(w) {
   # Warning Handler
   message(paste("DBI write:", w$message))
   invokeRestart("muffleWarning")
 },

 finally = {
   # Cleanup / Log action
   message("DBI finished.")
 })
 #check to see if oracle credentials exist
 if (file.exists(paste0(path.expand('~'),"/OracleCreds/credsL.txt"))) {
   oracredsL <<- TRUE
   Lcon <<- setupOracleConn("L")
   if (is.null(Lcon)) {
     oracredsL <<- FALSE
   }
 } else {
   oracredsL <<- FALSE
 }

 if (file.exists(paste0(path.expand('~'),"/OracleCreds/credsP.txt"))) {
   oracredsP <<- TRUE
   Ccon <<- setupOracleConn("P")
   if (is.null(Ccon)) {
     oracredsP <<- FALSE
   }
 } else {
   oracredsP <<- FALSE
 }
 if (file.exists(paste0(path.expand('~'),"/OracleCreds/credsT.txt"))) {
   oracredsT <<- TRUE
   Ccon <<- setupOracleConn("T")
   if (is.null(Ccon)) {
     oracredsT <<- FALSE
   }
 } else {
   oracredsT <<- FALSE
 }

 # By sourcing this file, reused dataframes are loaded from CSV files
 if (oracredsL) {

   areasql <- 'with cla as (select cla.lrg_area_id, cla.lrg_area_nme, cla.description, length(cla.lrg_Area_nme) as chrs from
otolith_v1.creel_crest_lrg_Area cla
where cla.lrg_Area_id in (select lrg_Area_id_id from otolith_v1.creel_irec_area)
)
, splits as (select cla.lrg_Area_id as lrg_Area_id, cla.lrg_Area_nme, cla.chrs,
claP.lrg_Area_id as P_id, claP.lrg_Area_nme as P_nme, clap.chrs as p_chrs
from cla
left join cla claP on cla.description = claP.description and cla.chrs > claP.chrs
where 1=1
and claP.chrs is not null
)
, singles as (select lrg_Area_id, lrg_Area_nme, chrs, lrg_Area_nme as p_nme, chrs as p_chrs
from cla
where lrg_Area_id not in (select lrg_Area_id from splits)
)
, clean_splits as (select lrg_area_id, lrg_Area_nme, chrs, min(p_nme) as p_nme, min(p_chrs) as p_chrs
from splits
group by lrg_area_id, lrg_Area_nme, chrs
)
, d as (select * from singles
union
select * from clean_splits
)
select d.lrg_Area_id as "lrg_Area_id_id", d.lrg_area_nme as "type_name", d.p_nme as "parent_name", cla.lrg_Area_id as "parent_id" from  d
left join cla on d.p_nme = cla.lrg_Area_nme
order by cast(REGEXP_REPLACE(d.lrg_Area_nme, \'[^0-9]\') as number)'
Qareas_df <<- getdata(Lcon, areasql)
names(Qareas_df) <- tolower(names(Qareas_df))

sppqry <- "select i.text,i.column_name, d.text as Disposition, m.alttext as modifier, g.text as grouping, chc.species_txt as Common_name,
rt.display_name, chc.hart_Cd as species, r.text as lengthclass
from otolith_V1.creel_irec_item i
left join otolith_v1.creel_irec_disposition d on i.disposition_id_id = d.disposition_id
left join otolith_v1.creel_irec_modifier m on i.modifier_id_id = m.modifier_id
left join otolith_V1.creel_irec_grouping g on i.grouping_id_id = g.grouping_id
left join otolith_V1.creel_hart_Cd chc on i.hart_Cd_id_id = chc.hart_Cd
left join otolith_V1.refdata_resource_types rt on chc.rsty_id_id = rt.rsty_id
left join otolith_V1.creel_irec_retainability r on i.retainability_id_id = r.retainability_id"
df_spp <<- getdata(Lcon, sppqry)

 } else {
   data_path <- system.file("extdata", "areasDFO.csv", package = "iRECUIpkg")
   areas_dfO <- read.csv(data_path)
   areas_dfO <- dplyr::select_all(areas_dfO, tolower)
   areas_dfU <- unique(areas_dfO[,c("std_ref_type_cde","lrg_area_id_id")])
   area_cde <- unique(areas_dfO[,c("std_ref_type_cde","type_name")])
   Qareas_df <<- dplyr::left_join(areas_dfU,area_cde, by=c("std_ref_type_cde" = "std_ref_type_cde"))

   rm(areas_dfO)
   rm(areas_dfU)
   rm(area_cde)
   data_path <- system.file("extdata", "CRESTItemLookup.csv", package = "iRECUIpkg")
   df_spp <<- read.csv(data_path)
 }

 results_list <- list()
 for (i in lubridate::year(lubridate::now()):2012){
   iP <- as.numeric(substr(i,3,4)) + 1
   results_list[[i]] <- data.frame( LICENCE_YEAR = paste0(i, '-', iP))
 }
  ly_df <<- do.call(rbind, results_list)
  ly_df <<- dplyr::arrange(ly_df,desc(LICENCE_YEAR))

# shiny::runApp(appDir, display.mode = "normal", port = port)
 shiny::runApp(appDir, display.mode = "normal")
}

#run_iRECUI(9997, 2)
