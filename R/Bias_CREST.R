#' Function to load bias corrections as from the tool that generates them into CREST
#' parameters are:
#' datapath = path w/filename to the csv file containing the corrections
#' dbDest = the designation of the destintion database be it "P" for OIOSP01 (prod) or "T" for OIOST01 (test)
#'           or "D" for OISOD01 (dev)

Load_C_Bias <- function (datapath, dbDest) {
  Ocon <- setupOracleConn(dbDest)

  # read in the csv
  auto_notify("Read in CSV")
  cor_vals <- read.csv(datapath)
  # drop any rows with no species
  cor_vals <- cor_vals[!is.na(cor_vals$Species) , ]

  # loop through each row
  auto_notify("Looping through rows")
  cntr <- 0
  bcf_sqlH = "insert into otolith_v1.creel_irec_bias_cor_raw (BC_ID, YEAR, START_DATE, END_DATE, ITEM, DISPOSITION_ID_ID, BCF, SD, BC_DATE, OUTLIERS_REMOVED, R2, SAMPLE_SIZE, SUM_CREEL_IREC)
                    with data1 as ("
 bcf_sqlI <- ""
  for (row in 1:nrow(cor_vals)) {
    cntr <- cntr + 1
    auto_notify(paste0(cntr , " of ", nrow(cor_vals)))
    ss_df <- cor_vals[row, ]
    # build sql insert from the columns expected in the Bias correction file supplied
    bcf_sqlI <- paste0(
      bcf_sqlI ,
      "\n",
      "select '" ,
      ss_df$SPECIES ,
      "' as species, '",
      ss_df$DISPOSITION,
      "' as DISPOSITION, ",
      ss_df$LIC_YEAR,
      ' as LIC_YEAR, ',
      ss_df$BIAS_COR,
      ' as bias_cor,',
      ss_df$SD,
      ' as SD, ',
      ss_df$R2,
      ' as R2, ',
      ss_df$SAMPLE_SIZE,
      ' as sample_size, ',
      round(as.numeric(ss_df$SUM_CREEL), 0),
      '.',
      round(as.numeric(ss_df$SUM_IREC), 0),
      ' as SUM_CREEL_IREC from Dual',
      '\n',
      ' UNION'
    )
  }
  bcf_sqlI <- word(bcf_sqlI, 1, -2)
  bcf_sql = paste0(
    bcf_sqlH,
    "\n",
    bcf_sqlI,
    "\n",
    ")
,cid as (select substr(text,1,3) as text, min(disposition_id) as disposition_id from otolith_v1.creel_irec_disposition group by substr(text,1,3))
, data2 as (select rownum, data1.* from data1)
select (select max(bc_id) as bc_id from otolith_v1.creel_irec_bias_cor_raw)+ rownum as bc_id,  lic_year as year,
to_Date(lic_year || '-04-01', 'YYYY-MM-DD') as start_Date, to_Date(lic_year + 1 || '-03-31', 'YYYY-MM-DD') as end_date, chc.hart_cd as item, cid.disposition_id as disposition_id_id,
bias_cor as bcf, sd, sysdate as bc_date, null as outliers_removed, r2, SAMPLE_SIZE, SUM_CREEL_IREC
from data2 d
left join  cid on substr(d.disposition,1,3) = substr(cid.text,1,3)
left join otolith_v1.creel_hart_Cd chc on replace(d.species, 'Effort', 'Boat Trips') = chc.species_txt
where chc.hart_cd is not null
",
    collapse = "\n"
  )
  output = ""
  tryCatch({
    # send the sql insert
    dbGetQuery(Ocon, bcf_sql)
    dbCommit(Ocon)
  }, error = function(err) {
    auto_notify(err)
    output = err
  })


  dbDisconnect(Ocon)
}

