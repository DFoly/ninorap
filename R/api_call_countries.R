#' @title api_call_tables
#' @description Function accesses the Stat-Xplore API and returns country and UK city data for the latest quarter
#' @param start_date start date of data we want
#' @param end_date what date do we want data until
#' @param frequency One of quarter, month, annual
#' @param apiKey apiKey for stat-xplore: you will need to create an account to generate a key.
#' @param verbose if true, it will return information sent to the Stat-Xplore server.
#' @return class containing data and other attributes useful for plotting
#' @examples \dontrun{eu_non_eu_total("2002-03-01", "2018-12-01", "quarter", 3, TRUE)}
#' @export


api_call_tables <- function(start_date = "2017-03-01", end_date, apiKey,  verbose = FALSE){

  if (apiKey.isnull()) {

    apiKey <- "65794a30655841694f694a4b563151694c434a68624763694f694a49557a49314e694a392e65794a7063334d694f694a7a644849756333526c6247786863694973496e4e3159694936496d5268626d35355a6a4532514768766447316861577775593239744969776961574630496a6f784e54517a4f5445354f4455304c434a68645751694f694a7a6448497562325268496e302e70496474346b5763677546564d42486b6773484f306a6d5f536d556b6a33586e574946527041516f794f6f"
  }

  # checks
  assertthat::is.string(start_date)
  assertthat::is.string(end_date)
  assertthat::is.string(frequency)
  assertthat::assert_that(frequency %in% c("month", "quarter", "day", "year"))
  assertthat::is.string(apiKey)

  out <- tryCatch(
    expr = {

      date_list <- generate_date_sequence(start_date, end_date, "quarter")
      dates <- date_list[[1]]
      length_dates <- date_list[[2]]


      # Generate JSON date sequence
      num_quarters = seq(from=1, to=length_dates, by=1)
      quarters <- lapply(num_quarters, function(x) paste('["str:value:NINO:f_NINO:QTR:c_QTR:',x,'"',']' ,sep=""))
      quarters_final <- paste(quarters, collapse=",")

      ######################
      # Generate api call
      ######################

      # Issue Rstudio crahes when trying to paste this with quarter seletion so this is hardcoded right now
      body <- '{"database":"str:database:NINO","measures":["str:count:NINO:f_NINO"],"recodes":{"str:field:NINO:f_NINO:NEWNAT":{"map":[["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:101"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:102"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:103"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:104"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:105"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:106"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:107"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:108"],
      ["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:109"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:110"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:111"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:112"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:113"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:114"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:201"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:202"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:203"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:204"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:205"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:206"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:207"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:208"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:211"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:212"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:209"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:210"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:213"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:301"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:302"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:303"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:304"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:305"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:306"],
      ["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:307"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:308"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:309"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:310"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:311"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:312"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:314"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:315"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:316"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:317"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:318"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:319"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:320"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:321"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:324"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:603"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:708"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:501"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:502"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:503"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:504"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:515"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:516"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:517"],
      ["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:519"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:522"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:524"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:531"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:534"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:535"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:536"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:537"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:538"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:539"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:540"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:543"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:547"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:509"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:510"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:511"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:518"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:520"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:521"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:528"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:544"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:505"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:506"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:513"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:530"],
      ["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:532"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:542"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:548"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:551"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:507"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:508"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:512"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:514"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:523"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:525"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:526"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:527"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:529"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:533"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:541"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:545"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:546"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:402"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:403"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:404"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:405"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:406"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:407"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:408"],
      ["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:409"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:410"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:411"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:412"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:413"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:414"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:415"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:416"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:418"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:419"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:420"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:421"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:422"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:423"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:424"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:425"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:426"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:427"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:429"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:430"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:431"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:433"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:434"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:436"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:437"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:438"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:439"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:440"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:441"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:442"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:443"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:444"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:445"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:446"],
      ["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:447"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:449"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:450"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:451"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:453"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:455"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:456"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:457"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:401"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:417"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:428"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:432"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:435"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:448"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:452"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:454"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:601"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:602"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:639"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:652"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:604"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:605"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:606"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:607"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:608"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:609"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:610"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:611"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:612"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:613"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:614"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:615"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:616"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:617"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:618"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:619"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:620"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:621"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:622"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:623"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:624"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:625"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:626"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:627"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:628"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:629"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:630"],
      ["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:631"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:632"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:633"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:634"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:635"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:636"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:637"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:638"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:640"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:641"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:642"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:643"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:644"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:645"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:646"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:647"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:648"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:650"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:651"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:653"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:654"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:655"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:549"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:550"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:649"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:701"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:702"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:703"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:704"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:705"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:706"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:707"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:709"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:710"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:711"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:712"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:713"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:714"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:715"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:716"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:717"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:718"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:719"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:720"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:721"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:722"],
      ["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:723"],["str:value:NINO:f_NINO:NEWNAT:C_NINO_COUNTRY:999"]],"total":true},"str:field:NINO:f_NINO:QTR":{"map":[["str:value:NINO:f_NINO:QTR:c_QTR:61"],["str:value:NINO:f_NINO:QTR:c_QTR:62"],["str:value:NINO:f_NINO:QTR:c_QTR:63"],["str:value:NINO:f_NINO:QTR:c_QTR:64"],["str:value:NINO:f_NINO:QTR:c_QTR:65"],["str:value:NINO:f_NINO:QTR:c_QTR:66"],["str:value:NINO:f_NINO:QTR:c_QTR:67"],["str:value:NINO:f_NINO:QTR:c_QTR:68"]],"total":true}},"dimensions":[["str:field:NINO:f_NINO:QTR"],["str:field:NINO:f_NINO:NEWNAT"]]}'

      if (verbose==TRUE) {
        request <- httr::POST("https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table", httr::add_headers(apiKey = apiKey),
                              body = body,
                              encode = "json", httr::verbose())
      }
      request <- httr::POST("https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table", httr::add_headers(apiKey = apiKey),
                            body = body,
                            encode = "json")

      get2json<- httr::content(request, as = "parsed")
      parse2json<- jsonlite::toJSON(get2json, pretty=TRUE) # returns raw data

      parse_df <- as.data.frame(t(sapply(parse2json, jsonlite::fromJSON)))

      title <- parse_df$database[[1]]$label

      # Extract dates
      data_values <- parse_df$cubes[[1]][[1]][[1]]
      date_labels <- unlist(parse_df$fields[[1]][[4]][[1]]$labels)
      nrows = dim(data_values)[1]
      ncols =  dim(data_values)[2]
      date_labels <- date_labels[1:nrows-1]

      # destination labels
      origin <- unlist(parse_df$fields[[1]][[4]][[2]]$labels)

      # create data frame
      df <- as.data.frame(data_values)

      colnames(df) <-  gsub(" ", "_", origin)
      colnames(df) <-  gsub("-", "_", colnames(df))


      x = structure(
        list(
          df = df,
          #df_year_to_date_sum =
          title = title,
          colnames = colnames(df),
          dates = dates,
          date_labels = date_labels
        ),
        class = "eu_non_eu_data")

      message("API call Successful!")


      return (x)
      ##
    },
    warning = function() {

      w <- warnings()
      warning('Warning produced running in eu_non_eu_total():', w)

    },
    error = function(e)  {

      stop('Error produced running eu_non_eu_total():', e)

    },
    finally = {}
  )

}
