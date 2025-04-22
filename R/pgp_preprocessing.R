# Clean up HAPS data for PrestoGP fit
pgp_preprocessing = function(data) {
  df = as.data.frame(data) # Convert to dataframe

  # Preparing MDL values for PrestoGP fit
  df <- df %>%
    mutate(
      CONC_DAILY_STD = if_else(
        CONC_DAILY_STD <= 0,
        MDL_DAILY_STD_UG_M3 / 20,
        CONC_DAILY_STD
      )
    ) %>%
    filter(CONC_DAILY_STD < 300)

  # Change NA's or <=0s in MDL
  df <- df %>%
    group_by(AQS_PARAMETER_CODE) %>%
    mutate(
      MDL_DAILY_STD_UG_M3 = if_else(
        MDL_DAILY_STD_UG_M3 <= 0 | is.na(MDL_DAILY_STD_UG_M3),
        min(CONC_DAILY_STD[CONC_DAILY_STD > 0], na.rm = TRUE) / 10,
        MDL_DAILY_STD_UG_M3
      )
    ) %>%
    ungroup()

  # Select only one instance in case of repeat site-times with different duration_desc
  df = df %>%
    group_by(AQS_PARAMETER_CODE, AMA_SITE_CODE, time) %>% # group by pollutant/site/year/duration (across days)
    filter(DURATION_DESC == min(DURATION_DESC)) %>%
    ungroup()

  return(df)
}
