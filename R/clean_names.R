#' Create standard compliant variable names.
#'
#' @param data A dataframe with non-standard names.
#' @param factor_to_string Convert factors to string.
#' @param sep_cols Seperate colums that contain both code and name into two variables.
#'     Only for downloaded SCB-data.
#' @return A dataframe \code{df} with standard names.
#' @examples
#' df <- data.frame(ärVar = c(1:3), örÅr = c(4:6), region = c("0 Riket", "12 Skåne", "1280 Lund"))
#' clean_names_swe(df)
#' @export
clean_names_swe <-
  function(data = df,
           factor_to_string = TRUE,
           sep_cols = FALSE) {

    fsep_cols <- function(df) {
      # Region

      if (any(names(df) == "region")) {
        df <- tidyr::separate(
          df,
          region,
          c("region_kod", "region_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$region <- NULL
      }

      # Yrke

      if (any(names(df) == "occupation_ssyk_2012")) {
        df <- tidyr::separate(
          df,
          occupation_ssyk_2012,
          c("yrke_kod", "yrke_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$occupation_ssyk_2012 <- NULL
      }

      if (any(names(df) == "yrke_ssyk_2012")) {
        df <- tidyr::separate(
          df,
          yrke_ssyk_2012,
          c("yrke_kod", "yrke_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$yrke_ssyk_2012 <- NULL
      }

      if (any(names(df) == "yrke")) {
        df <- tidyr::separate(
          df,
          yrke,
          c("yrke_kod", "yrke_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$yrke <- NULL
      }

      if (any(names(df) == "occupation")) {
        df <- tidyr::separate(
          df,
          occupation,
          c("yrke_kod", "yrke_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$occupation <- NULL
      }

      # SNI

      if (any(names(df) == "naeringsgren_sni92")) {
        df <- tidyr::separate(
          df,
          naeringsgren_sni92,
          c("bransch_kod", "bransch_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$naeringsgren_sni92 <- NULL
      }


      if (any(names(df) == "naeringsgren_sni_2002")) {
        df <- tidyr::separate(
          df,
          naeringsgren_sni_2002,
          c("bransch_kod", "bransch_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$naeringsgren_sni_2002 <- NULL
      }

      if (any(names(df) == "naeringsgren_sni_2007")) {
        df <- tidyr::separate(
          df,
          naeringsgren_sni_2007,
          c("bransch_kod", "bransch_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$naeringsgren_sni_2007 <- NULL
      }

      if (any(names(df) == "industrial_classification_se_sic92")) {
        df <- tidyr::separate(
          df,
          industrial_classification_se_sic92,
          c("bransch_kod", "bransch_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$industrial_classification_se_sic92 <- NULL
      }

      if (any(names(df) == "industrial_classification_nace_rev_2")) {
        df <- tidyr::separate(
          df,
          industrial_classification_nace_rev_2,
          c("bransch_kod", "bransch_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$industrial_classification_nace_rev_2 <- NULL
      }

      if (any(names(df) == "industrial_classification_nace_rev_1_1")) {
        df <- tidyr::separate(
          df,
          industrial_classification_nace_rev_1_1,
          c("bransch_kod", "bransch_namn"),
          sep = " ",
          extra = "merge",
          remove = F
        )
        df$industrial_classification_nace_rev_1_1 <- NULL
      }
      return(df)
    }

    txt <- names(df)
    txt <- gsub("å|Å", "aa", txt)
    txt <- gsub("ä|Ä", "ae", txt)
    txt <- gsub("ö|Ö", "oe", txt)
    names(df) <- txt
    df <- janitor::clean_names(df)

    if (sep_cols == TRUE) {
      df <- fsep_cols(df)
    }

    if (factor_to_string == TRUE) {
      df <- dplyr::mutate_if(df, is.factor, as.character)
    }

    return(df)
  }
