#' Create a Processed Version of the Ames Housing Data
#'
#' @details
#' For the processed version, the exact details can be found in
#'  the code of `make_ames` but a summary of the differences between
#'  these data sets and `ames_raw` is:
#'
#' * All factors are _unordered_.
#' * `PID` and `Order` are removed.
#' * Spaces and special characters in column names where changed
#'  to snake case. To be consistent, `SalePrice` was changed to
#'  `Sale_Price`.
#' * Many factor levels were changed to be more understandable
#'  (e.g. `Split_or_Multilevel` instead of `080`)
#' * Many missing values were reset. For example, if the variable
#'  `Bsmt_Qual` was missing, this implies that there is no basement
#'  on the property. Instead of a missing value, the value of
#'  `Bsmt_Qual` was changed to `No_Basement`. Similarly, numeric
#'  data pertaining to basements were set to zero where appropriate
#'  such as variables `Bsmt_Full_Bath` and `Total_Bsmt_SF`.
#' * `Garage_Yr_Blt` contained many missing data and was removed.
#' * Approximate longitude and latitude are included for the
#'  properties. Also, note that there are 6 properties with
#'  identical geotags. These are units within the same building.
#'  For some properties, updated versions of the PID identifiers
#'  were found and are replaced with new values.
#'
#' `make_ordinal_ames` is the same as `make_ames` but many factor
#'   variables were changed to class `ordered` (see below).
#'
#'  The documentation for [ames_raw()] contains descriptions of
#'   the columns although, as noted above, the column names in
#'   [ames_raw()] are slightly different from the processed
#'   versions.
#'
#' `make_ames_new()` creates a data set of new properties. These were populated
#'  using less data sources than the original and lack a number of the condition
#'  and quality. Both properties were unsold at the time of this writing.
#' @return A tibble with the data.
#' @examples
#' ames <- make_ames()
#' nrow(ames)
#' summary(ames$Sale_Price)
#'
#' ames_ord <- make_ordinal_ames()
#' ord_vars <- vapply(ames_ord, is.ordered, logical(1))
#' names(ord_vars)[ord_vars]
#' @export
#' @importFrom dplyr add_rownames add_rownames vars contains
#' @importFrom dplyr funs rename_at rename mutate recode_factor
#' @importFrom dplyr recode filter select inner_join
#
make_ames <- function() {
  process_ames(AmesHousing::ames_raw)
}

#' @export
#' @rdname make_ames
make_ames_new <- function() {
  process_ames(AmesHousing::ames_new)
}

process_ames <- function(dat) {
  out <- dat %>%
    # Rename variables with spaces or begin with numbers.
    # SalePrice would be inconsistently named so change that too.
    dplyr::rename_with(
      ~ gsub(' ', '_', .),
      dplyr::contains(' '),
    ) %>%
    dplyr::rename(
      Sale_Price = SalePrice,
      Three_season_porch = `3Ssn_Porch`,
      Year_Remod_Add = `Year_Remod/Add`,
      First_Flr_SF = `1st_Flr_SF`,
      Second_Flr_SF = `2nd_Flr_SF`,
      Year_Sold = Yr_Sold
    ) %>%
    # Remove leading zeros
    dplyr::mutate(
      MS_SubClass = as.character(as.integer(MS_SubClass))
    ) %>%
    # Make more meaningful factor levels for some variables
    dplyr::mutate(
      MS_SubClass =
        dplyr::recode_factor(
          factor(MS_SubClass),
          '20' = 'One_Story_1946_and_Newer_All_Styles',
          '30' = 'One_Story_1945_and_Older',
          '40' = 'One_Story_with_Finished_Attic_All_Ages',
          '45' = 'One_and_Half_Story_Unfinished_All_Ages',
          '50' = 'One_and_Half_Story_Finished_All_Ages',
          '60' = 'Two_Story_1946_and_Newer',
          '70' = 'Two_Story_1945_and_Older',
          '75' = 'Two_and_Half_Story_All_Ages',
          '80' = 'Split_or_Multilevel',
          '85' = 'Split_Foyer',
          '90' = 'Duplex_All_Styles_and_Ages',
          '120' = 'One_Story_PUD_1946_and_Newer',
          '150' = 'One_and_Half_Story_PUD_All_Ages',
          '160' = 'Two_Story_PUD_1946_and_Newer',
          '180' = 'PUD_Multilevel_Split_Level_Foyer',
          '190' = 'Two_Family_conversion_All_Styles_and_Ages'
        )
    ) %>%
    dplyr::mutate(
      MS_Zoning =
        dplyr::recode_factor(
          factor(MS_Zoning),
          'A' = 'Agriculture',
          'C' = 'Commercial',
          'FV' = 'Floating_Village_Residential',
          'I' = 'Industrial',
          'RH' = 'Residential_High_Density',
          'RL' = 'Residential_Low_Density',
          'RP' = 'Residential_Low_Density_Park',
          'RM' = 'Residential_Medium_Density',
          'A (agr)' = 'A_agr',
          'C (all)' = 'C_all',
          'I (all)' = 'I_all'
        )
    ) %>%
    dplyr::mutate(
      Lot_Shape =
        dplyr::recode_factor(
          factor(Lot_Shape),
          'Reg' = 'Regular',
          'IR1' = 'Slightly_Irregular',
          'IR2' = 'Moderately_Irregular',
          'IR3' = 'Irregular'
        )
    ) %>%
    dplyr::mutate(Bldg_Type =
                    dplyr::recode_factor(factor(Bldg_Type),
                                         '1Fam' = 'OneFam',
                                         '2fmCon' = 'TwoFmCon')) %>%
    # Change some factor levels so that they make valid R variable names
    dplyr::mutate(
      House_Style =  gsub("^1.5", "One_and_Half_", House_Style),
      House_Style =  gsub("^1", "One_", House_Style),
      House_Style =  gsub("^2.5", "Two_and_Half_", House_Style),
      House_Style =  gsub("^2", "Two_", House_Style),
      House_Style = factor(House_Style)
    ) %>%
    # Some characteristics that houses lack (e.g. garage, pool) are
    # coded as missing instead of "No_pool" or "No_Garage". Change these
    # and also cases where the number of missing (e.g. garage size)
    dplyr::mutate(
      Bsmt_Exposure = ifelse(is.na(Bsmt_Exposure), "No_Basement", Bsmt_Exposure),
      Bsmt_Exposure = factor(Bsmt_Exposure),
      BsmtFin_Type_1 = ifelse(is.na(BsmtFin_Type_1), "No_Basement", BsmtFin_Type_1),
      BsmtFin_Type_1 = factor(BsmtFin_Type_1),
      BsmtFin_SF_1 = ifelse(is.na(BsmtFin_SF_1), 0, BsmtFin_SF_1),
      BsmtFin_Type_2 = ifelse(is.na(BsmtFin_Type_2), "No_Basement", BsmtFin_Type_2),
      BsmtFin_Type_2 = factor(BsmtFin_Type_2),
      BsmtFin_SF_2 = ifelse(is.na(BsmtFin_SF_2), 0, BsmtFin_SF_2),
      Bsmt_Unf_SF = ifelse(is.na(Bsmt_Unf_SF), 0, Bsmt_Unf_SF),
      Total_Bsmt_SF = ifelse(is.na(Total_Bsmt_SF), 0, Total_Bsmt_SF),
      Bsmt_Full_Bath = ifelse(is.na(Bsmt_Full_Bath), 0, Bsmt_Full_Bath),
      Bsmt_Half_Bath = ifelse(is.na(Bsmt_Half_Bath), 0, Bsmt_Half_Bath),
      Electrical = ifelse(is.na(Electrical), "Unknown", Electrical),
    ) %>%
    dplyr::mutate(Garage_Type =
                    dplyr::recode(Garage_Type,
                                  '2Types' = 'More_Than_Two_Types')) %>%
    dplyr::mutate(
      Garage_Type = ifelse(is.na(Garage_Type), "No_Garage", Garage_Type),
      Garage_Finish = ifelse(is.na(Garage_Finish), "No_Garage", Garage_Finish),
      Garage_Cars = ifelse(is.na(Garage_Cars), 0, Garage_Cars),
      Garage_Area = ifelse(is.na(Garage_Area), 0, Garage_Area),
      Bsmt_Full_Bath = ifelse(is.na(Bsmt_Full_Bath), 0, Bsmt_Full_Bath),
      Bsmt_Half_Bath = ifelse(is.na(Bsmt_Half_Bath), 0, Bsmt_Half_Bath),
      Misc_Feature = ifelse(is.na(Misc_Feature), "None", Misc_Feature),
      Mas_Vnr_Type = ifelse(is.na(Mas_Vnr_Type), "None", Mas_Vnr_Type),
      Mas_Vnr_Area = ifelse(is.na(Mas_Vnr_Area), 0, Mas_Vnr_Area),
      Lot_Frontage = ifelse(is.na(Lot_Frontage), 0, Lot_Frontage)
    ) %>%
    mutate(
      Overall_Qual =
        dplyr::recode(
          Overall_Qual,
          `10` = "Very_Excellent",
          `9` = "Excellent",
          `8` = "Very_Good",
          `7` = "Good",
          `6` = "Above_Average",
          `5` = "Average",
          `4` = "Below_Average",
          `3` = "Fair",
          `2` = "Poor",
          `1` = "Very_Poor"
        )
    ) %>%
    mutate(
      Overall_Cond =
        dplyr::recode(
          Overall_Cond,
          `10` = "Very_Excellent",
          `9` = "Excellent",
          `8` = "Very_Good",
          `7` = "Good",
          `6` = "Above_Average",
          `5` = "Average",
          `4` = "Below_Average",
          `3` = "Fair",
          `2` = "Poor",
          `1` = "Very_Poor"
        )
    ) %>%
    mutate(
      Exter_Qual =
        dplyr::recode(
          Exter_Qual,
          "Ex" = "Excellent",
          "Gd" = "Good",
          "TA" = "Typical",
          "Fa" = "Fair",
          "Po" = "Poor"
        )
    ) %>%
    mutate(
      Exter_Cond =
        dplyr::recode(
          Exter_Cond,
          "Ex" = "Excellent",
          "Gd" = "Good",
          "TA" = "Typical",
          "Fa" = "Fair",
          "Po" = "Poor"
        )
    ) %>%
    mutate(
      Bsmt_Qual =
        dplyr::recode(
          Bsmt_Qual,
          "Ex" = "Excellent",
          "Gd" = "Good",
          "TA" = "Typical",
          "Fa" = "Fair",
          "Po" = "Poor",
          .missing = "No_Basement"
        )
    ) %>%
    mutate(
      Bsmt_Cond =
        dplyr::recode(
          Bsmt_Cond,
          "Ex" = "Excellent",
          "Gd" = "Good",
          "TA" = "Typical",
          "Fa" = "Fair",
          "Po" = "Poor",
          .missing = "No_Basement"
        )
    ) %>%
    mutate(
      Heating_QC =
        dplyr::recode(
          Heating_QC,
          "Ex" = "Excellent",
          "Gd" = "Good",
          "TA" = "Typical",
          "Fa" = "Fair",
          "Po" = "Poor"
        )
    ) %>%
    mutate(
      Kitchen_Qual =
        dplyr::recode(
          Kitchen_Qual,
          "Ex" = "Excellent",
          "Gd" = "Good",
          "TA" = "Typical",
          "Fa" = "Fair",
          "Po" = "Poor"
        )
    ) %>%
    mutate(
      Fireplace_Qu =
        dplyr::recode(
          Fireplace_Qu,
          "Ex" = "Excellent",
          "Gd" = "Good",
          "TA" = "Typical",
          "Fa" = "Fair",
          "Po" = "Poor",
          .missing = "No_Fireplace"
        )
    ) %>%
    mutate(
      Garage_Qual =
        dplyr::recode(
          Garage_Qual,
          "Ex" = "Excellent",
          "Gd" = "Good",
          "TA" = "Typical",
          "Fa" = "Fair",
          "Po" = "Poor",
          .missing = "No_Garage"
        )
    ) %>%
    mutate(
      Garage_Cond =
        dplyr::recode(
          Garage_Cond,
          "Ex" = "Excellent",
          "Gd" = "Good",
          "TA" = "Typical",
          "Fa" = "Fair",
          "Po" = "Poor",
          .missing = "No_Garage"
        )
    ) %>%
    mutate(
      Pool_QC =
        dplyr::recode(
          Pool_QC,
          "Ex" = "Excellent",
          "Gd" = "Good",
          "TA" = "Typical",
          "Fa" = "Fair",
          "Po" = "Poor",
          .missing = "No_Pool"
        )
    ) %>%
    mutate(
      Neighborhood =
        dplyr::recode(
          Neighborhood,
          "Blmngtn" = "Bloomington_Heights",
          "Bluestem" = "Bluestem",
          "BrDale" = "Briardale",
          "BrkSide" = "Brookside",
          "ClearCr" = "Clear_Creek",
          "CollgCr" = "College_Creek",
          "Crawfor" = "Crawford",
          "Edwards" = "Edwards",
          "Gilbert" = "Gilbert",
          "Greens" = "Greens",
          "GrnHill" = "Green_Hills",
          "IDOTRR" = "Iowa_DOT_and_Rail_Road",
          "Landmrk" = "Landmark",
          "MeadowV" = "Meadow_Village",
          "Mitchel" = "Mitchell",
          "NAmes" = "North_Ames",
          "NoRidge" = "Northridge",
          "NPkVill" = "Northpark_Villa",
          "NridgHt" = "Northridge_Heights",
          "NWAmes" = "Northwest_Ames",
          "OldTown" = "Old_Town",
          "SWISU" = "South_and_West_of_Iowa_State_University",
          "Sawyer" = "Sawyer",
          "SawyerW" = "Sawyer_West",
          "Somerst" = "Somerset",
          "StoneBr" = "Stone_Brook",
          "Timber" = "Timberland",
          "Veenker" = "Veenker",
          "Hayden Lake" = "Hayden_Lake"
        )
    ) %>%
    mutate(
      Alley =
        dplyr::recode(
          Alley,
          "Grvl" = "Gravel",
          "Pave" = "Paved",
          .missing = "No_Alley_Access"
        )
    ) %>%
    mutate(
      Paved_Drive =
        dplyr::recode(
          Paved_Drive,
          "Y" = "Paved",
          "P" = "Partial_Pavement",
          "N" = "Dirt_Gravel"
        )
    )   %>%
    mutate(
      Fence =
        dplyr::recode(
          Fence,
          "GdPrv" = "Good_Privacy",
          "MnPrv" = "Minimum_Privacy",
          "GdWo" = "Good_Wood",
          "MnWw" = "Minimum_Wood_Wire",
          .missing = "No_Fence"
        )
    )   %>%
    # Convert everything else to factors
    dplyr::mutate(
      Alley = factor(Alley),
      Bsmt_Qual = factor(Bsmt_Qual),
      Bsmt_Cond = factor(Bsmt_Cond),
      Central_Air = factor(Central_Air),
      Condition_1 = factor(Condition_1),
      Condition_2 = factor(Condition_2),
      Electrical = factor(Electrical),
      Exter_Cond = factor(Exter_Cond),
      Exter_Qual = factor(Exter_Qual),
      Exterior_1st = factor(Exterior_1st),
      Exterior_2nd = factor(Exterior_2nd),
      Fence = factor(Fence),
      Fireplace_Qu = factor(Fireplace_Qu),
      Foundation = factor(Foundation),
      Functional = factor(Functional),
      Garage_Cond = factor(Garage_Cond),
      Garage_Finish = factor(Garage_Finish),
      Garage_Qual = factor(Garage_Qual),
      Garage_Type = factor(Garage_Type),
      Heating = factor(Heating),
      Heating_QC = factor(Heating_QC),
      Kitchen_Qual = factor(Kitchen_Qual),
      Land_Contour = factor(Land_Contour),
      Land_Slope = factor(Land_Slope),
      Lot_Config = factor(Lot_Config),
      Mas_Vnr_Type = factor(Mas_Vnr_Type),
      Misc_Feature = factor(Misc_Feature),
      Paved_Drive = factor(Paved_Drive),
      Pool_QC = factor(Pool_QC),
      Roof_Matl = factor(Roof_Matl),
      Roof_Style = factor(Roof_Style),
      Sale_Condition = factor(Sale_Condition),
      Sale_Type = factor(Sale_Type),
      Street = factor(Street),
      Utilities = factor(Utilities),
      Overall_Qual = factor(Overall_Qual, levels = rev(ten_point)),
      Overall_Cond = factor(Overall_Cond, levels = rev(ten_point))
    ) %>%
    # see issue #2, updated PIDs for some properties
    mutate(
      PID = ifelse(PID == "0904351040", "0904351045,", PID),
      PID = ifelse(PID == "0535300120", "0535300125,", PID),
      PID = ifelse(PID == "0902401130", "0902401135,", PID),
      PID = ifelse(PID == "0906226090", "0906226090,", PID),
      PID = ifelse(PID == "0908154040", "0908154045,", PID),
      PID = ifelse(PID == "0909129100", "0909129105,", PID),
      PID = ifelse(PID == "0914465040", "0914465043,", PID),
      PID = ifelse(PID == "0902103150", "0902103145,", PID),
      PID = ifelse(PID == "0902401120", "0902401125,", PID),
      PID = ifelse(PID == "0916253320", "0916256880,", PID),
      PID = ifelse(PID == "0916477060", "0916477065,", PID),
      PID = ifelse(PID == "0916325040", "0916325045,", PID)
    ) %>%
    dplyr::inner_join(AmesHousing::ames_geo, by = "PID") %>%
    # Garage_Yr_Blt is removed due to a fair amount of missing data
    dplyr::select(-Order,-PID, -Garage_Yr_Blt)

  out <- out %>%
    dplyr::mutate(
      Neighborhood = factor(Neighborhood, levels = AmesHousing::hood_levels)
    )

  out
}

ten_point <- c(
  "Very_Excellent",
  "Excellent",
  "Very_Good",
  "Good",
  "Above_Average",
  "Average",
  "Below_Average",
  "Fair",
  "Poor",
  "Very_Poor"
)
five_point <- c(
  "Excellent",
  "Good",
  "Typical",
  "Fair",
  "Poor"
)

#' @rdname make_ames
#' @export
make_ordinal_ames <- function() {
  get_no <- function(x)
    grep("^No", levels(x), value = TRUE)

  out <- make_ames()
  out$Lot_Shape <- ordered(
    as.character(out$Lot_Shape),
    levels = c("Irregular", "Moderately_Irregular",
               "Slightly_Irregular", "Regular")
  )
  out$Land_Contour <- ordered(
    as.character(out$Land_Contour),
    levels = c("Low", "HLS", "Bnk", "Lvl")
  )
  out$Utilities <- ordered(
    as.character(out$Utilities),
    levels = c("ELO", "NoSeWa", "NoSewr", "AllPub")
  )
  out$Land_Slope <- ordered(
    as.character(out$Land_Slope),
    levels = c("Sev", "Mod", "Gtl")
  )
  out$Overall_Qual <- ordered(
    as.character(out$Overall_Qual),
    levels = rev(ten_point)
  )
  out$Overall_Cond <- ordered(
    as.character(out$Overall_Cond),
    levels = rev(ten_point)
  )
  out$Exter_Qual <- ordered(
    as.character(out$Exter_Qual),
    levels = rev(five_point)
  )
  out$Exter_Cond <- ordered(
    as.character(out$Exter_Cond),
    levels = rev(five_point)
  )
  out$Bsmt_Qual <- ordered(
    as.character(out$Bsmt_Qual),
    levels = c(get_no(out$Bsmt_Qual), rev(five_point))
  )
  out$Bsmt_Cond <- ordered(
    as.character(out$Bsmt_Cond),
    levels = c(get_no(out$Bsmt_Cond), rev(five_point))
  )
  out$Bsmt_Exposure <- ordered(
    as.character(out$Bsmt_Exposure),
    levels = c(
      "No_Basement", "No", "Mn", "Av", "Gd"
    )
  )
  out$BsmtFin_Type_1 <- ordered(
    as.character(out$BsmtFin_Type_1),
    levels = c(
      "No_Basement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"
    )
  )
  out$BsmtFin_Type_2 <- ordered(
    as.character(out$BsmtFin_Type_2),
    levels = c(
      "No_Basement", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"
    )
  )
  out$Heating_QC <- ordered(
    as.character(out$Heating_QC),
    levels = rev(five_point)
  )
  out$Electrical <- ordered(
    as.character(out$Electrical),
    levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr")
  )
  out$Kitchen_Qual <- ordered(
    as.character(out$Kitchen_Qual),
    levels = rev(five_point)
  )
  out$Functional <- ordered(
    as.character(out$Functional),
    levels = c(
      "Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"
    )
  )
  out$Fireplace_Qu <- ordered(
    as.character(out$Fireplace_Qu),
    levels = c(get_no(out$Fireplace_Qu), rev(five_point))
  )
  out$Garage_Finish <- ordered(
    as.character(out$Garage_Finish),
    levels = c(get_no(out$Garage_Finish), "Unf", "RFn", "Fin")
  )
  out$Garage_Qual <- ordered(
    as.character(out$Garage_Qual),
    levels = c(get_no(out$Garage_Qual), rev(five_point))
  )
  out$Garage_Cond <- ordered(
    as.character(out$Garage_Cond),
    levels = c(get_no(out$Garage_Cond), rev(five_point))
  )
  out$Paved_Drive <- ordered(
    as.character(out$Paved_Drive),
    levels = c("Dirt_Gravel", "Partial_Pavement", "Paved")
  )
  out$Pool_QC <- ordered(
    as.character(out$Pool_QC),
    levels = c(get_no(out$Pool_QC), rev(five_point))
  )
  out$Fence <- ordered(
    as.character(out$Fence),
    levels = c("No_Fence", "Minimum_Wood_Wire", "Good_Wood",
               "Minimum_Privacy", "Good_Privacy")
  )
  out
}



ames_vars <-
  c('.', 'SalePrice', '3Ssn_Porch', 'Year_Remod/Add', '1st_Flr_SF',
    '2nd_Flr_SF', 'MS_SubClass', 'MS_Zoning', 'Alley', 'Lot_Shape',
    'Bldg_Type', 'House_Style', 'Bsmt_Qual', 'Bsmt_Cond',
    'Bsmt_Exposure', 'BsmtFin_Type_1', 'BsmtFin_SF_1',
    'BsmtFin_Type_2', 'BsmtFin_SF_2', 'Bsmt_Unf_SF', 'Total_Bsmt_SF',
    'Bsmt_Full_Bath', 'Bsmt_Half_Bath', 'Fireplace_Qu', 'Garage_Type',
    'Garage_Finish', 'Garage_Qual', 'Garage_Cond', 'Garage_Cars',
    'Garage_Area', 'Pool_QC', 'Fence', 'Misc_Feature', 'Mas_Vnr_Type',
    'Mas_Vnr_Area', 'Lot_Frontage', 'Central_Air', 'Condition_1',
    'Condition_2', 'Electrical', 'Exter_Cond', 'Exter_Qual',
    'Exterior_1st', 'Exterior_2nd', 'Foundation', 'Functional',
    'Heating', 'Heating_QC', 'Kitchen_Qual', 'Land_Contour',
    'Land_Slope', 'Lot_Config', 'Neighborhood', 'Yr_Sold',
    'Overall_Cond', 'Overall_Qual', 'Paved_Drive',
    'Roof_Matl', 'Roof_Style', 'Sale_Condition', 'Sale_Type',
    'Street', 'Utilities', 'Order', 'PID', 'Garage_Yr_Blt')

#' @importFrom utils globalVariables
utils::globalVariables(ames_vars)
