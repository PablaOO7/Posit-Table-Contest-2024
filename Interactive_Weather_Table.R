# Install required packages from CRAN
install.packages(c("dplyr", "tidyr", "apexcharter", "DT", "lubridate", "toastui", "scales"))


library(dplyr)
library(tidyr)
library(apexcharter)
library(DT)
library(lubridate)
library(toastui)
library(scales)

set.seed(123) # For reproducibility

# List of Indian states
states <- c("Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattisgarh", 
            "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jharkhand", "Karnataka", 
            "Kerala", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya", 
            "Mizoram", "Nagaland", "Odisha", "Punjab", "Rajasthan", "Sikkim", 
            "Tamil Nadu", "Telangana", "Tripura", "Uttar Pradesh", "Uttarakhand", 
            "West Bengal")

# Generate random meteorological data for each state and each month
generate_random_data <- function(state) {
  months <- c("January", "February", "March", "April")
  data <- tibble(
    State = state,
    Month = rep(months, each = 1),
    Temp = list(
      tibble(date = seq.Date(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day"), Temp = runif(31, min = 10, max = 35)),
      tibble(date = seq.Date(as.Date("2020-02-01"), as.Date("2020-02-29"), by = "day"), Temp = runif(29, min = 10, max = 35)),
      tibble(date = seq.Date(as.Date("2020-03-01"), as.Date("2020-03-31"), by = "day"), Temp = runif(31, min = 10, max = 35)),
      tibble(date = seq.Date(as.Date("2020-04-01"), as.Date("2020-04-30"), by = "day"), Temp = runif(30, min = 10, max = 35))
    ),
    rh = list(
      tibble(date = seq.Date(as.Date("2020-01-01"), as.Date("2020-01-31"), by = "day"), rh = runif(31, min = 30, max = 90)),
      tibble(date = seq.Date(as.Date("2020-02-01"), as.Date("2020-02-29"), by = "day"), rh = runif(29, min = 30, max = 90)),
      tibble(date = seq.Date(as.Date("2020-03-01"), as.Date("2020-03-31"), by = "day"), rh = runif(31, min = 30, max = 90)),
      tibble(date = seq.Date(as.Date("2020-04-01"), as.Date("2020-04-30"), by = "day"), rh = runif(30, min = 30, max = 90))
    )
  )
  return(data)
}

# Create dataset for all states
met_india <- bind_rows(lapply(states, generate_random_data))

# Print the structure of the sample data
print(met_india)

library(purrr)
# Calculate minimum and maximum temperature for each state and month
met_india <- met_india %>%
  mutate(min_temp = purrr::map_dbl(Temp, ~ min(.$Temp, na.rm = TRUE)),
         max_temp = purrr::map_dbl(Temp, ~ max(.$Temp, na.rm = TRUE)))

# Round min_temp and max_temp to one decimal place
met_india <- met_india %>%
  mutate(
    min_temp = round(min_temp, 1),
    max_temp = round(max_temp, 1)
  )
# Print the updated structure of the data
print(met_india)



# Create a function to determine font color based on background color intensity
get_font_color <- function(background_color) {
  # Convert hex color to RGB
  rgb <- col2rgb(background_color)
  
  # Calculate luminance
  luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
  
  # Return black or white based on luminance
  if (luminance > 0.5) {
    return("black")
  } else {
    return("white")
  }
}

set_grid_theme(
  row.even.background = "#f7e9d1", # Light beige for even rows
  row.odd.background = "#eddcbc", # Slightly darker beige for odd rows
  cell.normal.border = "#8b4513", # Dark brown border for cells
  cell.normal.showVerticalBorder = TRUE,
  cell.normal.showHorizontalBorder = TRUE,
  cell.header.background = "#cd853f", # Reddish-brown header background
  cell.header.text = "#ffffff", # White text for headers
  cell.selectedHeader.background = "#8b4513", # Darker brown for selected header
  cell.focused.border = "#4d2600", # Golden yellow for focused border
  cell.normal.text = "#4d2600", # Dark reddish-brown text for normal cells
  cell.normal.background = "#fff8dc" # Pale yellow background for normal cells
)

# reset_grid_theme()
# Create the table with visualizations and styled columns
datagrid(met_india, colwidths = "auto", pagination = 8, sortable = TRUE, filters = TRUE) %>%
  grid_complex_header(
    "Indian States Meteorological Data" = names(met_india)
  ) %>%
  grid_columns(
    columns = c("State", "Month", "min_temp", "max_temp"), width = 150
  ) %>%
  grid_sparkline(
    column = "Temp",
    renderer = function(data) {
      apex(data, aes(x = date, y = Temp), type = "area") %>%
        ax_chart(sparkline = list(enabled = TRUE)) %>%
        ax_yaxis(min = 10, max = 40) %>%
        ax_colors(c("#e67e22"))
    }
  ) %>%
  grid_sparkline(
    column = "rh",
    renderer = function(data) {
      apex(data, aes(x = date, y = rh), type = "column") %>%
        ax_chart(sparkline = list(enabled = TRUE)) %>%
        ax_yaxis(min = 30, max = 90) %>%
        ax_colors(c("#27ae60"))  # Setting custom color for the line
    }
  ) %>%
  grid_style_column(
    column = "min_temp",
    background = col_numeric("Blues", domain = range(met_india$min_temp))(met_india$min_temp),
    fontWeight = "bold",
    color = sapply(col_numeric("Blues", domain = range(met_india$min_temp))(met_india$min_temp), get_font_color),
    textAlign = "center"  # Center align the text in max_temp column
  ) %>%
  grid_style_column(
    column = "max_temp",
    background = col_numeric("Reds", domain = range(met_india$max_temp))(met_india$max_temp),
    fontWeight = "bold",
    color = sapply(col_numeric("Reds", domain = range(met_india$max_temp))(met_india$max_temp), get_font_color),
    textAlign = "center"  # Center align the text in max_temp column
  )


