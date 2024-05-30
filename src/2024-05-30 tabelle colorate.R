# Tabelle colorate

library(pacman)
library(DT)

# Assuming df is your dataframe named statistiche
df <- statistiche

# Define the rowCallback function with a helper function for conditional formatting
rowCallback <- JS(
  "function(row, data, index) {",
  "$('td:eq(0)', row).css('font-weight', 'bold');",  # Apply bold to inputval
  "$('td', row).css('font-size', '12px');",  # Apply font size reduction to all cells
  "function applyColor(value, reference, index) {",
  "if (parseFloat(value) < parseFloat(reference) - (parseFloat(reference) / 3)) {",
  "$('td:eq(' + index + ')', row).css('background-color', 'red');",
  "} else if (parseFloat(value) > parseFloat(reference) + (parseFloat(reference) / 3)) {",
  "$('td:eq(' + index + ')', row).css('background-color', 'magenta');",
  "}",
  "}",
  "applyColor(data[2], data[1], 2);",  # Apply to column 2
  "applyColor(data[3], data[1], 3);",  # Apply to column 3
  "applyColor(data[4], data[1], 4);",  # Apply to column 4
  "applyColor(data[5], data[1], 5);",  # Apply to column 5
  "}"
)

# Generate the DataTable with the custom rowCallback and CSS for header
datatable(df, options = list(
  rowCallback = rowCallback,
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().body()).css({'font-size': '12px'});",  # Font size for table body
    "$(this.api().table().header()).css({'font-size': '12px'});",  # Font size for table header
    "}"
  )
))
