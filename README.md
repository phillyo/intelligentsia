# Intelligentsia

## Prerequisites

This shinyapp is using the `geocode()` function in the R package `ggmap`. In order for this to work on your computer, you will need to create an account on Google Cloud Platform and generate an API key to Google's Geocoding API.

Additionally, you will need to create a file keyring.R and store the register your key for Google's Geocoding API like so:

```{r eval = FALSE}
googleKey <- "your-API-key"
register_google(key = googleKey) #register for ggmap
```

This script is called by server.R