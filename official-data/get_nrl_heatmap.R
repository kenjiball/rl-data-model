# get nrl heatmap data
# Steps to scrape:
# 1. Install ARC welder Chrome Extension to run App
# 2. Generate an APK file for the App
# 3. Run Wireshark to monitor requests
# 4. Find the outgoing GET request

library(xml2)
library(base64)
library(RCurl)


# category MISSED_TACKLES

test_url <- paste0(heatmap_url,
"20191110410?",
"teamId=500001&",
"isMatchCentreHeatmap=true&",
"offenseDefence=DEFENCE&",
"period=FIRST_HALF&",
"category=MISSED_TACKLES")

test_xml <- xml2::read_xml(test_url)

PngBase64 <- xml_text(xml_find_all(test_xml,"PngBase64"))

#par(mfrow = c(1,2))
txt <- PngBase64
plot(x = -50:50, y = -50:50, type='n')
rasterImage(getImg(txt), -50, -50, 50, 50, interpolate=FALSE)
dev.off()


# getImg function
# From: https://stackoverflow.com/questions/46032969/how-to-display-base64-images-in-r
getImg <- function(txt) {
  raw <- base64Decode(txt, mode="raw")
  if (all(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))==raw[1:8])) { # it's a png...
    img <- png::readPNG(raw)
    transparent <- img[,,4] == 0
    img <- as.raster(img[,,1:3])
    img[transparent] <- NA
  } else if (all(as.raw(c(0xff, 0xd8, 0xff, 0xd9))==raw[c(1:2, length(raw)-(1:0))])) { # it's a jpeg...
    img <- jpeg::readJPEG(raw)
  } else stop("No Image!")
  return(img)
}