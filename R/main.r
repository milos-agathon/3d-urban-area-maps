# 1. PACKAGES
#------------

libs <- c(
    "tidyverse",
    "terra",
    "giscoR",
    "sf",
    "elevatr",
    "png",
    "httr",
    "rayshader"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libraries == F)){
    install.packages(
        libs[!installed_libraries],
        dependencies = T
    )
}

invisible(
    lapply(
        libs, library, character.only = T
    )
)

# 2. COUNTRY
#------------

country_sf <- giscoR::gisco_get_countries(
    country = "AT",
    resolution = "1"
)

# 3. GHSL BUILT-UP LAYER
#-----------------------

url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_NRES_E2020_GLOBE_R2023A_4326_3ss/V1-0/GHS_BUILT_S_NRES_E2020_GLOBE_R2023A_4326_3ss_V1_0.zip"

file_name <- basename(url)

download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
)

unzip(file_name)

country_builtup <- terra::rast(
    "GHS_BUILT_S_NRES_E2020_GLOBE_R2023A_4326_3ss_V1_0/GHS_BUILT_S_NRES_E2020_GLOBE_R2023A_4326_3ss_V1_0.tif"
) |>
terra::crop(
    terra::vect(country_sf),
    snap = "in",
    mask = T
)

terra::plot(country_builtup)

# 4. GET DEM AND RESAMPLE BUILT-UP LAYER
#---------------------------------------

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 9, clip = "locations"
)

country_builtup_resampled <- terra::resample(
    x = country_builtup,
    y = terra::rast(elev),
    method = "near"
)

crs_lambert <- 
"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_builtup_single <- terra::ifel(
    country_builtup_resampled > 0,
    1,
    country_builtup_resampled
) |>
terra::project(crs_lambert)

# 5. CHANGE COLOR
#----------------

cols <- "#EB0017"
from <- 1
to <- t(
   col2rgb(
    cols
   ) 
)

country_builtup_single <- na.omit(country_builtup_single)

country_builtup_col <- terra::subst(
    country_builtup_single,
    from = from,
    to = to,
    names = cols
)

img_file <- "austria-builtup.png"
terra::writeRaster(
    country_builtup_col,
    img_file,
    NAflag = 0,
    overwrite = T
)

img <- png::readPNG(img_file)

# 6. DEM TO MATRIX
#-----------------

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
    elev_lambert
)

# 7. CITY NAMES
#--------------

filename <- "geonames-population.csv"

get_geonames_data <- function(){
    table_link <- "https://documentation-resources.opendatasoft.com/api/explore/v2.1/catalog/datasets/doc-geonames-cities-5000/exports/csv?lang=en&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
    res <- httr::GET(
        table_link,
        httr::write_disk(
            filename
        ),
        httr::progress()
    )
}

get_geonames_data()

load_geonames_data <- function(){
    places_df <- read.csv(
        filename,
        sep = ";"
    )
    return(places_df)
}

places_df <- load_geonames_data()

head(places_df)

places_modified_df <- places_df[, c(2, 7, 13, 18)]
names(places_modified_df) <- c(
    "name", "country_code", "pop", "coords")

places_modified_df[c("lat", "long")] <-
    stringr::str_split_fixed(
        places_modified_df$coords, ",", 2
    )

places_clean_sf <- places_modified_df |>
    dplyr::filter(country_code == "AT") |>
    dplyr::slice_max(
        pop,
        n = 3
    ) |>
    dplyr::select(
        -coords,
        -country_code,
        -pop
    ) |>
    sf::st_as_sf(
        coords = c(
            "long",
            "lat"
        ),
        crs = 4326
    ) |>
    sf::st_transform(crs = crs_lambert)

max_altitude <- max(
    terra::values(
        elev_lambert,
        na.rm = T
    )
)

# 8. RENDER SCENE
#----------------

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "grey90",
                "grey40"
            )
        )(128)
    ) |>
    rayshader::add_overlay(
        img,
        alphalayer = 1
    ) |>
    rayshader::add_overlay(
        rayshader::generate_label_overlay(
            places_clean_sf,
            extent = elev_lambert,
            text_size = 8,
            color = "#00004e",
            halo_color = "grey90",
            halo_expand = 8,
            halo_blur = 10,
            halo_alpha = .8,
            heightmap = elmat,
            data_label_column = "name"
        )
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 10,
        solid = F,
        shadow = T,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 8, h / 8
        ),
        zoom = .515,
        phi = 85,
        theta = 0
    )

# 9. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(u)

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

rayshader::render_highquality(
    filename = "3d-builtup-austria.png",
    preview = T,
    light = T,
    environment_light = hdri_file,
    intensity_env = 1.25,
    ground_material = rayrender::diffuse(
        color = "grey10"
    ),
    text_size = 90,
    interactive = F,
    width = w,
    height = h
)
