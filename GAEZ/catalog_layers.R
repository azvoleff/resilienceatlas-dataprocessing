library(stringr)

source('../0_settings.R')

in_folder <- file.path(prefix, "GAEZ")

###############################################################################
# Load actual yield layers
act <- list.files(in_folder, pattern='^act[0-9a-zA-Z_]*_yld.tif$')
re_act <- '(^act)([0-9]*)_([irt])_([a-z]*)_([0-9]*)_yld.tif$'
act <- data.frame(str_match(act, re_act))
names(act) <- c('file',
                'type',
                'year',
                'water',
                'crop',
                'year')

###############################################################################
# Load agroclimatic yield layers
acy <- list.files(in_folder, pattern='^res[0-9a-zA-Z_]*_yld.tif$')
re_acy <- '(^[a-z]*)([0-9]*)_([a-z0-9]{2})([a-z0-9]{2})([0-9]{4})([hil])_([a-z]*)([0-9]*)([ab])_yld.tif$'
acy <- data.frame(str_match(acy, re_acy))
names(acy) <- c('file',
                'type',
                'type_1',
                'model',
                'scenario',
                'period',
                'inputs',
                'crop',
                'water',
                'water_1')

###############################################################################
# Load yield gap layers
gap <- list.files(in_folder, pattern='^gap[0-9a-zA-Z_]*_qga.tif$')
re_gap <- '(^[a-z]*)([0-9]*)_([irt])_([a-z]*)_([0-9]*)_([a-z]*).tif$'
gap <- data.frame(str_match(gap, re_gap))
names(gap) <- c('file',
                'type',
                'year',
                'inputs',
                'crop',
                'year_1',
                'units')
