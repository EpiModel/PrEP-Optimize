

# Core Package Stack for Project ------------------------------------------

# Base EpiModel
install.packages("EpiModel")
install.packages("tidyverse", dep = TRUE)

# Extra Helper Packages
install.packages(c("remotes", "sessioninfo"))

# Fixed Dev Versions of Statnet Packages
remotes::install_github(c("statnet/network@deff2a0",
                          "statnet/networkDynamic@14182bf",
                          "statnet/statnet.common@3307a8c",
                          "statnet/ergm@8b30e92",
                          "statnet/tergm@d3af135"),
                        upgrade = FALSE)

# Latest Dev Versions of EpiModel Packages
remotes::install_github(c("statnet/EpiModel@3914c98",
                          "statnet/EpiModelHPC@257bbf3",
                          "statnet/tergmLite@73d2a2d",
                          "EpiModel/EpiABC@c32ecb6",
                          "EpiModel/ARTnetData@c5ebaba",
                          "EpiModel/ARTnet@150c631"),
                        upgrade = FALSE)

# Current Version of EpiModelHIV for Project
remotes::install_github("EpiModel/EpiModelHIV-p@PrEP-Optim",
                        upgrade = FALSE)


# Package Listing ---------------------------------------------------------

library("EpiModelHIV")
options(width = 100)
sessioninfo::package_info(pkgs = c("network", "networkDynamic", "statnet.common",
                                   "ergm", "tergm", "EpiModel", "EpiModelHPC",
                                   "tergmLite", "EpiABC", "EpiModelHIV",
                                   "ARTnetData", "ARTnet"),
                          dependencies = FALSE)

# Nov 14 2019
# package        * version     date       lib source
# ARTnet           1.0.0       2019-06-06 [1] Github (EpiModel/ARTnet@150c631)
# ARTnetData       1.0         2019-11-14 [1] Github (EpiModel/ARTnetData@c5ebaba)
# EpiABC           1.0         2019-05-16 [1] Github (EpiModel/EpiABC@c32ecb6)
# EpiModel       * 1.7.5       2019-11-14 [1] Github (statnet/EpiModel@3914c98)
# EpiModelHIV    * 1.5.0       2019-11-14 [1] Github (EpiModel/EpiModelHIV-p@eedc7e7)
# EpiModelHPC    * 2.1.0       2019-10-12 [1] Github (statnet/EpiModelHPC@257bbf3)
# ergm           * 3.10.0-4851 2019-11-14 [1] Github (statnet/ergm@8b30e92)
# network        * 1.14-377    2019-11-14 [1] Github (statnet/network@deff2a0)
# networkDynamic * 0.10        2019-05-24 [1] Github (statnet/networkDynamic@14182bf)
# statnet.common   4.3.0-230   2019-11-14 [1] Github (statnet/statnet.common@3307a8c)
# tergm          * 3.6.0-1659  2019-11-14 [1] Github (statnet/tergm@d3af135)
# tergmLite      * 1.2.0       2019-10-17 [1] Github (statnet/tergmLite@73d2a2d)
