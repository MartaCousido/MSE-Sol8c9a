# Title: Review of packages
# Authors: Marta Cousido-Rocha
# Date: 16/01/2023


# Install packages

devtools::install_github(repo = "flr/FLCore", 
                         ref = "d55bc6570c0134c6bea6c3fc44be20378691e042")
devtools::install_github(repo = "flr/FLash", 
                         ref = "7c47560cf57627068259404bb553f2b644682726")
devtools::install_github(repo = "flr/FLBRP", 
                         ref = "142d5e14137c5ceb4526afd6718c26269ad81e7c")
devtools::install_github(repo = "flr/ggplotFL", 
                         ref = "9b502a1aa01524637f4f269a3353a92c7d452db0")
devtools::install_github(repo = "flr/FLife", 
                         ref = "d0cca5e574a77fb52ec607a25c244969b9c8dd38")
devtools::install_github(repo = "shfischer/mse",
                         ref = "80b5cf18dc9611f7307f599564ccdfbad433948d")

# Review the available ones

packageDescription("FLCore", fields = c("GithubSHA1")) #This does not work
packageDescription("FLash", fields = c("GithubSHA1"))  # The remaining ones 
                                                        # are corrected
packageDescription("FLBRP", fields = c("GithubSHA1"))
packageDescription("ggplotFL", fields = c("GithubSHA1"))
packageDescription("FLife", fields = c("GithubSHA1"))
packageDescription("mseDL", fields = c("GithubSHA1"))
