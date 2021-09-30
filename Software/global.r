

options(java.parameters = "-Xmx3000m")

      if (exists("setupWT"))
        rm(setupWT)

      ipak <- function(pkg) {
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
          install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
      }

      packages <-
        c("ggplot2",
          "shinythemes",
          "shinyjs",
          "readxl",
          "data.table",
          "xlsx",
          "GTest",
          "DEoptim",
          "doParallel",
          "parallel",
          "zoo",
          "shinyFiles",
          "rlist",
          "knitr",
          "markdown",
          "kableExtra",
          "xtable",
          "DT",
          "cowplot",
          "shinyBS",
          "shinyWidgets",
          "datasets")

      ipak(packages)

      cores = detectCores()
      set_cores = 1:cores


      first_straight_section=function(data, endd)
      {
        t=c()
        l=2
        end=round(endd*0.16,0)

        for(i in 1:end)
        {
          min=l
          max=l+10
          r=lm(data[min:max,2] ~ log(data[min:max,1]))
          t[i]=r$coefficients[2]
          l=l+1
        }
        max_slope=which.max(t)
        skin_slope=which(t>t[max_slope]*0.9)
        print(skin_slope)
        final_slope = (lm(data[skin_slope, 2] ~ log10(data[skin_slope, 1])))
        print(final_slope)
        return(final_slope)
      }
