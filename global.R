if (!require("shiny")) install.packages("shiny")
if (!require("DT")) install.packages("DT")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("e1071")) install.packages("e1071")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("scales")) install.packages("scales")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("data.table")) install.packages("data.table")

library(shiny)
# library(bslib)
library(DT)
library(openxlsx)
library(e1071)
library(RColorBrewer)
library(scales)
library(dplyr)
library(ggplot2)
# library(data.table)

theme_set(theme_bw())

noms_colnames <- c("age.at.onset", "sex", "prematurity", "family.history.of.epilepsy", "febrile.convulsion", "head.trauma", "meningitis.or.encephalitis", "aura", "abdominal.aura", "psycho.affective.aura", "autonomic.aura", "experiential.aura", "visual.aura", "sensory.aura", "non.specific.aura", "gestural.automatisms", "oro.alimentary.automatisms", "verbal.automatisms", "dystonia.of.a.limb", "at.least.one.focal.to.bilateral.tonic.clonic.seizure")
data_dictionnary <- as.data.frame(openxlsx::read.xlsx(paste('data', 'data_test.xlsx', sep = '/'), "data_dictionary", colNames = TRUE))

algo <- readRDS(paste('data', 'algorithm.RDS', sep = '/'))
cutoff <- 0.7617891

