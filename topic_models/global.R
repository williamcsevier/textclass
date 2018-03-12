library(shiny)
library(shinythemes)
library(tidytext)
library(dplyr)
library(textclass)
library(tidyverse)
library(tm)
library(topicmodels)
library(ldatuning)
library(ggraph)
library(igraph)

ITdata <- AFICAdata
sw <- add_row(stop_words, word = c("igf","ot", "ct"), lexicon = c("SMART", "SMART", "SMART"))

