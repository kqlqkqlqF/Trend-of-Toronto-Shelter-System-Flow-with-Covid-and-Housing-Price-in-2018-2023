# Navigating Toronto’s Homelessness Landscape: A Comprehensive Data Analysis of Trends, Compositional Shifts, and Projections for the City’s Homeless Population Dynamics (2018-2023)

This paper was finished in Rstudio. It focus on finding the overall composition and trending of homeless community in Toronto from 2018 January to 2023 December, and develope a linear model to predict its trend in the future.

**Statement of LLM usage: Paper "Overall Analysis of Homeless Community Flow in Toronto Shelter System between 2018-2023" was edited with the help of ChatGPT3.5. The chat history can be found at `llm_usage.txt`, which can be found "llm" folder under `other`. (The words produced by ChatGPT3.5 was not 100% copied to the paper since the words it produced did not meet my criteria.)

- FIle `data` contais all of the data we generated during the study, which includes four types of data: simulated data, raw data, cleaned data, and cleaned data for figures and models.

- File `scripts` contains all scripts serving for this study. These include `00-simulate_data.R`, `01-download_data.R`, `02-data_cleaning.R`, `03-test_data.R`, and `04-model.R`.

- File `model` contains the linear model we generated, `linear_model_month.rds`.

- File `paper` contain the bibliography of the paper `references11.bib`, and the qmd file and PDF file for the paper.

- File `other` contains materials that are not belongs to the upper categories. It includes:

  - `sketches` contains sketches pictures, which show a general idea of what the graph and data should be like. you can find it in the "sketches" folder under `inputs`.

  - `literature` contains pdf of literatures we have referenced in out article.

  -  `llm` which contains the llm statement.
