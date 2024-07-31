# Item Bank Analysis

This repository is dedicated to analyzing item bank data to gain insights into item usage, identify gaps, and develop guidelines for form building and usage. The following sections provide an overview of the project, including the purpose, research questions, data considerations, and the various analyses conducted.

## Table of Contents

1. [Purpose](#purpose)
2. [Research Questions](#research-questions)
3. [Data Considerations](#data-considerations)
4. [Analyses](#analyses)
   - [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
   - [Trend Analysis](#trend-analysis)
   - [Survival Analysis](#survival-analysis)
   - [Cluster Analysis](#cluster-analysis)
5. [License](#license)

## Purpose

- Gather more information about the item banks
- Determine gaps or areas for improvement
- Develop guidelines for form building and usage
- Figure out how many items we need to add each administration to grow the bank

## Research Questions

### Item Usage

- What are the usage rates of items?
  - Do we have some items sitting in the pool never used?
  - Do we have some items that get used often?
- How many times does an item usually get used before it is retired?
- At what rate do pretest items turn to operational (i.e., pretest items on a form that are retained as operational for subsequent administrations)?
- How many operational items are lost during each administration?
- How many items in the pool aren't very good items but are there and just sit unused?
- How many items get flagged for content review multiple times?
- How many items are okay twice and then get flagged for content review? Are there any patterns?
- Are there enough media items? What is the rate for losing media items? How many need to be pretested to keep the same level number or to grow the bank?
- What about item types (e.g., do PACS items fail at a higher rate than SSMC)?

### Windowed Exams Only

- All of the above research questions are applicable here too
- What proportion of pretest items were scored on the exam?
- Do interpretations of items change if we pool together data from multiple administrations?

## Data Considerations

- Five years of data (if it makes sense to do so)
- If there is incomplete data, discuss with the group and potentially remove.

## Analyses

### Exploratory Data Analysis (EDA)

EDA will help us understand the basic structure and characteristics of the item bank data. We will perform the following:
- Summarize item usage statistics
- Identify items that are never used and those that are frequently used
- Visualize the distribution of item usage

### Trend Analysis

Trend analysis will allow us to identify patterns and trends over time in item usage and other key metrics. We will:
- Analyze the trends in item usage rates over the years
- Identify any seasonal patterns or shifts in item performance

### Survival Analysis

Survival analysis will help us understand the lifespan of items in the pool. We will:
- Estimate the survival function for items
- Identify factors that influence the likelihood of item retirement
- Analyze the rate at which pretest items become operational

### Cluster Analysis

Cluster analysis will help us group items based on their characteristics and usage patterns. We will:
- Perform clustering to identify groups of similar items
- Analyze the characteristics of each cluster to understand common traits
- Identify clusters of items that may need review or improvement


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

