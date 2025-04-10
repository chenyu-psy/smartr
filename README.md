# Overview

This package primarily enhances the personal programming experience when conducting Bayesian Framework analyses in R. While most functions may not be general enough, they can be useful in specific situations.

This package has only been tested on Mac OS and Linux. We welcome feedback from Windows users who encounter any issues while using this package.

# Job Manager

The Job Manager provides a robust system for efficient background code execution in R. Built on the job package, it adds intelligent job scheduling, monitoring, and resource management to prevent system overload.

## Core Functions

### 1. Running Background Jobs

The `smart_runFun` function executes any R function in the background with full control over arguments, priorities, and naming:

``` r
smart_runFun(
  fun = brm,                         # Any R function
  args = list(                       # Arguments passed to the function
    formula = mpg ~ wt + qsec,
    data = mtcars
  ),
  name = "brm_model",                # Custom name for tracking
  priority = 1,                      # Optional: priority level (higher runs first)
)
```

### 2. Job Management

The package provides functions to manage computational jobs.

#### Initialize Job Log

Create and configure the job tracking system:

``` r
init_job()  # Creates job log in temp directory
init_job("./my_jobs/")  # Specify custom directory
```

#### View Job Status

Track progress of all background processes:

``` r
view_job()  # View all jobs
view_job(1)  # View job by index
view_job("model_fit")  # View job by name
```

#### Remove Jobs

Remove completed or unwanted jobs:

``` r
remove_job(1)  # Remove by index
remove_job("model_fit")  # Remove by name
```

## Advanced Functions

This package also includes advanced functions for model comparison

### 1. Parallel Model Comparison

The `parallel_model_comparison` function enables efficient comparison of multiple statistical models using parallel execution. It automates model fitting, bridge sampling, and Bayes Factor calculations across parameter combinations.

#### Key Features:

-   Parallel execution of model fitting and bridge sampling
-   Automatic model comparison table generation
-   Support for both `brms` and `bmm` models
-   Built-in checks for existing results to avoid redundant computation

#### Basic Usage:

``` r

list_for_model_comparison <- list(
  par_a = c("a1", "a2"),
  par_b = c("b1", "b2")
)

parallel_model_comparison(
  fun = brm,  # Model fitting function
  pars = list_for_model_comparison,
  form_fun = gen_formula,
  args = list(  # Common arguments for all models
    data = my_data,
    chains = 4
  ),
  model_name = "Model",
  model_path = "./models",  # Directory to store fitted models
  sample_path = "./samples"  # Directory to store bridge samples
)
```

### 2. Sequential Model Comparison

# Data processing

## JATOS relative functions

The JATOS relative functions are designed to help you process the data from JATOS. The data from JATOS is usually in JSON format.

### 1. Download Data from JATOS

The `get_JATOS_data` function downloads result data and returns metadata about the downloaded files. Data is saved in either: - The specified `dataPath` directory, or - A "JATOS_DATA" subfolder if no path is specified

``` r
# Download data and get metadata
metadata <- get_JATOS_data(
  token = "your_api_token",
  batchId = c(123, 124),  # One or more batch IDs
  dataPath = "./downloaded_data/",  # Optional custom path
  attachments = FALSE               # Set TRUE to download attachments
)

# View metadata structure
head(metadata)
```

### 2. Read Metadata

When you use the `get_JATOS_data` function to download data from JATOS, the metadata is also saved. The next time, you can use the `read_metaData` function to access it.

``` r
metadata <- read_metaData("./downloaded_data/metadata.json")
```

### 3. Enrich and Filter Metadata

Add information extracted from data files and filter results:

``` r
# Extract additional info from data files (e.g., participant IDs)
metadata <- extract_data_info(
  metadata,
  info = c("participant_id"),  # Keys to extract
  warn = FALSE
)

# Filter metadata (e.g., only completed components)
completed_data <- metadata %>% 
  filter(studyState == "FINISHED")
```

### 4. Read and Analyze Data

Read all JSON data files from filtered results:

``` r
# Read all data files (NA paths automatically excluded)
study_data <- read_json_data(completed_data$file)

# Analyze the combined data
summary(study_data)
```

### Notes:

-   An introduction to creating API tokens in JATOS can be found at: <https://www.jatos.org/JATOS-API.html#personal-access-tokens>
-   Data is saved in organized subfolders by batch ID and result ID
-   The metadata contains timestamps, durations, and file paths for all results

## Aggregation relative functions

The aggregation relative functions are designed to help you aggregate the data.

1.  `agg_plot`: Summarize the mean, standard deviation, and 95% confidence interval of the data for plotting.
2.  `agg_multinomial`: Aggregate the data for models using a multinomial data distribution.

## Model relative functions

1.  `mutate_form`: Add new columns to the data frame according to the provided formula.
