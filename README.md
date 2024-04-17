# Overview
This package involves a series of functions that can make your R coding much easier.

# job relative fucntions

The job relative functions are designed to help you run your code in background either sequentially or simultaneously. They relies on the [job package](https://github.com/lindeloev/job) that transfer your code to background. The problem for the job package is that it runs all the jobs simultaneously, which may cause the machine to be busy or overloaded. The `smart_runFun` function is designed to solve this problem. It creates a temporary job log monitoring the status of the machine. All the codes in jobs will be executed sequentially according to the order and priority.

## Functions and examples

The `smart_run` function is similar to the `job` function but with additional arguments. You can write your script as usual. Then wrap parts of it using `smart_run({<your code>})` to run that chunk as a job. However, the `smart_run` function doesn't support passing environment variables to the job. You need to make sure that all the variables are defined in the job chunk.

``` R

smart_run(
  {
    # your code here
    print("Hello world")
    Sys.sleep(10)
  },
  name = "hello world"
)

```

Additionally, an alternative is the `smart_runFun` function. It executes a function in the job, and arguments can be passed to the job through the function.

``` R

smart_runFun(
  fun = brm,
  args = list(
    formula = mpg ~ wt + qsec,
    data = mtcars
  ),
  name = "brm_model"
)

```

## job log management

There also are a few functions to help you manage the job log:

1. `init_job` : Initialize a job log or clean the job log.
2. `view_job` : view the job log
3. `remove_job` : Remove the specific jobs from the job log

# Data processing

## JATOS relative functions

The JATOS relative functions are designed to help you process the data from JATOS. The data from JATOS is usually in JSON format. 

### Download data from JATOS

The `get_JATOS_data` function is designed to download the data from JATOS. You can specify the study ID and the batch ID to download the data. The data will be saved in the specified folder.

``` R
Table_file_path = get_JATOS_data(
  token = "the token of your personal account",, 
  url = "the url of your server", 
  studyId = 00, # study ID 
  batchId = 00 , # batch ID 
  dataPath = NULL # the path to save the data
)

```

### Read json data

The `read_json_data` function is designed to read the data from the JSON file. The data will be saved in a data frame.

``` R

data = read_json_data(c(file1, file2))

```


## Model relative functions

1. `mutate_form`: Add new columns to the data frame according to the provided formula.

