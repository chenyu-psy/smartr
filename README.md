# Overview
This package involves a series of functions that can make your R coding much easier.

# job relative fucntions

The job relative functions are designed to help you run your code in background either sequentially or simultaneously. They relies on the [job package](https://github.com/lindeloev/job) that transfer your code to background. The problem for the job package is that it runs all the jobs simultaneously, which may cause the machine to be busy or overloaded. The `smart_run` function is designed to solve this problem. It creates a temporary job log monitoring the status of the machine. All the codes in jobs will be executed sequentially according to the order and priority.

## Example

The `smart_run` function is similar to the `job` function but with additional arguments. You can write your script as usual. Then wrap parts of it using `smart_run({<your code>})` to run that chunk as a job:

``` R

smart_run(
  {
    # your code here
    print("Hello world")
    Sys.sleep(10)
  },
  untilFinished = FALSE,
  core = 1,
  maxCore = 2,
  name = "hello world"
)

smart_run(
  {
    # your code here
    print("some random words")
    Sys.sleep(10)
  },
  untilFinished = FALSE,
  core = 1,
  maxCore = 2,
  name = "random words"
)

smart_run(
  {
    # your code here
    print("some random words2")
    Sys.sleep(10)
  },
  untilFinished = FALSE,
  core = 1,
  maxCore = 2,
  name = "random words2"
)

```


## Functions

There also are a few functions to help you manage the job log:

1. `init_job` : Initialize a job log or clean the job log.
2. `view_job` : view the job log
3. `remove_job` : Remove the specific jobs from the job log

# Data processing
1. `mutate_form`: Add new columns to the data frame according to the provided formula.

