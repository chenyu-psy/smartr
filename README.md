# Overview
This package involves a series of functions that can make your R coding much easier.

# job relative fucntions

The job relative functions are designed to help you run your code in background either sequentially or simultaneously. They relies on the [job package](https://github.com/lindeloev/job) that transfer your code to background. The problem for the job package is that it runs all the jobs simultaneously, which may cause the machine to be busy or overloaded. The `smart_runCustom` function is designed to solve this problem. It creates a temporary job log monitoring the status of the machine. All the codes in jobs will be executed sequentially according to the order and priority.

## Example

The `smart_runCustom` function is similar to the `job` function but with additional arguments. You can write your script as usual. Then wrap parts of it using `smart_runCustom({<your code>})` to run that chunk as a job:

``` R

smartr_runCustom(
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

smartr_runCustom(
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

smartr_runCustom(
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

1. `init_job` : initialize a job log
2. `view_job` : view the job log
3. `update_job` : update the specific jobs from the job log
4. `remove_job` : remove the specific jobs from the job log
5. `smart_runCustom` : Run the custom code in the job; the code will not be executed if your machine is busy.

# Data processing
1. `mutate_form`: Add new columns to the data frame according to the provided formula.

