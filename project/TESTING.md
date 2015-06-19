# Testing methodology

There are several layers of testing for the CoMP data pipeline code.


## Unit testing

Unit tests are the simplest level of tests. They should test individual routines, passing in known input and verifying that the output matches the known result. It is particularly useful to check edge and corner cases of the possible inputs. Unit tests pass/fail.

Examples: tests of `COMP_EXTRACT_TIME` with given inputs/outputs that can be checked.


## Regression testing

Regression tests run the entire pipeline on a fixed raw input, e.g., 20120226, and verify that the result has not changed. If the result has changed, then the reason for the change needs to be found and approved (or fixed so that the result is not changed). Regression tests are pass/fail.

Examples: sets of raw data (potentially not the entire day) and the output to compare to.


## Verification

Verification testing is done after the pipeline runs on any given data and checks that the output is consistent.

Examples: check to make sure times of output files are increasing.


## Data quality

Data quality tests quantify some metric of the output data. Data quality tests are not necessarily pass/fail. They may result in logs or plots. The logs/plots must be examined to determine if there is a problem with the instrument, the processing, or some other condition. This may result in changes to the instrument or data processing pipeline.

Examples: plot of the median value of the flats, plot of the pixel value of the
center of the images.
