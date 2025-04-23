# run the following line of code:
renv::status()

# if it says "No issues found -- the project is in a consistent state.",
# all is well. close this file and proceed to 01_run_apps.R


# otherwise, run the following line:
# you will definitely need to run it the first time you use
# this directory on a new computer
renv::restore()

# tell it "yes" in whatever way it requests in the console
# to let it do its thing. Then go back and re-run line 2, renv::status().

# reach out to Kim if things don't seem to be working properly.