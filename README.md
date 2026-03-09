# Analysis of time-course expression data in maize in control

This analysis complements those from [maize-timecourse-cold](https://github.com/maciej-jonczyk/maize-timecourse-cold). The code presented here is based on a subset of code from mentioned repo. - cleaned/chosen path of analysis.
Both repos. entails the same research project.

## Organization of dirs

For each tissue separate directories and analyses are done.
Numbers of scripst presents order of processing, some have the same number - branching points

### First day as reference

Dirs and files with **one** in name

Using only subset of data for SAM / leaf (*depending on directory*) and design matrix to extract interesting comparisons.

The experiment entailed three days and have three replications.

d1 - d2 - d3

In each day we have three samples: dawn, day, and dusk.

d1[dawn, day, dusk] - d2[dawn, day, dusk] - d3[dawn, day, dusk]

And we want to compare:

d2[dawn, day, dusk] *vs* d1[dawn, day, dusk]
d3[dawn, day, dusk] *vs* d1[dawn, day, dusk]

So in all there are 3 time points and we have 6 comparisons.

There are four inbred lines so the same analysis must be done for each.


