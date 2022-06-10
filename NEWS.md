## Version 0.4-4

- A new helper function `classify_intervals()` is introduced to return a vector of class intervals of same length as input (@JosiahParry)
- `classIntervals()` has a new method `"maximum"` which returns maximum breaks classification based on the pysal library [mapclassify](https://pysal.org/mapclassify/index.html) (@JosiahParry)
- `findCols()` now takes new argument `factor` which, when `TRUE` returns class membership as a factor with intervals as labels (@JosiahParry)

