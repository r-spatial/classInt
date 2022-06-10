## Version 0.4-7

- A new helper function `classify_intervals()` is introduced to return a vector of class intervals of same length as input (@JosiahParry)
- `classIntervals()` has a new style `"maximum"` which returns maximum breaks classification based on the pysal library [mapclassify](https://pysal.org/mapclassify/index.html) (@JosiahParry)
- `findCols()` now takes new argument `factor` which, when `TRUE` returns class membership as a factor with intervals as labels (@JosiahParry)

## Version 0.4-3

- clarify `dataPrecision=` argument in help page

- Add `"headtails"` vignette (@dieghernan)

- Add `"headtails"` style (@dieghernan)