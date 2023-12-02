# Welcome to [Advent of Code 2023](https://adventofcode.com/2023)

This year it is going to be an `F#` advent.

My goal here is to build a 'pipeline' for each task, i.e. to read input once, process it (preferrably in a forward-only manner) and print out a result.
I plan to make each solution not throw any exception, but rather pipe everything as an `Option` of something. This makes the challenge a little bit harder.

## Solved tasks so far

- Day 1
  - [First task](Task-01-01)
  - [Second task](Task-01-02)
- Day 2
  - [First task](Task-02-01)
  - [Second task](Task-02-02)

## Lessons learned (Warning: spoilers ahead)

`Regex.Matches` returns only non-overlapping matches, which prevent its usage in task `01-02`. I was so confused until I realised it failed for cases like `"oneight"` (which should match to `18` and not `11` or `88`).
