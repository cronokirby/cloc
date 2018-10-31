# cloc

Count the lines of code in a project.

## Building
This project uses `stack` but building with other
things should be as straightforward:
```
stack install
```

## Usage
```
cloc file1 file2
```
Will count and accumulate the lines of code in both files.
Using this with some kind of shell helps. For example,
to recursively count all haskell files in `src`, you can do:
```
cloc src/**/*.hs
```
