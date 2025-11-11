# LPJ-GUESS rINS

## What?
- R package to read/write LPJ-GUESS ins files.
- Flexible because you can add any parameters, PFTs, groups or stands to your ins file, and it will handle them, i.e. nothing is hardcoded.
- The reader also recursively includes any "imports" of other ins files, so you just need to read the main one.

## Why?
- For tracking changes in model parameters in a more structured and automated way than using the text files.
- Helpful for doing sensitivity tests!

## Installation
Run the following command from R:
```R
devtools::install_github("wimverbruggen/lpjguess.rINS")
```

## How to use
Make sure to load the package: `library(lpjguess.rINS)`
- Reading: `my_params <- read_ins("my_ins_file.ins")`
- Writing: `write_ins(my_params,"my_new_ins_file.ins")`
- Resolve imported parameters from higher-level groups into PFTs: `resolve_groups(my_params)`

## Notes
- The writer function will create self-contained ins files, so all imports are resolved.
- The `resolve_groups()` function will only preserve the last-added value when a group value is overwritten in the PFT definition (just like the model does)
- Tip: you can use [waldo](https://waldo.r-lib.org) package to easily compare read INS objects: `waldo::compare(my_params_1,my_params_2)`

## To do
- Deal with "param" lines in a better, more structured, way.
- Add metadata, which could be included as comments in the INS file header
