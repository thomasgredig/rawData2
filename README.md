# rawData2

RAW files are instrument generated data files. For reproducibility, 
this package generates unique identifiers, so that files can be easily
found on different computing platforms regardless of the filename. 
This makes collaborations more effective.

## Problem

This package helps solve the problem of copying large numbers of files to a 
local directory. The example illustrates this by using 1 file from `Alice` 
and 1 file from `Bob` for a data analysis. Users may use different sub-directories 
or even names to collaborate on the analysis. The analysis code will continue
to work, even if more files are added or files are moved to other directories.

``` bash
# Alice's directory
sharedFolder/RAW/A/A1.txt
alice/RAW/A1.txt
alice/RAW/alice/A2.txt
alice/RAW/bob/bob3.txt
```

In this case, `Alice` kept her original file `A1.txt` on her local folder, but also
copied it into the shared folder. She also copied files from her collaborator `Bob`
into a subfolder called `bob`. Meanwhile `Bob`'s computer looks as follows:

``` bash
# Bob's directory
C:/sharedFolder/RAW/A/A1.txt
C:/bob/RAW/instrument/bob_instr.txt
```

For the data analysis, Bob uses two files, one from Alice and his own file.
Even though the file was labeled `bob3.txt`, he later renamed it `bob_instr.txt`.
Alice still has the old filename that she received at an earlier date. 

If `Bob` were to refer to `bob_instr.txt` in his analysis code, Alice cannot 
recreate the figure, even though she has all the files. Also it is cumbersome
to refer to local files, as both of them use different operating systems and
their absolute file directories differ markedly and moving it relative has
become an issues, since `Bob` is known for moving files around sub-directories.

## Workflow

Since each file remains constant (RAW data file) over time, then a hash (SHA256) is generated.
From the hash a shorter 6-digit `base64` code is generated that identifies the file with
minimal collisions. `Bob` will write the analysis code by finding the RAW ID.

``` r
library(rawdata2)
paths <- c("sharedFolder/RAW","bob/RAW")
raw_update(paths)
raw_find(c("bob3", "A1.txt"), type="SHA256")
# > "6Z4Yk" "C2PsK"
```

In order to find the absoluate file name, `Bob` will construct the filename from 
the base64 code.

``` r
library(rawdata2)
file_bob <- raw_file_by_id("6Z4Yk") 
file_a1 <- raw_file_by_id("C2PsK") 
```

He now provides the analysis code to `Alice` and she is able to run the same code; 
after telling the code the rough locations of all her `RAW` folders. 

``` r
library(rawdata2)
paths <- c("sharedFolder/RAW","alice/RAW")
file_bob <- raw_file_by_id("6Z4Yk") 
```

But, what if Bob forgot to give Alice the data file. She would see that 
`file_bob` is `NA` and is given a warning message that there is no such
file registered. She could tell Bob that she is looking for file "6Z4Yk" or
alternatively could request the `RAWdata register` from Alice. Bob would
generate the register and save it in the shared folder, so Alice will have
access in her shared folder.

``` r
# Bob shares the RAWdata register
raw_export_register("sharedFolder/RAW"")
```

Alice would then import the register with
``` r
# Alice imports all RAWdata registers in her paths
raw_import_register()
```

Now, she will know the name of the file and additional information, when she
tries ot find the filename with `raw_file_by_id()`.


