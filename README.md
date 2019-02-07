# minimalPairR
A simple R-script that calculates minimal pairs and minimal sets from a wordlist

## Use case

### The user

This project was built for linguists and language users engaged in language documentation. The use case envisioned assumes:

* Resources on the language are relatively limited (so that processing a corpus that one personally manages is sufficient)
* Familiarity with R and RStudio, limited coding experience otherwise
* A workflow that isn't based on proprietary data formats (so that handling data in tabular formats is a common activity)
* This output is desired primarily for description (so that the primary interest is discovering minimal pairs, not necessarily further computational analyses of those minimal pairs)

### The code

In order to be user-friendly to the target audience, this project is based on an R-script, rather than a function, to make it easier to understand and tweak for beginning coders. The script is designed so that it can be run first, and then understood later.

The main concern for the first time user should be that all the necessary packages are installed (see code section 'REQUIRED PACKAGES'). Other than that, the user can choose to hard-code input values (in the 'USER INPUT SECTION') or be prompted for them at runtime.

Finally, the use of an R-script is to encourage the user to tweak the code as necessary for their personal needs (and even contribute improvements to the repository!)

## Prerequisites

R (optionally with RStudio), in order to run an R-script

A wordlist as a column in a .csv, .ods, or .xlsx file

## Outputs

Two .csv files in the same directory as the input file.

1. Unique minimal pairs and their distinctions
2. Minimal sets for each item in the wordlist that has any minimal pairs


## Authors

* **Kavon Hooshiar** - *Initial work*

See also the list of [contributors](https://github.com/kavonjon/minimalPairR/graphs/contributors) who participated in this project.

## License

This project is licensed under the GNU General Public License (GPL) - see the [LICENSE.md](LICENSE.md) file for details


