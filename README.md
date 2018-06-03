# cleanswe

Change variable names to snake case using the excellent janitor package. Split fields that have both a code and a name to seperate varables (only for SCB data). Change å, ä, ö to aa, ae, oe. This package is mostly for my own personal use.

For now you can separete code and names into two different varaiables if you have downloaded data from Statistics Sweden (SCB) for three types of variables: Industry (following the standards SNI92, SNI2002 and SNI2007), ockupation, and region.

Install package with:

devtools::install_github("christianlindell/cleanswe")


