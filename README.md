# QRSE

An updated version of Guillaume Girol's Quantitative Robust Symbolic Execution (QRSE) as a Binsec plugin.

## How to build and install

### Nix

You can run either:
- `nix-build`, which will create a symlink to the nix store (`result`).
- `nix-shell`, which will open a shell with binsec and all necessary runtime dependencies (i.e., Popcon).

Getting dune plugins to work with Nix is tricky, `nix/dunePlugins.nix` implements a solution (can be used for other / multiple plugins too).

### Locally

First, you will need to install [Binsec](https://github.com/binsec/binsec) and Popcon.

Then, run:
```c
dune build && dune install
```

## Usage 

### Controlled inputs

QRSE defines a `controlled` keyword, equivalent to `nondet`, for marking inputs as controlled.

Examples:
```c
input<32> := controlled
@[esp + 4, 4] := controlled
@[esp + 8, 4] := controlled as input
```

### Objectives

QRSE extends the regular reachability objectives syntax.

```c
qr [<name>] [merge] [threshold t] reach ...
```

- `<name>` attaches the name "name" to the objective
- `merge` enables the merging of states reaching the objective (only for checking quantitative robustness)
- `threshold t` sets an acceptance threshold (float between 0 and 1), causing the objective to be considered fulfilled if quantitative robustness exceeds it

Examples:
```c
qr reach * address
qr <single> threshold 0.2 reach address such that ... then ...
qr <merged> merge threshold 0.2 reach address such that ... then ...
```

### Command line options

- `-qrse`: enable QRSE
- `-qrse-term`: terminate analysis when all QRSE objectives are fulfilled
- `-qrse-portfolio`: set up the quantitative robustness solver portfolio (see description from `binsec --help`)
- `-qrse-rr`: sets how robust reachability should be used:
    * precheck: check before quantitative robustness (default behaviour)
    * only: only check robust reachability
    * off: don't check robust reachability

Example:
```c
binsec -sse -sse-script test/crackme.ini -qrse test/magic
```

## Publication

- [Quantitative Robustness for Vulnerability Assessment](https://doi.org/10.1145/3656407) (PLDI 2024)
