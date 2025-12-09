# Package configuration (internals)

Internal machinery for pkgdepends configuration. See
[`current_config()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
for how this machinery can be used to define the configuration of a
package.

This is a standalone file that can be copied into another package
without changes.

## Usage

``` r
config
```

## Format

`config` is a list of functions with a closure. You can use it two ways.

For *one* set of configuration values in a package, include this in
`.onLoad()`:

    conf <- config$new("prefix")
    conf$add("myenrry", "string", "default-value")
    ...
    conf$lock()

For \_multiple \_ sets of configuration values (e.g. one per object),
include a function like this in the package:

    current_config <- function() {
      conf <- config$new("prefix")
      conf$add("myentry", "string", "default-value")
      ...
      conf$lock()
      conf
    }

and then call
[`current_config()`](https://r-lib.github.io/pkgdepends/dev/reference/pkg_config.md)
every time you need to create a new set of configuration values.

## Terminology

### Config entry (or entry)

A single configurable entity. A config entry has a name. This name is
always standardized by replacing dashes with underscores.

A config entry has a type: a string, a flag, or some user defined type.
It can also have a default value. Examples:

- The URL of an API, which can be a string.

- Path to a directory to use for caching, either a string, or NULL
  (`string_or_null` type).

- Number of processor cores to use, a positive integer (`count` type).

### Configuration (or config)

A set of configuration entries. `config$new()` returns a configuration.
A configuration can be extended by adding new entries to it, until it is
locked.

### R option (or option)

An option set with
[`base::options()`](https://rdrr.io/r/base/options.html) and queried
with [`base::getOption()`](https://rdrr.io/r/base/options.html).

### Configuration prefix

A prefix that is typically package specific, and it is used for all
config entries of a package, to avoid environment variable and option
name clashes between packages.

### Config entry type

It defines the allowed values of configuration entries, and also how a
string from an environment variable is translated to the value of the
entry. See the list of builtin types below. You can also define new
types.

## Built-in types

### `string`

String scalar, `NA` is not allowed.

### `count`

Non-negative integer, `NA` is not allowed.

### `flag`

Logical scalar, `NA` is not allowed. For environment variables the
`TRUE` values are: `yes`, `true`, `1` and `on`, and the `FALSE` values
are: `no`, `false`, `0`, `off`. (All are case insensitive.)

### `string_or_null`

String or `NULL`. `NA` is not allowed. In environment variables the
string `NULL` means an R `NULL` value.

### `character`

Character vector without `NA` values. In environment variables the
entries are separated by a semicolon.

### `custom`

Custom type. An `env_decode` function must be specified for each config
entry of this type, otherwise an error is throw if the corresponding
environment variable is set.

## `config$onload_hook()`: pretty-printing configuration

Call `config$onload_hook()` from the package's `.onLoad()` function if
you want to define S3 methods to print the package configuration nicer.

### Usage

    config$onload_hook()

## `config$new()`: Create a new configuration

Typically `new()` is called outside of the functions of the package, so
the `config` object is created at install time.

### Usage

    conf <- config$new(prefix = utils::packageName(parent.frame()))

### Arguments

- `prefix`: prefix of the config entry names. For environment variables
  it is converted to uppercase, and dots are converted to underscores.
  The default prefix is the name of the calling package. An underscore
  separator is used between the prefix and the entry name for
  environment variable names. A dot separator is used for R option
  names.

### Value

`new()` returns a `config` object, which is a list containing the
configuration data and functions (methods) to query and change it.

    conf <- config$new()

## Configuration methods

### `conf$add()`: add a new configuration entry

#### Usage

    conf$add(
      name,
      type = conf$types,
      default = NULL,
      check = type[1],
      env_decode = type[1]
    )

#### Arguments

- `name`: name of the entry.

- `type`: type of the entry, string.

- `default`: default value of config entry. If this is a function, then
  it is called to generate the default, at the time the config entry's
  value is queried.

- `check`: if it is a function, it is called to check the value of the
  entry. The function should return `TRUE` for successful and `FALSE`
  for unsuccessful checks. If `NULL`, then no check is performed. It can
  also be the name of a type, then the default check for that type is
  performed, if any.

- `env_decode`: if it is a function, then it is used to decode the value
  of the entry from an environment variable, i.e. a string. It can also
  be the name of a type, then the `env_decode()` function of that type
  is used for decoding.

#### Value

The configuration, invisibly.

### `conf$get()`: query the value of a configuration entry

#### Usage

    conf$get(name)

#### Arguments

- `name`: name of the entry.

#### Value

Value of the entry.

- If the entry was set via `conf$set()` or `conf$update()`, then that
  value is returned.

- Otherwise, if the entry is set via an R option (see
  [`base::getOption()`](https://rdrr.io/r/base/options.html)), then that
  value is returned. The config prefix is used to get the option name,
  with a dot separator. I.e. for an entry called 'foo', and prefix
  'pkg', the `pkg.foo` option is used. If the entry has a check
  function, that is called before returning.

- Otherwise, if the entry is set via an environment variable, then that
  value is returned. The config prefix is used to get the name of the
  environment variable. In addition, dots are replaced with underscores
  and the name is converted to uppercase. I.e. for an entry called
  'foo.bar', and prefix 'pkg', the 'PKG_FOO_BAR\` environment variable
  is used.

- Otherwise, if the entry has a default value, that is returned. The
  default value might be a function, in which case that function is
  called to produce a default value. If the entry has a check function,
  that is called before returning.

### `conf$set()`: set the value of a configuration entry

If you `$set()` a config entry, then the `value` used in `$set()` will
be returned by `$get()`, without consulting R options or environment
variables.

#### Usage

    conf$set(name, value)

#### Arguments

- `name`: name of the entry.

- `value`: value of the entry. If the entry has a check function, it is
  called here.

#### Value

The configuration, invisibly.

### `$conf$unset()`: unset a configuration entry

Note that this function does *not* unset R options and environment
variables. It merely removes a value that was assigned by `$set()` or
`$update()`.

#### Usage

    conf$unset(name)

#### Arguments

- `name`: name of the entry.

#### Value

The configuration, invisibly.

### `cond$update()`: update the values of configuration entries

You can use this method to
set()`and/or`\$unset()`multiple config entries.`NULL`values in`new\`
will unset the corresponding entry.

#### Usage

    conf$update(new)

#### Arguments

- `new`: named list, where names are config entry names and values are
  the corresponding config entry values. `NULL` values will unset the
  config entry. The list is processed sequentially, so for duplicates
  the latest values will be in effect. (But every value is checked with
  its check function, if any.)

#### Value

The configuration, invisibly.

### `conf$list()`: query the names of all config entries

Note that their order is non-deterministic currently.

#### Usage

    conf$list()

#### Value

Character vector.

### `conf$exists()`: check if a config entry exists

#### Usage

    conf$exists(name)

#### Arguments

- `name`: name of the entry.

#### Value

Logical scalar, `TRUE` if the entry exists in the configuration, `FALSE`
otherwise. Note that `TRUE` does not mean that the value of the entry
was set.

### `conf$lock()`: lock the configuration

If a configuration is locked, then no more entries can be added to it.

#### Usage

    conf$lock()

#### Value

The configuration, invisibly.

### `conf$fix()`: fix the value of a single configuration entry

The currently `$set()` value is fixed and cannot be changed any more.
You can fix an entry without setting its value, in which case R options
and environment variables can still be used to update it.

#### Usage

    conf$unset(name)

#### Arguments

- `name`: name of the entry.

#### Value

The configuration, invisibly.

### `conf$add_type()`: add a new config entry type to a configuration

After this the new type can be used in `conf$add()` calls.

#### Usage

    conf$add_type(type_name, check, env_decode)

#### Arguments

- `type_name`: type name.

- `check`: default check function of the type. Use `NULL` for no checks.

- `env_decode`: function to use to decode a value of the type from an
  environment variable (i.e. string).

#### Value

The configuration, invisibly.
