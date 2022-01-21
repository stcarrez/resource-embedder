# Installation

This chapter explains how to build and install the tool.

## Before Building

To build `Advanced Resource Embedder` you will need the GNAT Ada compiler, either
the FSF version available in Debian, FreeBSD systems NetBSD or the
AdaCore GNAT Community 2021 edition.  Because there exists different versions of
the compiler, you may have to adapt some of the commands proposed below for
the installation.

### Ubuntu 18.04

Install the following packages:
```
sudo apt install -y make git
sudo apt install -y gnat-7 gprbuild libxmlada-dom7-dev
```

### Ubuntu 20.04

Install the following packages:
```
sudo apt install -y make git
sudo apt install -y gnat-9 gprbuild libxmlada-dom9-dev
```

### FreeBSD 12

Install the following packages:

```
pkg install gmake gcc6-aux-20180516_1,1 gprbuild-20160609_1 git
```

### Windows

Get the Ada compiler from [AdaCore Download](https://www.adacore.com/download)
site and install.

Install the following packages:

```
pacman -S git
pacman -S make
pacman -S base-devel --needed
```

## Getting the sources

The project uses a sub-module to help you in the integration and build
process.  You should checkout the project with the following commands:

```
git clone --recursive https://gitlab.com/stcarrez/resource-embedder.git
cd resource-embedder
```

## Configuration (optional)

Running the `configure` script is optional and is useful if the pre-defined
default configuration must be changed.

The `configure` script is used to detect the build environment,
setup specific build configuration.
If some component is missing, the
`configure` script will report an error or it will disable the feature.
The `configure` script provides several standard options
and you may use:

  * `--enable-distrib` to build for a distribution and strip symbols,
  * `--disable-distrib` to build with debugging support,
  * `--enable-coverage` to build with code coverage support (`-fprofile-arcs -ftest-coverage`),
  * `--prefix=DIR` to control the installation directory,
  * `--help` to get a detailed list of supported options.

In most cases you will configure with the following command:
```
./configure
```

## Build

After configuration is successful, you can build the library by running:
```
make
```

If you have a recent GNAT compiler (gcc >= 8) you may build with:

```
make GNAT_SWITCH=HAS_CALLBACK
```

and this allows to use multiple `--resource=` and `--fileset=` options.

After building, it is good practice to run the unit tests before installing
the library.  The unit tests are built and executed using:
```
make test
```

## Installation
The installation is done by running the `install` target:

```
make install
```

If you want to install on a specific place, you can change the `prefix`
and indicate the installation direction as follows:

```
make install prefix=/opt
```

