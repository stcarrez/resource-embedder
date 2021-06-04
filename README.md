# Advanced Resource Embedder

[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Bionic-Resource-Embedder.svg)](http://jenkins.vacs.fr/job/Bionic-Resource-Embedder/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Bionic-Resource-Embedder.svg)](http://jenkins.vacs.fr/job/Bionic-Resource-Embedder/)
[![codecov](https://codecov.io/gh/stcarrez/ada-keystore/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/resource-embedder)
[![Documentation Status](https://readthedocs.org/projects/resource-embedder/badge/?version=latest)](https://resource-embedder.readthedocs.io/en/latest/?badge=latest)
[![Download](https://img.shields.io/badge/download-1.0.0-brightgreen.svg)](http://download.vacs.fr/resource-embedder/resource-embedder-1.0.0.tar.gz)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/resource-embedder/1.0.0.svg)
![semver](https://img.shields.io/badge/semver-2.0.0-blue.svg?cacheSeconds=2592000)

# TL;DR

The resource embedder allows to embed files in binaries by producing C, Ada or Go source
files that contain the original files.

For example to generate a `my-config.h` and `my-config.c` files with the content of the
`config` directory to include files matching the `*.conf` pattern in any
directory:

```
are --lang=c -o src --resource=my-config --name-access --fileset='**/*.conf' config
```

And if you prefer to generate a `config.ads` and `config.adb` Ada package with the
resources, you may use:

```
are --lang=Ada -o src --resource=config --name-access --fileset='**/*.conf' config
```

## Version 1.0.0  - June 2021
- First version of the resource embedder
- Add support for C, Ada and Go

[List all versions](https://github.com/stcarrez/resource-embedder/blob/master/NEWS.md)

# Overview

Incorporating files in a binary program can sometimes be a challenge.
The `Advance Resource Embedder` is a flexible tool that collects files such as
documentation, images, scripts and others.  It is able to apply some
transformation on the collected files:

* it can run a Javascript minifier such as `closure`,
* it can compress CSS files by running `yui-compressor`,
* it can compress files by running `gzip` or another compression tool,

Once these transformations are executed, it invokes a target generator
to produce a source file either in C, Ada or Go language.  The generated
source file can then be used in the final program and taken into account
during the compilation process of that program.  At the end, the binary
will contain the embedded files with their optional transformations.

![Resource Embedder Overview](https://github.com/stcarrez/resource-embedder/raw/master/docs/images/resource-embedder.png)


# Building ARE

To build the ARE you will need the GNAT Ada compiler, either
the FSF version available in Debian, FreeBSD systems, NetBSD or the
AdaCore GNAT Community 2021 edition.

## Development Host Installation

### Ubuntu

Install the following packages:
```
sudo apt-get install -y make gnat-7 gprbuild git
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
   git clone --recursive https://github.com/stcarrez/resource-embedder.git
   cd resource-embedder
```

## Configuration

To configure the resource embedder, use the following command:
```
   ./configure
```

## Build

Then, build the application:
```
   make
```

And install it:
```
   make install
```

# Documents

* [Resource Embedder Guide](https://resource-embedder.readthedocs.io/en/latest/) [PDF](https://github.com/stcarrez/resource-embedder/blob/master/docs/are-book.pdf)
* Man page: [are (1)](https://github.com/stcarrez/resource-embedder/blob/master/docs/are.md)

