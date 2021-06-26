# Advanced Resource Embedder

[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Bionic-Resource-Embedder.svg)](http://jenkins.vacs.fr/job/Bionic-Resource-Embedder/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Bionic-Resource-Embedder.svg)](http://jenkins.vacs.fr/job/Bionic-Resource-Embedder/)
[![codecov](https://codecov.io/gh/stcarrez/resource-embedder/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/resource-embedder)
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

Complex resource integrations are best described with and XML and are generated with:

```
are --lang=Ada -o src --rule=package.xml --name-access .
```

## Version 1.1.0  - Developemnt
- Add support to emit Ada String types for embedded content
- Add support to represent content as an array of lines
- New example to show the new 'lines' resource format

## Version 1.0.0  - June 2021
- First version of the resource embedder
- Add support for C, Ada and Go

[List all versions](https://github.com/stcarrez/resource-embedder/blob/master/NEWS.md)

# Overview

Incorporating files in a binary program can sometimes be a challenge.
The `Advance Resource Embedder` is a flexible tool that collects files such as
documentation, images, scripts, configuration files and generates a source
code that contains these files.  It is able to apply some
transformations on the collected files:

* it can run a Javascript minifier such as `closure`,
* it can compress CSS files by running `yui-compressor`,
* it can compress files by running `gzip` or another compression tool,

Once these transformations are executed, it invokes a target generator
to produce a source file either in C, Ada or Go language.  The generated
source file can then be used in the final program and taken into account
during the compilation process of that program.  At the end, the binary
will contain the embedded files with their optional transformations.

The process to use ARE is simple:

* You describe the resources that you want to embed.
  The description is either made on command line arguments or by writing an XML file.
  The XML description gives more flexibility as it allows to define a transformation rule that
  must be executed on the original file before being embedded.  This allows to minify a Javascript
  or CSS file, compress some files and even encrypt a file before its integration.
* You run the ARE command with your target language and rule description and you give the tool
  a list of directories that must be scanned to identify the files that must be collected.
  The ARE tool scan the directories according to the patterns that you have given either on
  the command line or in the XML rule description.  After identifying the files, the tool applies
  the rules and execute the transformations.
  The ARE tool then invokes the target language generator that writes one or several files depending
  on the list of resources.
* Once the files are generated, you use them in your program and add them in your build process
  as they are now part of your sources.  After building your program, it now embeds the
  resource files that were collected and optionally transformed.

![Resource Embedder Overview](https://github.com/stcarrez/resource-embedder/raw/master/docs/images/resource-embedder.png)

Note:

The generated code is not coverred by any license but because it integrates the resource file,
you have to consider the license of these resource file.


## Examples

This first set of example shows how to you can embed configuration files in a C, Ada or Go program.
The Advance Resource Embedder simply puts the configuration files in an array of bytes that can easily
be retrieved by a generated function.

* [Embedding configuration files in C](https://github.com/stcarrez/resource-embedder/tree/master/examples/c-config)
* [Embedding configuration files in Ada](https://github.com/stcarrez/resource-embedder/tree/master/examples/ada-config)
* [Embedding configuration files in Go](https://github.com/stcarrez/resource-embedder/tree/master/examples/go-config)

A second set of example is more advanced by the use of an XML file that describes what must be embedded with
the transformations that must be made.  It creates two distinct resource sets `help` and `man`.  The `help` resource
set is composed of a set of fixed documentation files provided in the example.  The `man` resource set is created
by running the `man` Unix command on various names to embed the man page of `ls`, `pwd` and `sh`.

* [Embedding help and documentation in C](https://github.com/stcarrez/resource-embedder/tree/master/examples/c-help)
* [Embedding help and documentation in Ada](https://github.com/stcarrez/resource-embedder/tree/master/examples/ada-help)
* [Embedding help and documentation in Go](https://github.com/stcarrez/resource-embedder/tree/master/examples/go-help)

More specific examples show how to make specific transformations on the files before integrating them:

* [Embedding web files in C](https://github.com/stcarrez/resource-embedder/tree/master/examples/c-web)
* [Embedding merged properties in Ada](https://github.com/stcarrez/resource-embedder/tree/master/examples/ada-bundles)
* [Embedding SQL scripts in Ada and mapping them in array of String](https://github.com/stcarrez/resource-embedder/tree/master/examples/ada-lines)


# Building ARE

To build the ARE you will need the GNAT Ada compiler, either
the FSF version available in Debian, FreeBSD systems, NetBSD or the
AdaCore GNAT Community 2021 edition.  Because there exists different versions of
the compiler, you may have to adapt some of the commands proposed below for
the installation.

## Development Host Installation

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
git clone --recursive https://github.com/stcarrez/resource-embedder.git
cd resource-embedder
```

## Configuration (optional)

Running the `configure` script is optional
To configure the resource embedder, use the following command:
```
./configure
```

## Build

To build the resource embedder, run the command:
```
make
```

If you have a recent GNAT compiler (gcc >= 8) you may build with:

```
make GNAT_SWITCH=HAS_CALLBACK
```

and this allows to use multiple `--resource=` and `--fileset=` options.

And install it:

```
make install
```

# Debian Packages for x86_64

You can install ARE by using the Debian 10 and Ubuntu 20.04 or 18.04 packages.
First, setup to accept the signed packages:

```
wget -O - https://apt.vacs.fr/apt.vacs.fr.gpg.key | sudo apt-key add -
```

and choose one of the `echo` command according to your Linux distribution:

Ubuntu 20.04
```
echo "deb https://apt.vacs.fr/ubuntu-focal focal main" | sudo tee -a /etc/apt/sources.list.d/vacs.list
```

Ubuntu 18.04
```
echo "deb https://apt.vacs.fr/ubuntu-bionic bionic main" | sudo tee -a /etc/apt/sources.list.d/vacs.list
```

Debian 10
```
echo "deb https://apt.vacs.fr/debian-buster buster main" | sudo tee -a /etc/apt/sources.list.d/vacs.list
```

Then, launch the apt update command:

```
sudo apt-get update
```

and install the tool using:

```
sudo apt-get install -y are
```


# Documents

* [Resource Embedder Guide](https://resource-embedder.readthedocs.io/en/latest/) [PDF](https://github.com/stcarrez/resource-embedder/blob/master/docs/are-book.pdf)
* Man page: [are (1)](https://github.com/stcarrez/resource-embedder/blob/master/docs/are.md)

