# Installation

This chapter explains how to build and install the tool.

## Before Building

To build `Advanced Resource Embedder` you will need the GNAT Ada compiler, either
the FSF version available in Debian, FreeBSD systems NetBSD.

### Ubuntu 22.04

Install the following packages:

```
sudo apt install -y make git alire
sudo apt install -y gnat gprbuild
```

### FreeBSD 14

Install the following packages:

```
pkg install gmake gnat13 gprbuild git alire
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

You should checkout the project with the following commands:

```
git clone https://gitlab.com/stcarrez/resource-embedder.git
cd resource-embedder
```

## Using Alire

The `Advanced Resource Embedder` is available as an Alire crate to simplify the installation
and setup your project.  Run the following commands to setup your project to use the library:

```
alr index --update-all
alr with are
```

### Build

After configuration is successful, you can build the library by running:

```
make
```

After building, it is good practice to run the unit tests before installing
the library.  The unit tests are built and executed using:
```
make test
```

### Installation

The installation is done by running the `install` target:

```
make install
```

If you want to install on a specific place, you can change the `prefix`
and indicate the installation direction as follows:

```
make install prefix=/opt
```

