dnl Autoconf configure script for are

dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/are.ads)

# Current release settings
ARE_MAJOR_VERSION=1
ARE_MINOR_VERSION=0
ARE_MICRO_VERSION=0
ARE_VERSION=$ARE_MAJOR_VERSION.$ARE_MINOR_VERSION.$ARE_MICRO_VERSION

# Set the version number of the project
AC_SUBST(ARE_VERSION)
AC_SUBST(ARE_MAJOR_VERSION)
AC_SUBST(ARE_MINOR_VERSION)
AC_SUBST(ARE_MICRO_VERSION)

AC_CHECK_PROG(HAVE_PANDOC,pandoc,yes)
AC_CHECK_PROG(HAVE_DYNAMO,dynamo,yes)

AM_GNAT_LIBRARY_PROJECT(are)

AC_OUTPUT(
Makefile.conf
)
