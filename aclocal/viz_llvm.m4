# Copyright (C) 2011  Petter Urkedal
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

AC_DEFUN([VIZ_LIB_LLVM],
  [ AC_PATH_PROGS([LLVM_CONFIG], [llvm-config-2.9 llvm-config], [false])
    if test ${LLVM_CONFIG} != false; then
	LLVM_VERSION=`${LLVM_CONFIG} --version`
	LLVM_CPPFLAGS=`${LLVM_CONFIG} --cppflags | sed s/-DNDEBUG//g`
	LLVM_CFLAGS=`${LLVM_CONFIG} --cflags`
	LLVM_CXXFLAGS=`${LLVM_CONFIG} --cxxflags`
	LLVM_LDFLAGS=`${LLVM_CONFIG} --ldflags`
	LLVM_LIBS=`${LLVM_CONFIG} --libs $1`
	have_llvm=true
	$2
    else
	have_llvm=false
	$3
    fi
    AC_SUBST([LLVM_VERSION])
    AC_SUBST([LLVM_CPPFLAGS])
    AC_SUBST([LLVM_CFLAGS])
    AC_SUBST([LLVM_CXXFLAGS])
    AC_SUBST([LLVM_LDFLAGS])
    AC_SUBST([LLVM_LIBS])
    AC_DEFINE([HAVE_LLVM], [1], [Define if LLVM is present.])
  ])
