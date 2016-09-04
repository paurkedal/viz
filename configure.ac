AC_INIT([Viz], [0.1], [https://github.com/paurkedal/viz/issues],
	[viz], [http://www.vizlang.org/])
m4_pattern_forbid([^VIZ_])

AC_CONFIG_SRCDIR([camlviz/ast_types.ml])
AC_CONFIG_AUX_DIR([tools])
AC_CONFIG_MACRO_DIR([aclocal])
AC_CONFIG_HEADER([libvsl/config.h])
AS_CASE([$srcdir], [.], [],
    [AC_MSG_ERROR([You need to configure in the source directory for now.  Since this project use ocamlbuild, most of the generated files will stored under the _build sub-directory.])])

AC_PROG_INSTALL
AC_PROG_CC_C_O
AC_PROG_CC_C99

VIZ_C_INLINE

VIZ_LIB_LLVM(
  [asmprinter bitwriter core executionengine interpreter ipo mcjit native],
  [], [AC_MSG_ERROR([The LLVM library is required.])])

AC_SEARCH_LIBS([setupterm], [ncurses curses])
AC_CHECK_FUNCS([isatty setupterm tigetstr])

AC_CONFIG_FILES([Makefile myocamlbuild.ml])
AC_OUTPUT
