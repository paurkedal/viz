AC_DEFUN([VIZ_C_INLINE], [
    AC_MSG_CHECKING([semantics of inline keyword])
    AC_REQUIRE([AC_PROG_CC_C99])
    AC_LANG_PUSH([C])

    AC_COMPILE_IFELSE([
	AC_LANG_SOURCE(
	  [[
	    extern inline int einline_func(int i) { return i; }
	    inline int inline_func(int i) { return i; }
	  ]])
      ], [
	mv -f conftest.$OBJEXT conftest2.$OBJEXT
	saved_LIBS="$LIBS"
	LIBS="$LIBS conftest2.$OBJEXT"
	AC_LINK_IFELSE(
	    [AC_LANG_SOURCE(
		[[
		    int einline_func(int);
		    int main() { return einline_func(0); }
		]])],
	    [AC_MSG_RESULT([C99])
	     AC_DEFINE([HAVE_C99_INLINE], [1],
		[Define if "extern inline" emits a definition, as in C99.])],
	    [AC_LINK_IFELSE(
		[AC_LANG_SOURCE(
		    [[
			int inline_func(int);
			int main() { return inline_func(0); }
		    ]])],
		[AC_MSG_RESULT([GNU89])
		 AC_DEFINE([HAVE_GNU89_INLINE], [1],
		    [Define if "inline" emits a definition, as i GNU89.])],
		[AC_MSG_RESULT([unknown])])
	    ])
	LIBS="$saved_LIBS"
	rm -f conftest2.$OBJEXT
      ],
      [AC_MSG_RESULT([missing])])

    AC_LANG_POP([C])
  ])
