#! /bin/sh

die()
{
    echo 1>&2 "$*"
    exit 69
}

usage()
{
    cat <<EOF

Usage:	camlviz --help
	camlviz [-v|-u] (ocamlc|ocamlopt|ocamldep) ARGUMENTS...

The arguments are passed to ocamlfind and the corresponding subcommand.  For
compiling source files, the "-c" option is required, so you can't compile and
link with the same command line.

Examples:
	camlviz ocamlopt -c test.viz -package foo
	camlviz ocamlopt -o test.native test.cmx -package foo -linkpkg

Wrapper Options:
    -v		Print the command invoked by this wrapper.
    -u		Don't filter error messages from ocmalc and ocamlopt.

EOF
}

[ x"$VIZ_SRCDIR" != x ] || die "Please run this script via a link to vizdev"
builddir=$VIZ_SRCDIR/_build


# Parse and Shift Wrapper Options
#
show_command=false
filter_errors="${CAMLVIZ_FILTER_ERRORS:-true}"
if [ -z "$VIZ_LOCATION_ORIGIN" ]; then
    if [ -n "$VIMRUNTIME" ]; then
	# I don't think Vim's errorformat supports 0-based column numbers.
	export VIZ_LOCATION_ORIGIN='1,1'
    fi
fi
if [ "$1" = "--help" ]; then
    usage
    exit 0
fi
while getopts ":vu" opt; do
    case $opt in
	v)
	    show_command=true
	    ;;
	u)
	    filter_errors=false
	    ;;
	'?')
	    usage
	    exit 64
	    ;;
    esac
done
shift `expr $OPTIND - 1`

# Get and Shift Subcommand
#
case "$1" in
    ocamlc)
	libext=.cma
	;;
    ocamlopt)
	libext=.cmxa
	;;
    ocamldep)
	;;
    cstubs|consts)
	subcommand="$1"
	shift
	[ x$show_command != xtrue ] || set -x
	$VIZ_SRCDIR/bin/camlvizpp --$subcommand -I $builddir/vsl "$@"
	exit $?
	;;
    *)
	usage
	[ "$1" = "--help" ] && exit 0 || exit 64
	;;
esac
command="$1"
shift

# Scan through the Subcommand Parameters
#
pp_opts=
oc_args=
seen_source=false
extra_includes="-I $builddir/vsl -I $builddir"
extra_packages="-package camomile"
mode=linkprog
while [ $# -gt 0 ]; do
    arg="$1"
    shift
    case "$arg" in
	-I)
	    pp_opts="$pp_opts -I $1"
	    oc_args="$oc_args -I $1"
	    shift
	    ;;
	-R)
	    pp_opts="$pp_opts -R $1"
	    shift
	    ;;
	*.vz|*.viz|*.ff)
	    seen_source=true
	    oc_args="$oc_args -impl $arg"
	    ;;
	-c)
	    mode=compile
	    oc_args="$oc_args $arg"
	    ;;
	-a)
	    mode=archive
	    oc_args="$oc_args $arg"
	    ;;
	--no-vsl)
	    extra_includes=
	    extra_packages=
	    pp_opts="$pp_opts --no-pervasive"
	    ;;
	*)
	    oc_args="$oc_args $arg"
	    ;;
    esac
done

# Error Filter and Verbosity
#
run() { echo "$*"; "$@"; }

filter_error()
{
    exitcode_file=`mktemp ${TMPDIR:-/tmp}/camlviz-subexitcode-XXXXXX`
    [ x$show_command != xtrue ] || echo "$*"
    ( "$@"; echo $? >$exitcode_file ) 2>&1 | $VIZ_SRCDIR/bin/camlvizerror
    exitcode=`cat $exitcode_file`
    rm -f $exitcode_file
    exit $exitcode
}
if [ x$filter_errors = xtrue ]; then
    pp_opts="$pp_opts --add-locations"
    wrap=filter_error
elif [ x$show_command = xtrue ]; then
    wrap="exec run"
else
    wrap=exec
fi

oc_args="$extra_includes $oc_args"
pp_command="$VIZ_SRCDIR/bin/camlvizpp $extra_includes $pp_opts"

# Determine and Run the Final Command
#
case "$command" in
    ocamlopt|ocamlc)
	case "$mode" in
	    compile)
		oc_args="-nopervasives $oc_args"
		$wrap ocamlfind "$command" -pp "$pp_command" $oc_args
		;;
	    archive)
		oc_args="-nopervasives $oc_args"
		$wrap ocamlfind "$command" $extra_packages $oc_args
		;;
	    linkprog)
		if [ $seen_source = true ]; then
		    die "The -c flag is required for compiling Viz sources with" \
			"ocamlopt."
		fi
		oc_args="$builddir/vsl$libext $oc_args -cclib -lvsl"
		$wrap ocamlfind "$command" $extra_packages $oc_args
		;;
	esac
	;;
    ocamldep)
	extra_packages=
	$wrap ocamlfind ocamldep -pp "$pp_command" $extra_includes $oc_args
	;;
esac
