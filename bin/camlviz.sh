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
    *)
	usage
	[ "$1" = "--help" ] && exit 0 || exit 64
	;;
esac
command="$1"
shift

# Scan through the Subcommand Parameters
#
grab_arg=false
pp_opts=
oc_args=
have_c_flag=false
seen_source=false
extra_includes="-I $builddir/vsl -I $builddir"
extra_packages="-package camomile"
camlviz_error_pipe=
for arg in "$@"; do
    if [ x"$grab_arg" = xtrue ]; then
	pp_opts="$pp_opts $arg"
	oc_args="$oc_args $arg"
	grab_arg=false
	continue
    fi
    case "$arg" in
	-I)
	    grab_arg=true
	    pp_opts="$pp_opts -I"
	    oc_args="$oc_args -I"
	    ;;
	*.vz|*.viz|*.ff)
	    seen_source=true
	    oc_args="$oc_args -impl $arg"
	    ;;
	-c)
	    have_c_flag=true
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

# Determine and Run the Final Command
#
builddir=$VIZ_SRCDIR/_build
case "$command" in
    ocamlopt|ocamlc)
	if [ $have_c_flag = true ]; then
	    oc_args="-nopervasives $oc_args"
	elif [ $seen_source = true ]; then
	    die "The -c flag is required for compiling Viz sources with" \
		"ocamlopt."
	fi
	oc_args="$builddir/vsl$libext $oc_args"
	;;
    ocamldep)
	extra_packages=
	;;
esac

filter_error()
{
    exitcode_file=`mktemp ${TMPDIR:-/tmp}/camlviz-subexitcode-XXXXXX`
    ( "$@"; echo $? >$exitcode_file ) 2>&1 | $VIZ_SRCDIR/bin/camlvizerror
    exitcode=`cat $exitcode_file`
    rm -f $exitcode_file
    exit $exitcode
}

if [ x$filter_errors = xtrue ]; then
    pp_opts="$pp_opts --add-locations"
    wrap=filter_error
else
    wrap=exec
fi
pp_command="$VIZ_SRCDIR/bin/camlvizpp $extra_includes $pp_opts"
[ x$show_command != xtrue ] || set -x
$wrap ocamlfind "$command" -pp "$pp_command" \
	$extra_includes $extra_packages $oc_args $camlviz_error_pipe
