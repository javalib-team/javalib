#!/bin/bash

###
### A configuration script for Javalib / Sawja
###
###     Detect ocamlfind
###     Check the presence of unix, str, extlib
###     Determine whether camlzip and ptrees need to be make'd
###     Check for recode
###     Provide a "local" configuration option
###     
###     
### Written by Florent Kirchner
### Postdoctoral Researcher, INRIA Rennes - Bretagne Atlantique, France
### Contact: florent.kirchner@lix.polytechnique.fr, www.lix.polytechnique.fr/~fkirchner/
### 
### This file: began on         march-18-2010,
###            last updated on  .
###

#
# The msg function takes care of the pretty-printing.
# It uses "fmt" to stick to 75 characters columns.
#
function msg() 
{
  if [ $# -ne 2 ]; then
    echo "script error (msg): incorrect number of message arguments. Please file a bug." >&2
    exit 1
  elif [ $1 = "err" ]; then
    echo ""
    echo "configure error: $2" | fmt >&2
    return 1
  elif [ $1 = "inf" ]; then
    echo "* $2" | fmt
    return 0
  else
    echo "script error: unexpected message type. Please file a bug." >&2
    exit 1
  fi
}

#
# The push function takes an atom and a variable that contains a list, and
# performs the corresponding push.
#
# For instance, if LIST=bar:baz, then after 'push foo LIST', LIST=foo:bar:baz.
#
function push ()
{
  if [ $# -ne 2 ]; then
    echo "script error (push): incorrect number of message arguments. Please file a bug." >&2
    exit 1
  fi
  atom=$1
  list=$2
  if [ -z ${!list} ]; then
    eval $list=$atom
  else
    eval $list=$atom:${!list}
  fi
  return 0
}

#
# Check Ocamlfind
#
FINDER=`which ocamlfind`
if [ $FINDER ]; then
  msg "inf" "Ocamlfind found at $FINDER."
else
  msg "err" "Ocamlfind not found. Ocamlfind is part of the Findlib package management library, and is required to install Javalib/Sawja.

Use your system's software packaging tools to install Findlib, or download it from:
http://www.camlcity.org/archive/programming/findlib.html."
fi

#
# Check Camlzip and Ptrees
#
MAKEDEP=
for pkg in camlzip ptrees; do
location=`ocamlfind query $pkg 2> /dev/null`
if [ $location ]; then
  msg "inf" "Package $pkg found at $location."
else 
  msg "inf" "Package $pkg not found, will be compiled."
  push $pkg MAKEDEP
fi
done

#
# Check Unix, Str and Extlib
#
for pkg in unix str extlib; do
location=`ocamlfind query $pkg 2> /dev/null`
if [ $location ]; then
  msg "inf" "Package $pkg found at $location."
else 
  msg "err" "Package $pkg not found."
fi
done

#
# Check Recode
#
REC=`which recode`
if [ $REC ]; then
  msg "inf" "Recode found at $REC."
else
  msg "inf" "Recode not found, proceeding anyway."
fi


