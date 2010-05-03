#!/bin/bash

###
### A configuration script for Javalib
###
###     Provide a "local" configuration option
###     Detect ocamlfind
###     Determine whether camlzip and ptrees need to be make'd
###     Check the presence of unix, str, extlib
###     Check for recode
###     Set the debug flag
###     Select the camlp4o executable
###     Infer the destdir value from the localdest flag
###     Infer the ocamlopt value from the debug flag
###     Write the variables to the Makefile.config file
###     
###     
### Copyright (c)2010 Florent Kirchner
### 
### This program is free software: you can redistribute it and/or
### modify it under the terms of the GNU Lesser General Public License
### as published by the Free Software Foundation, either version 3 of
### the License, or (at your option) any later version.
### 
### This program is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### Lesser General Public License for more details.
### 
### You should have received a copy of the GNU Lesser General Public
### License along with this program.  If not, see 
### <http://www.gnu.org/licenses/>.
### 
### This file: began on         march-18-2010,
###            last updated on  may-2-2010.
###


# The directory for local installations. Leave it empty if the install goes
# global.
LOCALDEST=
# The destdir argument to "ocamlfind install" (depends on LOCALDEST)
DESTDIR=
# The ocamlpath variable for the compiler to locate the locally-installed
# packages (depends on LOCALDEST)
OCAMLPATH=
# The packages that need to be made in addition to Savalib / Sawja
MAKEDEP=
# The path to ocamlfind
FINDER=`which ocamlfind`
# The path to recode (used to fix accents in the documentation)
RECODEBIN=`which recode`
# The debug flag
DEBUG=yes
# The shared option flag
SHARED=
# The ocamlopt flags (depends on DEBUG)
OPT_FLAGS=
# The camlp4o pretty-printer
PP=

# The following variables are constants
FLAGS="-g -w Ae -annot"


# Differentiated error numbers make for easier bug hunting. Hopefully we won't
# have to use them.
E_MAKERERROR=83
E_SCRIPTERROR=84

#
# The msg recursive function takes care of the pretty-printing.
# It uses "fmt" to stick to 75 characters columns.
#
function msg() 
{
  if [ $# -eq 2 ]; then
    if [ $1 = "err" ]; then
      echo ""
      echo "! configure error: $2." | fmt >&2
      exit $E_MAKERERROR
    elif [ $1 = "inf" ]; then
      echo "* $2." | fmt
      return 0
    fi
  elif [ $# -eq 3 ]; then
    if [ $1 = "ser" ]; then
      echo ""
      echo "! script error ($2): $3. Please file a bug." | fmt >&2
      exit $E_SCRIPTERROR
    fi
    msg "ser" "msg" "unexpected message type"
  else
    msg "ser" "msg" "incorrect number of message arguments"
  fi
}


#
# The push function takes an atom and a variable that contains a list, and
# performs the corresponding push.
#
# For instance, if LIST=bar\ baz, then after 'push foo LIST', LIST=foo\ bar\ baz.
#
function push ()
{
  if [ $# -ne 2 ]; then
    msg "ser" "push" "incorrect number of message arguments"
  fi
  atom=$1
  list=$2
  if [ -z "${!list}" ]; then
    eval $list=$atom
  else
    eval $list="$atom\ ${!list}"
  fi
  return 0
}


#
# Macro function to print a usage message.
#
function print_usage()
{
  echo -e "
Javalib configure.sh
Usage: `basename $0` [-l [PATH|default]] [-d [yes|no|prof]] [-h]
Options:
  -l PATH \t Perform a local installation at PATH.
  -l default \t Perform a local installation in the default directory.
  -d FLAG \t Use the debug flag when compiling.
  -s  \t\t Complile a dynamically loadable plugin (cmxs).
  -h  \t\t Print this message and exit."
}


#
# The option parsing function. Uses getopts, a bash built-in function.
#
while getopts ":d:l:h" opt
do
  case $opt in 
    h   ) print_usage
          exit 0;;
    d   ) DEBUG=$OPTARG
          msg "inf" "Debug flag set to '$DEBUG'";;
    l   ) case "$OPTARG" in
            default)    tmpdest="`pwd`/lib";;
            *)          tmpdest="$OPTARG";;
          esac
          LOCALDEST=`(cd $tmpdest && pwd) 2>/dev/null`
          if [ $? != 0 ]; then
            msg "inf" "Local installation, but directory $tmpdest was not found"
            echo -n "  Creating directory... "
            mkdir -p $tmpdest/stublibs
            echo "done."
            LOCALDEST=`(cd $tmpdest && pwd)` # This one can't fail!
          fi
          msg "inf" "Local installation, at $LOCALDEST"
          # For the rest of this configure, set OCAMLPATH to $LOCALDEST
          # NB: only children of this script are in the scope of 'export'.
          export OCAMLPATH=$LOCALDEST;;
    s   ) SHARED="javalib.cmxs"
           msg "inf" "Plugin version of javalib will be generated at compilation (ocamlopt -shared)";;
    *   ) msg "err" "unrecognized option '$OPTARG'. Type '`basename $0` -h' to list available options";;
  esac
done

shift $(($OPTIND - 1))


#
# Check Ocamlfind, print the global installation directory if relevant.
#
if [ $FINDER ]; then
  msg "inf" "Ocamlfind found at $FINDER"
else
  msg "err" "ocamlfind not found. Ocamlfind is part of the Findlib package management library, and is required to install Javalib/Sawja.

Use your system's software packaging tools to install Findlib, or download it from:
http://www.camlcity.org/archive/programming/findlib.html"
fi

if [ -z $LOCALDEST ]; then 
  msg "inf" "System-wide installation, in `$FINDER printconf destdir`" 
fi
        
#
# Check Camlzip, Ptrees, and Extlib. Set them to compile if necessary.
#
for pkg in camlzip ptrees extlib; do
location=`$FINDER query $pkg 2>/dev/null`
if [ $location ]; then
  msg "inf" "Package $pkg found at $location"
else 
  msg "inf" "Package $pkg not found, will need to be compiled"
  push "$pkg" MAKEDEP
fi
done


#
# Check Unix, and Str
#
for pkg in unix str; do
location=`$FINDER query $pkg 2> /dev/null`
if [ $location ]; then
  msg "inf" "Package $pkg found at $location"
else 
  msg "err" "Package $pkg not found"
fi
done


#
# Check Recode
#
if [ $RECODEBIN ]; then
  msg "inf" "Recode found at $RECODEBIN"
  RECODE="-pp \"$RECODEBIN UTF-8..Latin-1 <\""  
else
  msg "inf" "Recode not found, proceeding anyway"
fi


#
# Check camlp4o
#
cp4=`which camlp4o.opt`
if [ -z "$cp4" ]; then
  msg "inf" "No camlp4o.opt executable found: trying vanilla camlp4o"
  cp4=`which camlp4o`
  if [ -z "$cp4" ]; then
    msg "err" "No camlp4o executable found"
  fi
fi
msg "inf" "Camlp4o found at $cp4"
PP="-pp $cp4"


#
# Infer the value of the DESTDIR and OPT_FLAGS variables
#
if [ -n "$LOCALDEST" ]; then
  DESTDIR="-destdir $LOCALDEST"
fi

case $DEBUG in
  yes)  OPT_FLAGS="-g";;
  prof) OPT_FLAGS="-g -p -noassert -ccopt -O3";;
  no)   OPT_FLAGS="-g -noassert -ccopt -O3";;
  *)    msg "err" "debug option $DEBUG is not recognized"
esac


#
# Output variables to the Makefile.config file
# TODO: move the Makefile.config.example out of the way (in src?)
#
makeconfig=`pwd`/Makefile.config
makeconfigtemplate=`pwd`/Makefile.config.example
# Copy the Makefile.config from template and add a warning header
msg "inf" "Writing $makeconfig"
echo "  Creating from $makeconfigtemplate."
# Header
echo "# WARNING: this file was automatically generated by `basename $0`." > $makeconfig
echo "# Edit at your own risk." >> $makeconfig
echo -n "  ."
# Constants
echo "" >> $makeconfig
echo "# Configuration constants" >> $makeconfig
for var in FLAGS; do
  echo "$var=${!var}" >> $makeconfig
done
echo -n "."
# Configuration variables
echo "" >> $makeconfig
echo "# Variables detected at configure-time" >> $makeconfig
for var in OPT_FLAGS LOCALDEST MAKEDEP FINDER RECODE DEBUG SHARED PP; do
  echo "$var=${!var}" >> $makeconfig
done
echo -n "."
# The rest from template
echo "" >> $makeconfig
echo "# Variables from template at: " >> $makeconfig
echo "# $makeconfigtemplate" >> $makeconfig
cat $makeconfigtemplate >> $makeconfig
echo -n "."
echo " done."


#
# Tell the user what to do next:
# - if MAKEDEP is non-empty, then compile and install the dependencies.
# - else compile and install Javalib 
#
if [ "$MAKEDEP" ]; then
  echo ""
  echo "WHAT'S NEXT: the following packages need to be compiled and installed:" | fmt
  echo "    $MAKEDEP"
  echo "In short, you will need to execute the following commands:" | fmt
  for dep in $MAKEDEP; do
    # Use sudo only if it's a nonlocal installation.
    if [ "$LOCALDEST" ]; then
      echo "    make $dep && make install$dep"
    else
      echo "    make $dep && sudo make install$dep"
    fi
  done
  echo "Once the packages have been installed, rerun `basename $0` to update the Javalib Makefiles." | fmt
else
  echo ""
  echo "WHAT'S NEXT: all dependencies are satisfied. Compile and install Javalib with the following commands:" | fmt
  if [ "$LOCALDEST" ]; then
    echo "    make && make install"
  else
    echo "    make && sudo make install"
  fi
fi
echo ""
echo "More details can be found in the installation documentation (INSTALL or http://javalib.gforge.inria.fr/javalib-doc.html)." | fmt

exit 0
