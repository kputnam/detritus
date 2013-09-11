#!/bin/sh
ghc -O2 \
  #ddump-parse \    # parser (HsSyn RdrName)
  #ddump-rn \       # renamer (HsSyn Name)
  #ddump-tc \       # type checker (HsSyn Id)
  #ddump-ds \       # desugared core
  -ddump-simpl \    # simplified core
  #ddump-stranal \  # strictness analyzed
  #ddump-cse \      # common subexpression elim
  -dsuppress-idinfo \
  -dsuppress-coercions \
  -dsuppress-type-applications \
  -dsuppress-uniques \
  -dsuppress-module-prefixes \
  $1
