#!/bin/bash

exec scala "$0" -J-Xmx96m

!#

def sr[T](x: T) = new java.lang.ref.SoftReference(x)

sr(new Array[Double](1000000))
$intp.definedTerms

// LF scala -J-Xmx96m

