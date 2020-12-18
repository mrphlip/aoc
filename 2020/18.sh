#!/bin/sh
(
	cat <<EOF
{-# OPTIONS_GHC -Wno-tabs #-}

infixl 5 +., *.
(+.) = (+)
(*.) = (*)

infixl 6 +..
infixl 5 *..
(+..) = (+)
(*..) = (*)

main = do
	print $ sum expressionsA
	print $ sum expressionsB

EOF
	echo "expressionsA = ["
	sed 's/^/	/g; s/$/,/g; s/\+/+./g; s/\*/*./g' < 18.txt
	echo "	0]"
	echo "expressionsB = ["
	sed 's/^/	/g; s/$/,/g; s/\+/+../g; s/\*/*../g' < 18.txt
	echo "	0]"
) > 18.hs
