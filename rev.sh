#!/bin/sh

if test -e .svn ;then
	rev=`svn info|grep Revision|awk '{print $2}'`
	echo ${rev}>rev
fi

#
# sed -i is not portable.
# This seems safer than using sed. -db
#
ed wsjt.py<<EOF
	1,$ s/^Version.*/Version="7.03 r${rev}"
	wq
EOF
	
ed setup.py<<EOF
	1,$ s/^version.*/version = "WSJT Version 7.03 r${rev}, by K1JT"
	wq
EOF
