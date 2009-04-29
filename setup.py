#!/usr/bin/env python

version="WSJT Version 7.03, by K1JT"

from distutils.core import setup
from distutils.file_util import copy_file
import os

def wsjt_install(install):
#
# In a true python environment, Audio.so would be compiled from python
# I'm doing a nasty hack here to support our hybrid build system -db
#
	if install == 1:
	    os.makedirs('build/lib/WsjtMod')
	    copy_file('WsjtMod/Audio.so', 'build/lib/WsjtMod')
	setup(name='Wsjt',
	version=version,
	description='Wsjt Python Module for Weak Signal detection',
	long_description='''
WSJT is a computer program designed to facilitate Amateur Radio
communication under extreme weak-signal conditions.  Three very
different coding and modulation methods are provided: one for
communication by "meteor scatter" techniques on the VHF bands; one for
meteor and ionospheric scatter, primarily on the 6 meter band; and one
for the very challenging EME (Earth-Moon-Earth) path.
''',
	author='Joe Taylor',
	author_email='joe@Princeton.EDU',
	license='GPL',
	url='http://physics.princeton.edu/pulsar/K1JT',
	scripts=['wsjt','wsjt.py'],
	packages=['WsjtMod'],
	)

if __name__ == '__main__':
	import sys
	if 'install' in sys.argv:
	    wsjt_install(1)
	else:
	    wsjt_install(0)

