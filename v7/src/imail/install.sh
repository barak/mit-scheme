#!/bin/sh
cd ~/new/imail
mkdir -p $bscm/lib/imail
cpx -c *.com *.bci imail.bco imail.bld load.scm $bscm/lib/imail
