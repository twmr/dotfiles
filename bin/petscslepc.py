#!/usr/bin/env python

import logging
import os
from os.path import isdir
import subprocess

petscdir = os.getenv('PETSC_DIR')
petsc4pydir = os.getenv('PETSC4PY_DIR')
slepcdir = os.getenv('SLEPC_DIR')
slepc4pydir = os.getenv('SLEPC4PY_DIR')

petscurl = "https://bitbucket.org/petsc/petsc.git"
slepcurl = "http://www.grycap.upv.es/slepc/svn/trunk"
# slepcurl = "http://www.grycap.upv.es/slepc/svn/branches/slepc-3_4-branch"

petsc4pyurl = "https://bitbucket.org/petsc/petsc4py"
slepc4pyurl = "https://code.google.com/p/slepc4py/"

# syscallstdout =  os.open(os.devnull, os.O_RDWR)
# syscallstderr = os.open(os.devnull, os.O_RDWR)
syscallstdout =  None
syscallstderr = None

import sys

if len(sys.argv) < 2:
    raise ValueError("specify a repo (in [slepc, petsc, "
                     "petscp4y, slepc4py]")

def check_call(*args, **kwargs):
    logger.info("executing %s" % (" ".join(args[0])))
    return subprocess.check_call(*args, **kwargs)

tool = sys.argv[1]

logging.basicConfig(
    format='%(asctime)s: %(levelname)s: %(name)s: %(message)s',
    level=logging.DEBUG)
logger = logging

if tool == 'petsc':
    if not isdir(petscdir):
        # os.makedirs(petscdir)
        cmd = ['git', 'clone', '--depth', '1', '-vvv', petscurl, petscdir]
        logger.info("Checkout of Repo petsc with " + " ".join(cmd))

        check_call(cmd, stdout=syscallstdout,
                   stderr=syscallstderr)

    logger.info('cd ' + petscdir)
    os.chdir(petscdir)
    check_call(['git', 'pull'], stdout=syscallstdout, stderr=syscallstderr)

    petsccompilecmd = ['./configure', os.getenv('PETSC_MAIN_FLAGS'),
                       os.getenv('PETSC_OPT_FLAGS'), os.getenv('PETSC_DEBUGGING')]

    for c in petsccompilecmd:
        print c
    check_call(petsccompilecmd, stdout=syscallstdout,
               stderr=syscallstderr)
    os.system("make all")

if tool == 'slepc':
    if not isdir(slepcdir):
        cmd = ['svn', 'checkout', slepcurl, slepcdir]
        logger.info("Checkout of Repo slepc with " + " ".join(cmd))
        check_call(cmd, stdout=syscallstdout,
                   stderr=syscallstderr)

    logger.info('cd ' + slepcdir)
    os.chdir(slepcdir)
    check_call(['svn', 'update'], stdout=syscallstdout, stderr=syscallstderr)
    check_call(["./configure"], stdout=syscallstdout,
               stderr=syscallstderr)
    logger.info("calling make")
    os.system("make all")
    logger.info("calling testscripts")
    os.system("make test")

if tool == 'petsc4py':
    print "petsc4py dir", petsc4pydir
    if not isdir(petsc4pydir):
        cmd = ['hg', 'clone', petsc4pyurl, petsc4pydir]
        logger.info("Checkout of Repo petsc4py with " + " ".join(cmd))
        check_call(cmd, stdout=syscallstdout,
                   stderr=syscallstderr)

    logger.info('cd ' + petsc4pydir)
    os.chdir(petsc4pydir)
    check_call(['hg', 'update'], stdout=syscallstdout, stderr=syscallstderr)
    check_call(['hg', 'pull'], stdout=syscallstdout, stderr=syscallstderr)
    check_call(['make', 'build'], stdout=syscallstdout, stderr=syscallstderr)

if tool == 'slepc4py':
    print "slepc4py dir", slepc4pydir
    if not isdir(slepc4pydir):
        cmd = ['hg', 'clone', slepc4pyurl, slepc4pydir]
        logger.info("Checkout of Repo petsc4py with " + " ".join(cmd))
        check_call(cmd, stdout=syscallstdout,
                   stderr=syscallstderr)

    logger.info('cd ' + slepc4pydir)
    os.chdir(slepc4pydir)
    check_call(['hg', 'update'], stdout=syscallstdout, stderr=syscallstderr)
    check_call(['hg', 'pull'], stdout=syscallstdout, stderr=syscallstderr)
    check_call(['make', 'build'], stdout=syscallstdout, stderr=syscallstderr)
