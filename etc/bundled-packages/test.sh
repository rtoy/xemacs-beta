# tests for the bundled packages feature

# usage: sh etc/bundled-packages/tests.sh [TMP_TEST_DIR]

# Always run this script from the top directory of the source tree.
# You need a mv that supports the -v for verbose flag, and a mkdir that
# supports the -p flag to make parents.
# Output from this script is preceded by 4 stars (****).

# This test script is probably more fragile than the build process, but if
# it runs to completion things are probably OK.

# configure the installation target

if test -z "$1"; then
 TMP_TEST_DIR=/tmp/test/bundled-packages
else
 TMP_TEST_DIR=$1
fi

srcdir=`pwd`
blddir=${TMP_TEST_DIR}/build
pkgdir=${TMP_TEST_DIR}/lib/xemacs

echo "**** srcdir = ${srcdir}"
echo "**** blddir = ${blddir}"
echo "**** pkgdir = ${pkgdir}"

if test -e "${pkgdir}"; then
 echo "**** pkgdir (${pkgdir}) exists; bailing out."
 exit -1
fi

# mv existing tarballs out of harm's way and make a fake one

echo "**** Moving existing tarballs to etc/bundled-packages/saved."
mkdir -p etc/bundled-packages/saved
cd etc/bundled-packages
echo "**** 'mv' may error because there are no files to move.  It's harmless."
mv -v *.tar.gz saved/
cd ../..

# configure in a temporary directory

if test -e ${blddir}; then
 echo "**** blddir (${blddir}) exists; bailing out."
 exit -1
fi
mkdir -p ${blddir}
cd ${blddir}
echo "**** Running 'configure'.  This takes *several minutes*."
echo "**** Redirecting configure output to ${blddir}/beta.err."
${srcdir}/configure >beta.err 2>&1

# test check-available-packages

echo "**** This test should produce no error and no output."
make check-available-packages
cd ${srcdir}/etc/bundled-packages
echo "**** This test should explain how to install bootstrap packages."
echo "This file pretends to be a bootstrap hierarchy." > xemacs-packages
tar czf bootstrap.tar.gz xemacs-packages
rm xemacs-packages
cd ${blddir}
make check-available-packages
echo "**** This test should explain how to install all three."
cd ${srcdir}/etc/bundled-packages
echo "This file pretends to be a xemacs-packages hierarchy." > xemacs-packages
echo "This file pretends to be a mule-packages hierarchy." > mule-packages
tar czf xemacs-sumo.tar.gz xemacs-packages
tar czf xemacs-mule-sumo.tar.gz mule-packages
rm xemacs-packages mule-packages
cd ${blddir}
make check-available-packages

# test installation without package path given

echo "**** Make the 'make-path' utility needed by the installation routine."
make -C lib-src make-path
echo "**** This test should error because --with-late-packages wasn't given."
make install-bootstrap-packages

# test installation with package path given

echo "**** Running 'configure'.  This takes *several minutes*."
echo "**** Redirecting configure output to ${blddir}/beta.err."
${srcdir}/configure --with-late-packages=${pkgdir} >beta.err 2>&1
echo "**** Make the 'make-path' utility needed by the installation routine."
make -C lib-src make-path
echo "**** Test install-bootstrap-packages."
make install-bootstrap-packages
echo "**** The following should list xemacs-packages in the right place."
ls ${pkgdir}/*

#### no tests below this line ####

# put tarballs back and clean up

cd ${srcdir}/etc/bundled-packages
rm *.tar.gz
echo "**** 'mv' may error because there are no files to move.  It's harmless."
mv -v saved/*.tar.gz ../
rmdir saved
rm -rf ${blddir} ${pkgdir}
exit 0
