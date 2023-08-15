set -ex
stack test --coverage
stack clean && stack build --pedantic --copy-bins --local-bin-path .
#git clone https://github.com/LineageOS/hudson
cd hudson
git pull
cd ..
rm -fr html
mkdir html/device html/brand -p
time ./lineageos-history-exe

