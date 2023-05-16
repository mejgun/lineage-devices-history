set -ex
#git clone https://github.com/LineageOS/hudson
cd hudson
git pull
cd ..
rm -ri html
mkdir html/device html/brand -p
./lineageos-history-exe

