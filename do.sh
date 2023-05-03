set -e
git clone https://github.com/LineageOS/hudson
cd hudson
git stash
git checkout master
git log --pretty=format:'%H %cs' > ../commits.list
cd -
# cat commits.list |xargs -n2 ./1.sh

