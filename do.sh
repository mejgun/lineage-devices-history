set -e
cd hudson
git stash
git checkout master
git log --pretty=format:'%H %cs' > ../commits.list
cd -
cat commits.list |xargs -n2 ./1.sh

