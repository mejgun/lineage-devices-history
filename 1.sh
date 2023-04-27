set -e
cd hudson
git checkout $1
cd ..
cp hudson/lineage-build-targets h/lineage-build-targets-$2


