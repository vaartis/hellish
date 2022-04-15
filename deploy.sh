HELLISH_BUILD_MODE=debug alr build

rm -rf _deploy
mkdir _deploy _deploy/db/
cp bin/hellish _deploy/

echo "Copying assets"
cp -r assets/ _deploy/
# cp aws.ini _deploy/
cp -r db/migrations/ _deploy/db/migrations/

echo "Done"
