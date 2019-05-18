cd _site/

git config user.email "weien1292@gmail.com"
git config user.name "Ng Wei En"
git config --global push.default simple

git remote set-url origin git@github.com:wei2912/blog
git add -A && git diff-index --quiet HEAD || git commit -m "update at $(date)"
git push

