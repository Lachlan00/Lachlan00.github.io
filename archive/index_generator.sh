echo "Creating index files.."
tree -H '.' -R -L 1 --noreport --charset utf-8 > index.html
# python index_rename.py
# echo ''
# echo 'Directory Tree'
tree