echo "Removing static server..."

# Remove man page
sudo rm /usr/share/man/man1/static.1.gz
sudo mandb

# Remove binary
sudo rm /usr/bin/static

echo "done"