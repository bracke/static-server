echo "Installing static..."

# Install the binary
sudo cp static /usr/bin/

# Install the man page
sudo cp static.1.gz /usr/share/man/man1/
sudo mandb

echo "done"