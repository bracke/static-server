# NAME
Static Server is an easy to use http server

# SYNOPSIS
**static** [switches]

# DESCRIPTION
Static_Server is a server software which is simple to use and get going.
You likely don't need to configure anything - just install it and run it from inside the folder you want to serve.

# OPTIONS
**-v, --version**
: Display version and exit

**-h, --help**
: Display help and exit

**-o, --open**
: Open browser window after starting the server

**-l, --log**
: Log activity

**-e, --error**
: Log errors

**-s, --secure**
: Enable https

**-P, --path PATH**
: Path to directory to be served (default is current directory)

**-ldir PATH**
: Path to log directory (default is current directory)

**-lname PATH**
: Name of log file, without file extension (default is 'static')

**-ename PATH**
: Name of error log file, without file extension (default is 'error')

**-p, --port**
: PORTNUMBER Port to use (defaults to 8080)


# EXAMPLES

**static**
: Most basic example. Will serve files from the current directory and be available from localhost:8080.

**static -P ~/Sites/ProjectA -p 8081**
: Will serve files from the supplied path and be available at localhost:8081.

**static -l -e -ldir "~/logs"**
: Will serve files from the current directory but will log activity and errors in the supplied directory.

**static -s -P ./home**
: Will serve with https from the supplied directory. This requires a pem file.
You can generate one with openSSL:

openssl req -newkey rsa:2048 -new -nodes -x509 -days 3650 -keyout key.pem -out cert.pem

cat key.pem >> cert.pem

rm key.pem

The cert.pem file needs be in the directory where you are starting static from, not the ./home folder.

# AUTHORS
Written by Bent Bracke.
https://www.linkedin.com/in/bentbracke

Static Server is highly dependant on the Ada Web Server, which has been written by Pascal Obry and Dmitriy Anisimkov.

# BUGS
Please submit bug reports and ideas as issues on the GitHub site of this project:
https://github.com/bracke/static-server


Also please consider submitting pull requests if You are able to fix bugs or otherwise improve on the project. This also includes improving the documentation, fixing spelling mistakes or improve install scripts.

# SEE ALSO
The source code for Static Server is available on GitHub:

You can read more about the Ada Web Server at https://docs.adacore.com/aws-docs/aws/index.html

