#!/bin/bash
# Create_Release.sh version
# Run from project root:
# ./scripts/Create_Release.sh 0101
#
# Requires:
# - Pandoc
# - LaTeX for PDF generation
# - GNAT/gprbuild
#
red="\e[0;91m"
reset="\e[0m"
version=$1
CurrentFolderName=${PWD##*/}

# Should be run from project root
if [[ $CurrentFolderName != "static-server" ]]; then
  echo -e "${red}Please execute script from the project root${reset}"
  echo -e "${red}Exit, release NOT created${reset}"
  echo -e "${red}Please use ./scripts/Create_Release.sh 0101${reset}"
  echo
  exit 1
fi

LEN=$(echo ${#version})

# Should have the new version number as an argument
if [[ $LEN -eq 0 ]] ; then
  echo -e "${red}Version argument missing.${reset}"
  echo -e "${red}Exit, release NOT created${reset}"
  echo -e "${red}Please use ./scripts/Create_Release.sh 0101${reset}"
  echo
  exit 1
fi
if [[ $LEN -lt 4 ]] ; then version=`printf %04d $version`; fi

versionReadable=${version:0:2}.${version:2:2}
date=$(date +'%Y-%m-%d')

# Has the new version been documented in the changelog?

fgrep -Fq -m 1 $versionReadable "CHANGELOG.md"
HasVersion=$?
fgrep -Fq -m 1 "MISSINGDESCRIPTION" "CHANGELOG.md"
HasMissing=$?

if [[ "$HasVersion" == "1" ]] || [[ "$HasMissing" == "0" ]] ; then
  echo -e "${red}Version description is missing in CHANGELOG.md${reset}"
  echo -e "${red}Exit, release NOT created${reset}"

  if [[ "$HasMissing" == "1" ]] ; then
    printf -- "\n## %b (%b) \n\t- [MISSINGDESCRIPTION]\n" $versionReadable $date >> "CHANGELOG.md"
  fi
  exit 1
fi

echo "Creating release V$versionReadable"
OS=( unix macos windows )

# Create release dir
releasedir=releases/$version
mkdir $releasedir

# Create version.ads based on argument
printf "package Version is\n\tServe_Version: constant string := \"$versionReadable\";\nend Version;" > source/version.ads

# Create readme man page
printf -- "---\ntitle: Static Server\nsection: 1\nheader: User Manual\nfooter: Static Server %b\ndate: %b\n---\n" "$versionReadable" "$date" > releases/readme.1.md
cat README.md >> releases/readme.1.md
pandoc releases/readme.1.md -s -t man -o releases/readme.1

# Text manual
pandoc releases/readme.1.md -s -t rst -o releases/readme.txt
pandoc CHANGELOG.md -s -t rst -o releases/changelog.txt

# PDF manual
pandoc releases/readme.1.md -s -o releases/readme.pdf
pandoc CHANGELOG.md -s -o releases/changelog.pdf

# HTML manual
pandoc releases/readme.1.md -s -o releases/readme.html
pandoc CHANGELOG.md -s -t rst -o releases/changelog.html

# for all supported os:
for i in "${OS[@]}"
do
  echo
  echo $i
  echo

  # Create subfolder in release folder
  osfolder=releases/$version/$i
  filefolder=os/$i/files
  mkdir $osfolder

  # Copy osdefinitions
  cp os/$i/definitions.ads source/

  # Run release.gpr
  gprbuild -Prelease.gpr -Xos=$i
  chmod +x static

  # Copy binary to subfolder
  cp static $osfolder/static

  # Copy man page
  cp releases/readme.1 $osfolder/static.1

  # Copy readme to subfolder
  cp releases/readme.txt $osfolder/
  cp releases/readme.pdf $osfolder/
  cp releases/readme.html $osfolder/
  cp releases/changelog.txt $osfolder/
  cp releases/changelog.pdf $osfolder/
  cp releases/changelog.html $osfolder/

  # Copy license to subfolder
  cp LICENSE $osfolder/LICENSE

  # Copy files
  echo "Copying os specific files"
  cp $filefolder/* $osfolder/

  # Compress each subfolder
  echo
  echo "Compressing..."
  zipname=staticserver-v$version-$i.zip
  zip -9r $releasedir/$zipname $osfolder

  # Remove osfolder
  echo
  echo "Cleaning up"
  rm -rf $osfolder
  echo Done
done

rm releases/readme.1.md
rm releases/readme.1
rm releases/readme.txt
rm releases/readme.pdf
rm releases/readme.html
rm releases/changelog.txt
rm releases/changelog.pdf
rm releases/changelog.html