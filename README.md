# ConvertAndSearch
The database searcher and log file converter is a stand-alone
application built to facilitate easy retrieval of information from
instrument log files and SQLite database files. In the case of
database files, the application allows the user to perform basic
searches through a GUI, or to enter SQL statements. These searches can
then be saved for future use. The user can easily switch between saved
databases. The application can also convert textual log files into an
easier-to-search database.

## Getting started
A windows installer is available under releases for those who want to
use the application rather than contribute to its development. The
following instructions are for development mode.

### Prerequisites
- Chez Scheme 9.5.1 from June 21, 2018, or later
- Cygwin with bash, git, graphviz, grep, perl, texlive, etc.
- Node.js
- Swish from Sep. 7, 2018, or later
- [WiX Toolset v3.11.1](http://wixtoolset.org/releases/v3.11.1/stable) or later

### Building
* Run `make` to build `InfozamInstaller.exe`.

### Debugging
* Use `npm start` to run the program.

### New version
* Update the version in InfozamInstaller.wxs.
* Update FirstStepForInstall.wxs:
  - Change the Product Id to a new GUID.
  - Update the version.
* Run `make`.

## License
This project is licensed under the MIT License - see the
[LICENSE](LICENSE) file for details.
