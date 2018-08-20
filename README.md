# ConvertAndSearch
The database searcher and log file converter is a stand-alone
application built to facilitate easy retrieval of information from
instrument log files and SQLite database files.
In the case of
database files, the application allows the user to perform basic
searches through a GUI, or to enter SQL statements. These searches can
then be saved for future use. The user can easily switch between saved
databases. The application can
also convert textual log files into an easier-to-search database.

## Getting started
A windows installer is avalible for those who want to use the application rather than contribute to its development. The following instructions are for development mode.

### Prerequisites
- Chez Scheme 9.5.1 from June 14, 2018 or later
- Cygwin with bash, git, graphviz, grep, perl, texlive, etc.

### Installing
* Run `make` to get started
* If you change anything in resources/src run `resources/buildScript`
* Use `npm start` to run the progrma

## Deployment
### Prerequisites 
- Wix-installer

### Creating installer
#### Updating InfozamInstaller.wxs
- Change the UpgradeCode to a new GUID
- Update the version
- Add any newly created files into the list of files and the corresponding feature.
- Remove any files that were removed

#### Build the installer
- Run `buildInstaller`

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details




