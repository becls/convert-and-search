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
* Run `make` to get started
* If you change anything in swish/src run `swish/buildScript`
* Use `npm start` to run the progrma

## Packaging the application
* It is recommened to use Electron-Packager to package the application for distribution
* Use `npm install electron-packager` to download Electron-Packager
* Electron-Packager has a number of optional flags. More information about these flagas can be found in the docs.
* `electron-packager . ` should create a folder with everything needed to run the application
* Running the executable file in this folder should run the program


