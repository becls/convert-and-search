install:
	npm install electron --save-dev
	npm install cross-spawn
	cd Resources; ./buildScript
	cd doc; make
	npm install electron-packager --save-dev -g
	./buildInstaller
