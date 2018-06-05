'From Cuis 5.0 of 7 November 2016 [latest update: #3341] on 1 June 2018 at 8:59:47 pm'!

!CodePackage class methodsFor: 'installing' stamp: 'jmv 6/1/2018 20:59:14'!
postPackageInstall
	"This gets called after installing all the package code.
	Redefine as appropriate"! !

!CodePackage class methodsFor: 'installing' stamp: 'jmv 6/1/2018 20:59:33'!
prePackageInstall
	"This gets called after creating the package class and installing its code, but before installing the rest of the package code
	Redefine as appropriate"! !


!CodePackage class reorganize!
('packages access' deregister: installedPackages named:createIfAbsent:registerIfNew: register:)
('searching' packageOfClass:ifNone: packageOfMethod:ifNone: packageOfMethodCategory:ofClass:ifNone: packageOfSystemCategory:ifNone:)
('startup' clearPaths)
('cached state access' releaseClassCachedState)
('installing' postPackageInstall prePackageInstall)
!

