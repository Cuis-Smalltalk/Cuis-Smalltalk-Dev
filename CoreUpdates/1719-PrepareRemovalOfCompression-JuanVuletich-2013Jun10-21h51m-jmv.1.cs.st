'From Cuis 4.1 of 12 December 2012 [latest update: #1718] on 10 June 2013 at 9:52:14.873595 pm'!

!CodePackageFile commentStamp: '<historical>' prior: 0!
A CodePackageFile represents a file with code for a package, regardless of whether it is installed (as a CodePackage) or not. It supports Cuis' .pck.st.!


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 6/9/2013 15:23'!
fileReaderServicesForFile: fullName suffix: suffix
	"FileList buttons that open a ChangeListWindow on contents"

	(#('st' 'cs' 'cs.st') includes: suffix)
		ifTrue: [ ^ {self serviceContents}  ].

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ {self servicePackageContents} ].

	suffix = 'changes'
		ifTrue: [ ^ {self serviceRecentChanges} ].

	^#()! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 6/9/2013 15:22'!
fileIn: fullName
	"File in the entire contents of the file specified by the name provided"

	|  stream |
	fullName ifNil: [^ Smalltalk beep ].
	stream _ FileStream readOnlyFileNamed: fullName.
	stream fileIn! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 6/9/2013 15:23'!
fileReaderServicesForFile: fullName suffix: suffix
	"FileList buttons that open a CodeFileBrowserWindow on contents."

	(#('st' 'cs' 'cs.st') includes: suffix) ifTrue: [
		^ { self serviceBrowseCode } ].

	(suffix = 'pck') | (suffix = 'pck.st') ifTrue: [
		^ { self serviceBrowseCode } ].

	^#()! !


!CodePackageFile class methodsFor: 'file list services' stamp: 'jmv 6/9/2013 15:24'!
fileReaderServicesForFile: fullName suffix: suffix
	"FileList buttons that are serviced by us."

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ { self serviceInstallPackage } ].
	^#()! !


!FileList class methodsFor: 'class initialization' stamp: 'jmv 6/9/2013 15:22'!
initialize
	"
	FileList initialize
	"
	FileReaderRegistry _ OrderedCollection new.
	self registerFileReader: CodeFileBrowser.	"code"
	self registerFileReader: ChangeList.			"contents"
	self registerFileReader: ChangeSet.			"filein install"
	self registerFileReader: CodePackageFile. 	"install"! !

FileList initialize!
