'From Cuis 4.1 of 12 December 2012 [latest update: #1553] on 13 January 2013 at 1:39:24 pm'!

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 1/11/2013 13:46'!
serviceContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'code file contents'
		selector: #browseContents:
		description: 'open a code file contents tool on this file'
		buttonLabel: 'contents')
			argumentGetter: [ :fileList | fileList readOnlyStream ]! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 1/11/2013 13:46'!
serviceMCZContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'code file contents'
		selector: #browseMCZContents:
		description: 'open a code file contents tool on this file'
		buttonLabel: 'contents')
			argumentGetter: [ :fileList | fileList readOnlyStream ]! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 1/11/2013 13:45'!
servicePackageContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'package file contents'
		selector: #browsePackageContents:
		description: 'open a code file contents tool on this package file'
		buttonLabel: 'pck contents')
			argumentGetter: [ :fileList | fileList readOnlyStream ]! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 1/11/2013 13:45'!
serviceRecentChanges
	"Answer a service for opening a changelist browser on the tail end of a .changes file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'recent changes in file'
		selector: #browseRecentLogOnPath:
		description: 'open a changelist tool on recent changes in file'
		buttonLabel: 'recent changes'! !


!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 1/11/2013 15:13'!
fileReaderServicesForFile: fullName suffix: suffix
	"These would better be done by ChangeSorter!!"

	(#('cs' 'cs.st') includes: suffix) ifTrue: [
		^{self serviceInstall} ].

	(#('st') includes: suffix) ifTrue: [
		^{self serviceFileIn} ].

	^#()! !

!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 1/11/2013 13:02'!
serviceFileIn
	"Answer a service for filing in an entire file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'fileIn entire file'
		selector: #fileIn:
		description: 'file in the entire contents of the file, which is expected to contain Smalltalk code in fileout ("chunk") format'
		buttonLabel: 'filein'! !

!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 1/11/2013 13:02'!
serviceInstall
	"Answer a service for filing in an entire file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'install code'
		selector: #install:
		description: 'install code (like fileIn), but store changes in a separate change set'
		buttonLabel: 'install'! !

!ChangeSet class methodsFor: 'services' stamp: 'jmv 1/11/2013 13:04'!
fileIn: fullName
	"File in the entire contents of the file specified by the name provided"

	| fileName stream |
	fullName ifNil: [^ Beeper beep].
	fileName _ fullName.
	(fullName endsWith: '.gz')
		ifTrue: [
			(self confirm: ('{1}
appears to be a compressed file.
Do you want to uncompress it?' format:{fullName}))
				ifFalse: [ ^self ]
				ifTrue: [
					fileName _ GZipReadStream saveContents: fullName.
					stream _ FileStream readOnlyFileNamed: fileName ]]
		ifFalse: [
			stream _ FileStream readOnlyFileNamed: fileName ].
	stream fileIn! !

!ChangeSet class methodsFor: 'services' stamp: 'jmv 1/11/2013 13:05'!
install: fullName
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	| localName |
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: localName do: [ self fileIn: fullName ].
	('Installed ChangeSet: ', localName) print! !


!CodePackageFile class methodsFor: 'services' stamp: 'jmv 1/11/2013 12:49'!
installMonticelloPackageStream: aStream
	
	| stream fullName pkName |
	fullName _ aStream name.
	pkName _ CodePackageFile monticelloPackageNameFrom: fullName.
	stream _ aStream asUnZippedStream: 'snapshot/source.st'.
	stream ascii.
	Cursor wait showWhile: [
		CodePackageFile
			installFileStream: stream
			packageName: pkName
			fullName: fullName ]! !

!CodePackageFile class methodsFor: 'services' stamp: 'jmv 1/11/2013 12:50'!
installPackageStream: aStream

	| fullName pkName existing |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	existing _ CodePackage named: pkName createIfAbsent: false registerIfNew: false.
	(existing isNil
		or: [ existing hasUnsavedChanges not
			or: [ self confirm: 'If you install this package, there are unsaved changes that will be lost.', String newLineString, 'Continue?' ]]) ifTrue: [
		Cursor wait showWhile: [
			CodePackageFile
				installFileStream: aStream
				packageName: pkName
				fullName: fullName ]]! !

!CodePackageFile class methodsFor: 'file list services' stamp: 'jmv 1/11/2013 12:52'!
fileReaderServicesForFile: fullName suffix: suffix
	"FileList buttons that are serviced by us."

	suffix = 'mcz'
		ifTrue: [ ^ { self serviceInstallMonticelloPackage } ].

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ { self serviceInstallPackage } ].
	^#()! !

!CodePackageFile class methodsFor: 'file list services' stamp: 'jmv 1/11/2013 12:49'!
serviceInstallMonticelloPackage
	"Answer the service of installing a PackageFile (a chunk format file for a PackageInfo)"

	^ (SimpleServiceEntry
		provider: self 
		label: 'install Monticello package'
		selector: #installMonticelloPackageStream:
		description: 'install the Monticello package, replacing all existing code in the package (if any); create a new change set just for the installation'
		buttonLabel: 'install package')
		argumentGetter: [ :fileList | fileList readOnlyStream ]! !

!CodePackageFile class methodsFor: 'file list services' stamp: 'jmv 1/11/2013 12:51'!
serviceInstallPackage
	"Answer the service of installing a PackageFile (a chunk format file for a PackageInfo)"

	^ (SimpleServiceEntry
		provider: self 
		label: 'install package'
		selector: #installPackageStream:
		description: 'install the package, replacing all existing code in the package (if any); create a new change set just for the installation'
		buttonLabel: 'install package')
		argumentGetter: [ :fileList | fileList readOnlyStream ]! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 1/13/2013 13:38'!
fileReaderServicesForFile: fullName suffix: suffix
	"FileList buttons that open a ChangeListWindow on contents"

	(#('st' 'cs' 'cs.st') includes: suffix)
		ifTrue: [ ^ {self serviceContents}  ].

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ {self servicePackageContents} ].

	suffix = 'changes'
		ifTrue: [ ^ {self serviceRecentChanges} ].

	suffix = 'mcz'
		ifTrue: [ ^ {self serviceMCZContents} ].

	^#()! !


!ChangeSorter class methodsFor: 'class initialization' stamp: 'jmv 1/11/2013 12:42'!
initialize
	"Initialize the class variables"

	AllChangeSets
		ifNil: [AllChangeSets _ OrderedCollection new].

	"
	ChangeSorter initialize
	"
! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 1/11/2013 15:14'!
fileReaderServicesForFile: fullName suffix: suffix
	"FileList buttons that open a CodeFileBrowserWindow on contents."

	(#('st' 'cs' 'cs.st') includes: suffix) ifTrue: [
		^ { self serviceBrowseCode } ].

	suffix = 'mcz' ifTrue: [
		^ { self serviceBrowseMCZCode } ].

	(suffix = 'pck') | (suffix = 'pck.st') ifTrue: [
		^ { self serviceBrowseCode } ].

	^#()! !


!FileList methodsFor: 'file list menu' stamp: 'jmv 1/11/2013 13:48'!
itemsForFile: fullName
	"Answer a list of services appropriate for a file of the given full name"
	^ self class itemsForFile: fullName! !


!FileList class methodsFor: 'class initialization' stamp: 'jmv 1/11/2013 13:42'!
initialize
	"
	FileList initialize
	"
	FileReaderRegistry _ OrderedCollection new.
	self registerFileReader: CodeFileBrowser.	"code"
	self registerFileReader: ChangeList.			"contents"
	self registerFileReader: ChangeSet.			"filein install"
	self registerFileReader: CodePackageFile. 	"install"
	self registerFileReader: GZipReadStream.
	self registerFileReader: GZipWriteStream.! !

!FileList class methodsFor: 'file reader registration' stamp: 'jmv 1/11/2013 13:00'!
itemsForFile: fullName
	"Answer a list of services appropriate for a file of the given full name"

	| services suffix |
	suffix _ (FileDirectory extensionFor: fullName) asLowercase.
	services _ OrderedCollection new.
	FileReaderRegistry do: [ :reader |
		reader ifNotNil: [
			services addAll: (reader fileReaderServicesForFile: fullName suffix: suffix)]].
	^ services! !

!FileList class methodsFor: 'file reader registration' stamp: 'jmv 1/11/2013 13:00'!
registerFileReader: aProviderClass
	"register the given class as providing services for reading files"

	(FileReaderRegistry includes: aProviderClass) 
			ifFalse: [ FileReaderRegistry addLast: aProviderClass ]! !

!FileList class methodsFor: 'file reader registration' stamp: 'jmv 1/11/2013 13:01'!
unregisterFileReader: aProviderClass
	"unregister the given class as providing services for reading files"

	FileReaderRegistry remove: aProviderClass ifAbsent: nil! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 1/11/2013 13:50'!
fileSelectedMenu

	| itemsPart1 itemsPart2 itemsPart3 itemsPart4 n1 n2 n3 services aMenu |
	aMenu _ MenuMorph new defaultTarget: model.
	itemsPart1 _ model itemsForAnyFile1.
	itemsPart2 _ model itemsForFile: model fullName.
	itemsPart3 _ model itemsForAnyFile2.
	itemsPart4 _ model itemsForNoFile.
	n1 _ itemsPart1 size.
	n2 _ n1 + itemsPart2 size.
	n3 _ n2 + itemsPart3 size.
	services _ itemsPart1, itemsPart2, itemsPart3, itemsPart4.
	services do: [ :svc | svc addDependent: model ].
	^ aMenu 
		addServices: services 
		for: model
		extraLines:{ n1 . n2 . n3 }
! !


!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'jmv 1/11/2013 12:52'!
fileReaderServicesForFile: fullName suffix: suffix
	"FileList buttons that are serviced by us."

	^suffix = 'gz'
		ifTrue: [ {self serviceViewDecompress. self serviceDecompressToFile} ]
		ifFalse: [ #() ]! !


!GZipWriteStream class methodsFor: 'file list services' stamp: 'jmv 1/11/2013 12:52'!
fileReaderServicesForFile: fullName suffix: suffix
	"FileList buttons that are serviced by us."
	"Don't offer to compress already-compressed files
	sjc 3-May 2003-added jpeg extension"

	^({ 'gz' . 'sar' . 'zip' . 'gif' . 'jpg' . 'jpeg'. 'pr'. 'png' } includes: suffix)
		ifTrue: [ #() ]
		ifFalse: [ { self serviceCompressFile } ]
! !

!methodRemoval: GZipWriteStream class #initialize!
GZipWriteStream class removeSelector: #initialize!
!methodRemoval: GZipWriteStream class #services!
GZipWriteStream class removeSelector: #services!
GZipWriteStream initialize!
!methodRemoval: GZipReadStream class #services!
GZipReadStream class removeSelector: #services!
!methodRemoval: Form class #fileReaderServicesForFile:suffix:!
Form class removeSelector: #fileReaderServicesForFile:suffix:!
!methodRemoval: Form class #initialize!
Form class removeSelector: #initialize!
!methodRemoval: Form class #services!
Form class removeSelector: #services!
Form initialize!
!methodRemoval: FileStream class #fileIn:!
FileStream class removeSelector: #fileIn:!
!methodRemoval: FileStream class #fileReaderServicesForFile:suffix:!
FileStream class removeSelector: #fileReaderServicesForFile:suffix:!
!methodRemoval: FileStream class #initialize!
FileStream class removeSelector: #initialize!
!methodRemoval: FileStream class #install:!
FileStream class removeSelector: #install:!
!methodRemoval: FileStream class #isSourceFileSuffix:!
FileStream class removeSelector: #isSourceFileSuffix:!
!methodRemoval: FileStream class #serviceFileIn!
FileStream class removeSelector: #serviceFileIn!
!methodRemoval: FileStream class #serviceInstall!
FileStream class removeSelector: #serviceInstall!
!methodRemoval: FileStream class #services!
FileStream class removeSelector: #services!
FileStream initialize!
!methodRemoval: FileList class #registeredFileReaderClasses!
FileList class removeSelector: #registeredFileReaderClasses!
FileList initialize!
!methodRemoval: FileList #fullFileListMenu!
FileList removeSelector: #fullFileListMenu!
!methodRemoval: FileList #myServicesForFile:suffix:!
FileList removeSelector: #myServicesForFile:suffix:!
!methodRemoval: FileList #offerAllFileOptions!
FileList removeSelector: #offerAllFileOptions!
!methodRemoval: FileList #serviceAllFileOptions!
FileList removeSelector: #serviceAllFileOptions!
!methodRemoval: CodeFileBrowserWindow class #installMonticelloPackageStream:!
CodeFileBrowserWindow class removeSelector: #installMonticelloPackageStream:!
!methodRemoval: CodeFileBrowserWindow class #installPackageStream:!
CodeFileBrowserWindow class removeSelector: #installPackageStream:!
!methodRemoval: CodeFileBrowser class #initialize!
CodeFileBrowser class removeSelector: #initialize!
!methodRemoval: CodeFileBrowser class #installMonticelloPackage:!
CodeFileBrowser class removeSelector: #installMonticelloPackage:!
!methodRemoval: CodeFileBrowser class #installPackage:!
CodeFileBrowser class removeSelector: #installPackage:!
!methodRemoval: CodeFileBrowser class #serviceInstallMonticelloPackage!
CodeFileBrowser class removeSelector: #serviceInstallMonticelloPackage!
!methodRemoval: CodeFileBrowser class #serviceInstallPackage!
CodeFileBrowser class removeSelector: #serviceInstallPackage!
!methodRemoval: CodeFileBrowser class #services!
CodeFileBrowser class removeSelector: #services!
CodeFileBrowser initialize!
!methodRemoval: ChangeSorter class #fileReaderServicesForFile:suffix:!
ChangeSorter class removeSelector: #fileReaderServicesForFile:suffix:!
!methodRemoval: ChangeSorter class #services!
ChangeSorter class removeSelector: #services!
ChangeSorter initialize!

!ChangeSorter class reorganize!
('enumerating' allChangeSets allChangeSetsWithClass:selector: changeSetNamed: existingOrNewChangeSetForPackage: existingOrNewChangeSetNamed:forBaseSystem: mostRecentChangeSetWithChangeForClass:selector:)
('class initialization' initialize unload)
('removing' removeChangeSet: zapAllChangeSets)
('services' countOfChangeSetsWithClass:andSelector: doesAnyChangeSetHaveClass:andSelector:)
!

!methodRemoval: ChangeList class #initialize!
ChangeList class removeSelector: #initialize!
!methodRemoval: ChangeList class #serviceBrowseChangeFile!
ChangeList class removeSelector: #serviceBrowseChangeFile!
!methodRemoval: ChangeList class #serviceBrowseDotChangesFile!
ChangeList class removeSelector: #serviceBrowseDotChangesFile!
!methodRemoval: ChangeList class #serviceBrowseMCZContents!
ChangeList class removeSelector: #serviceBrowseMCZContents!
!methodRemoval: ChangeList class #serviceBrowsePackageFile!
ChangeList class removeSelector: #serviceBrowsePackageFile!
!methodRemoval: ChangeList class #services!
ChangeList class removeSelector: #services!
ChangeList initialize!
