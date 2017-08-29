'From Cuis 5.0 of 7 November 2016 [latest update: #3160] on 28 August 2017 at 9:40:53 pm'!
!classDefinition: #SimpleServiceEntry category: #'Tools-FileList'!
ActiveModel subclass: #SimpleServiceEntry
	instanceVariableNames: 'provider label selector useLineAfter stateSelector description argumentGetter buttonLabel argumentProvider triggerFileListChanged sortOrder '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!

!SimpleServiceEntry methodsFor: 'accessing' stamp: 'jmv 8/28/2017 21:35:11'!
sortOrder
	^sortOrder! !

!SimpleServiceEntry methodsFor: 'accessing' stamp: 'jmv 8/28/2017 21:28:07'!
sortOrder: aNumber
	sortOrder _ aNumber! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 8/28/2017 21:40:08'!
serviceBrowseCode
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'code file browser'
		selector: #browseCode:
		description: 'open a "file-contents browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'code')
			sortOrder: 10;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 8/28/2017 21:40:05'!
serviceBrowsePackage
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'package file browser'
		selector: #browsePackage:
		description: 'open a "package file browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'package browser')
			sortOrder: 10;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 8/28/2017 21:39:59'!
serviceContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'code file contents'
		selector: #browseContents:
		description: 'open a code file contents tool on this file'
		buttonLabel: 'contents')
			sortOrder: 20;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 8/28/2017 21:39:55'!
servicePackageContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'package file contents'
		selector: #browsePackageContents:
		description: 'open a code file contents tool on this package file'
		buttonLabel: 'pck contents')
			sortOrder: 20;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !


!FileList class methodsFor: 'file reader registration' stamp: 'jmv 8/28/2017 21:39:36'!
itemsForFile: filename
	"Answer a list of services appropriate for a file of the given name"
	| services suffix classList |
	suffix _ (FileIOAccessor default extensionFor: filename) asLowercase.
	"Build the list dynamically for all implementers of the appropriate class method... registration no longer required"
	classList _ (Smalltalk allClassesImplementing: #fileReaderServicesForFile:suffix:)
		collect: [ :item |
			item class == Metaclass ifTrue: [ item soleInstance ]]
		thenSelect: [ :item |
			item notNil ].
	services _ OrderedCollection new.
	classList do: [ :reader |
		services addAll:
			(reader
				fileReaderServicesForFile: filename
				suffix: suffix) ].
	services sort: [ :a :b | a sortOrder < b sortOrder ].
	^ services.! !


!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 8/28/2017 21:28:59'!
serviceFileIn
	"Answer a service for filing in an entire file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'fileIn entire file'
		selector: #fileIn:
		description: 'file in the entire contents of the file, which is expected to contain Smalltalk code in fileout ("chunk") format'
		buttonLabel: 'filein')
			sortOrder: 100;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!ChangeSet class methodsFor: 'file list services' stamp: 'jmv 8/28/2017 21:29:05'!
serviceInstall
	"Answer a service for filing in an entire file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'install code'
		selector: #install:
		description: 'install change set'
		buttonLabel: 'install')
			sortOrder: 100;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !


!CodePackageFile class methodsFor: 'file list services' stamp: 'jmv 8/28/2017 21:29:39'!
serviceInstallPackage
	"Answer the service of installing a CodePackageFile (a chunk format file for a CodePackage)"

	^ (SimpleServiceEntry
		provider: self 
		label: 'install package'
		selector: #installPackageStream:
		description: 'install the package, replacing all existing code in the package (if any); create a new change set just for the installation'
		buttonLabel: 'install package')
			sortOrder: 100;
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!classDefinition: #SimpleServiceEntry category: #'Tools-FileList'!
ActiveModel subclass: #SimpleServiceEntry
	instanceVariableNames: 'provider label selector useLineAfter stateSelector description argumentGetter buttonLabel argumentProvider triggerFileListChanged sortOrder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-FileList'!
