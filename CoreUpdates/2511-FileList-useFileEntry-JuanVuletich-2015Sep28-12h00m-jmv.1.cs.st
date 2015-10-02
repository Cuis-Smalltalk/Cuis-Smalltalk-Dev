'From Cuis 4.2 of 25 July 2013 [latest update: #2509] on 28 September 2015 at 12:16:25.001205 pm'!

!CodePackageFile class methodsFor: 'services' stamp: 'jmv 9/28/2015 12:12'!
installPackage: aFileEntry

	| fullName pkName |
	fullName _ aFileEntry pathName.
	pkName _ CodePackageFile packageNameFrom: fullName.
	(FeatureRequirement name: pkName)
		pathName: fullName;
		satisfyRequirementsAndInstall "Install with all requirements"! !


!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 9/28/2015 12:07'!
browseFile: aFileEntry

	| codeFile organizer browser |
	organizer _ SystemOrganizer defaultList: Array new.
	aFileEntry readStream: [ :stream |
		codeFile _ (CodeFile new fullName: aFileEntry pathName; buildFrom: stream) ].
	organizer 
		classifyAll: codeFile classes keys 
		under: codeFile name.
	(browser _ CodeFileBrowser new)
		systemOrganizer: organizer;
		codeFile: codeFile.
	CodeFileBrowserWindow open: browser label: nil! !

!CodeFileBrowserWindow class methodsFor: 'services' stamp: 'jmv 9/28/2015 12:09'!
browsePackageFile: aFileEntry

	| codeFile organizer browser |
	organizer _ SystemOrganizer defaultList: Array new.
	aFileEntry readStream: [ :stream |
		codeFile _ (CodePackageFile new fullName: aFileEntry pathName; buildFrom: stream) ].
	organizer 
		classifyAll: codeFile classes keys 
		under: codeFile name.
	(browser _ CodeFileBrowser new)
		systemOrganizer: organizer;
		codeFile: codeFile.
	CodeFileBrowserWindow open: browser label: nil! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 9/28/2015 12:07'!
browseCode: aFileEntry

	CodeFileBrowserWindow browseFile: aFileEntry! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 9/28/2015 12:10'!
browsePackage: aFileEntry

	CodeFileBrowserWindow browsePackageFile: aFileEntry! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 9/28/2015 12:07'!
serviceBrowseCode
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'code-file browser'
		selector: #browseCode:
		description: 'open a "file-contents browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'code')
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 9/28/2015 12:10'!
serviceBrowsePackage
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'package-file browser'
		selector: #browsePackage:
		description: 'open a "package file browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'package browser')
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 9/28/2015 12:04'!
browseContents: aFileEntry
	"Opens a changeList on a fileStream"
	| changeList fileSize charCount |
	aFileEntry readStream: [ :stream |
		fileSize _ stream size.
		charCount _ fileSize.
		charCount > 1000000 ifTrue: [
			(self confirm: 'The file ', aFileEntry name , '
is really long (' , charCount printString , ' characters).
Would you prefer to view only the last million characters?')
				ifTrue: [ charCount _ 1000000 ]].
		changeList _ self new
			scanFile: stream from: fileSize-charCount to: fileSize.
	].
	ChangeListWindow open: changeList label: aFileEntry name! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 9/28/2015 12:04'!
serviceContents
	"Answer a service for opening a changelist browser on a file"

	^ (SimpleServiceEntry 
		provider: self 
		label: 'code file contents'
		selector: #browseContents:
		description: 'open a code file contents tool on this file'
		buttonLabel: 'contents')
			argumentGetter: [ :fileList | fileList selectedFileEntry ]! !


!CodePackageFile class methodsFor: 'file list services' stamp: 'jmv 9/28/2015 12:12'!
serviceInstallPackage
	"Answer the service of installing a CodePackageFile (a chunk format file for a CodePackage)"

	^ (SimpleServiceEntry
		provider: self 
		label: 'install package'
		selector: #installPackageStream:
		description: 'install the package, replacing all existing code in the package (if any); create a new change set just for the installation'
		buttonLabel: 'install package')
		argumentGetter: [ :fileList | fileList selectedFileEntry ]! !

!methodRemoval: CodeFileBrowserWindow class #browsePackageStream:named:!
CodeFileBrowserWindow class removeSelector: #browsePackageStream:named:!
!methodRemoval: CodeFileBrowserWindow class #browseStream:named:!
CodeFileBrowserWindow class removeSelector: #browseStream:named:!
!methodRemoval: FileList #readOnlyStream!
FileList removeSelector: #readOnlyStream!
