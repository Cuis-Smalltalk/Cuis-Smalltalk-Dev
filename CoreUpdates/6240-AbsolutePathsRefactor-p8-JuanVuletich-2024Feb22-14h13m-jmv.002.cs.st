'From Cuis6.3 [latest update: #6236] on 22 February 2024 at 3:03:40 pm'!

!FileList methodsFor: 'own services' stamp: 'jmv 2/22/2024 14:31:26'!
viewContentsInWorkspace
	"View the contents of my selected file in a new workspace"

	| fileEntry fileContents |
	fileEntry := directory // fileName.
	fileContents := fileEntry textContents.

	(Workspace new contents: fileContents) 
		openLabel: 'Workspace from ', fileName ::
			setProperty: #lastFilenameUsedToSave
			toValue: fileEntry pathName! !


!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 2/22/2024 15:01:04'!
asDirectoryEntry
	"Note: It is generally advisable to call one of the more specific messages:
		#/
		#withAbsolutePathName: 
		#withRelativePathName:to:"
	"See examples in #asFileEntry method comment"

	^self isAbsolutePathName
		ifTrue: [
			DirectoryEntry withAbsolutePathName: self ]
		ifFalse: [
			DirectoryEntry withRelativePathName: self to: DirectoryEntry currentDirectory ].! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 2/22/2024 15:03:14'!
asFileEntry
	"Note: It is generally advisable to call one of the more specific messages:
		#//
		#withAbsolutePathName: 
		#withRelativePathName:to:"
	"
	Windows	
		'C:\Windows' asFileEntry exists false
		'C:\Windows' asDirectoryEntry exists true
		'/' asFileEntry exists false
		'/' asDirectoryEntry exists false
		'C:\' asFileEntry exists false
		'C:\' asDirectoryEntry exists true
		('C:' asDirectoryEntry // 'Windows') exists false
		('C:' asDirectoryEntry / 'Windows') exists true
	Linux
		'/var' asFileEntry exists
		'/var' asDirectoryEntry exists true
		'/' asFileEntry exists false
		'/' asDirectoryEntry exists true
		'/media/cdrom' asFileEntry exists false
		'/media/cdrom' asDirectoryEntry exists true
		('/bin' asDirectoryEntry / 'more') exists false
		('/bin' asDirectoryEntry // 'more') exists true
	MacOsX
		'/var' asFileEntry exists false
		'/var' asDirectoryEntry exists true
		'/' asFileEntry exists false
		'/' asDirectoryEntry exists  true
		'/Volumes/SanDisk32-NTFS' asFileEntry exists false
		'/Volumes/SanDisk32-NTFS' asDirectoryEntry exists true
		'SanDisk32-NTFS' asFileEntry exists false
		'SanDisk32-NTFS' asDirectoryEntry exists false
	"
	self isRelativeMark ifTrue: [ ^self error: 'Maybe you need to call #asDirectoryEntry!!' ].
	^self isAbsolutePathName
		ifTrue: [
			FileEntry withAbsolutePathName: self ]
		ifFalse: [
			FileEntry withRelativePathName: self to: DirectoryEntry currentDirectory ].! !


!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 2/22/2024 14:52:26'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection := (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFullFileEntry name, '
does not exist.'.
	selection = 1 ifTrue: [
		^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue: [
		newName := self request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FileIOAccessor default privateWriteableFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !

!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 2/22/2024 14:51:45'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName entry |
	entry := fullFileName asFullFileEntry.
	dir := entry parent.
	localName := entry name.
	choice := (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir removeKey: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName := self request: 'Enter a new file name' initialAnswer: fullFileName.
		^ FileIOAccessor default privateNewFile: newName asFileEntry ].

	self error: 'Please close this to abort file opening'! !

!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 2/22/2024 14:52:35'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir := fullFileName asFullFileEntry parent.
	files := dir fileNames.
	fileName := fullFileName asFullFileEntry name.
	choices := fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection := (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFullFileEntry name, '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName := (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName := self request: 'Enter a new file name' initialAnswer: fileName.
		"If Cancel was pressed, no file should be opened - Hernan"
		newName isEmpty ifTrue: [ ^nil ]].
	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !


!DirectoryEntry class methodsFor: 'base directories' stamp: 'jmv 2/22/2024 14:48:07'!
currentDirectory
	"Answer the current directory.

	In Unix it is the current directory in the OS shell that started us.
	In Windows the same happens if the image file is in a subree of the Windows current directory.

	But it defaults to the directory in wich this Smalltalk image was started (or last saved) if this fails
	(this usually happens, for example, if the image is dropped on the VM in a Windows explorer).
	See #getCurrentWorkingDirectory

	DirectoryEntry currentDirectory
	"
	CurrentDirectory notNil ifTrue: [
		^ CurrentDirectory ].

	Smalltalk getCurrentWorkingDirectory ifNotNil: [ :wd |
		^ CurrentDirectory := self withAbsolutePathName: wd ].

	^ CurrentDirectory := self userBaseDirectory.! !


!SystemWindow methodsFor: 'user interface' stamp: 'jmv 2/22/2024 14:42:28'!
forceSaveContents
	"Saves the contents to the file name in the #lastFilenameUsedToSave property.
	
	If none exists ask the user for a filename."
	
	self hasSaveAs ifFalse: [ ^ self ].
	self valueOfProperty: #lastFilenameUsedToSave
		ifPresentDo: [ :pathName | self forceSaveContentsTo: pathName asFullFileEntry ]
		ifAbsent: [
			self
				request: 'Enter file name'
				initialAnswer: ''
				verifying: [ :aString | aString notEmpty ]
				do: [ :fileName | self saveContentsTo: fileName asFileEntry ] ]! !

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 2/22/2024 14:39:57'!
forceSaveContentsTo: aFileEntry
	"Saves the contents to the given filename.
	
	Force overwrting the file if it exists."

	self hasSaveAs ifFalse: [^self ].
	aFileEntry forceWriteStreamDo: [ :stream |
		model saveOn: stream ].
	self 
		setProperty: #lastFilenameUsedToSave toValue: aFileEntry pathName.! !

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 2/22/2024 14:44:03'!
saveContents
	"Prompts the user for a file name and saves the contents to the file"

	self hasSaveAs ifFalse: [ ^ self ].
	self
		request: 'Enter file name'
		initialAnswer: (self valueOfProperty: #lastFilenameUsedToSave ifAbsent: [ '' ])
		verifying: [ :aString |
			aString notEmpty ]
		do: [ :fileName |
			self saveContentsTo: fileName asFileEntry ].! !

!SystemWindow methodsFor: 'user interface' stamp: 'jmv 2/22/2024 14:40:08'!
saveContentsTo: aFileEntry
	"Saves the contents to the given filename"

	self hasSaveAs ifFalse: [^self].
	aFileEntry writeStreamDo: [ :stream |
		model saveOn: stream ].
	self 
		setProperty: #lastFilenameUsedToSave toValue: aFileEntry pathName;
		notifyUserWith: 'Contents saved'.! !

