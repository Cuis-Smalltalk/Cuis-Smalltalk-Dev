'From Cuis 4.2 of 25 July 2013 [latest update: #2530] on 16 October 2015 at 3:56:38.504429 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 10/16/2015 15:56'!
saveAsNewVersion
	"Save the image/changes using the next available version number."
	"
	Smalltalk saveAsNewVersion
	"
	| fileName newName changesName systemVersion |
	self okayToSave ifFalse: [ ^ self ].
	systemVersion _ SystemVersion current.
	fileName _ String streamContents: [ :strm |
		strm
			nextPutAll: 'Cuis';
			print: systemVersion versionMajor;
			nextPut: $.;
			print: systemVersion versionMinor;
			nextPut: $-;
			print: systemVersion highestUpdate ].
	newName _ fileName, '.image'.
	(FmDirectoryEntry smalltalkImageDirectory / newName) exists ifTrue: [
		newName _ FmDirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', newName, '
curiously already present, even though there is
no corresponding .image file.   Please remedy
manually and then repeat your request.' ].
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAs: newName andQuit: false clearAllClassState: true! !

!SystemDictionary methodsFor: 'toDeprecate' stamp: 'jmv 10/16/2015 15:50'!
getFileNameFromUser

	| newName |
	newName _ FillInTheBlankMorph
		request: 'New File Name?'
		initialAnswer: self imageName asFileEntry name.
	newName isEmpty ifTrue: [ ^nil ].
	((self fullNameForImageNamed: newName) asFileEntry exists or: [
		(self fullNameForChangesNamed: newName) asFileEntry exists ] ) ifTrue: [
			(self confirm: ('{1} already exists. Overwrite?' format: {newName}))
				ifFalse: [ ^nil ]].
	^newName
! !

