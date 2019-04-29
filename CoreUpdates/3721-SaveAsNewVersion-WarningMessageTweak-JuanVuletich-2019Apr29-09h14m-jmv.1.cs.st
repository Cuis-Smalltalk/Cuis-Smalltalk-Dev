'From Cuis 5.0 of 7 November 2016 [latest update: #3720] on 29 April 2019 at 9:15:50 am'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 4/29/2019 09:14:33'!
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
			print: systemVersion highestUpdate.
		Smalltalk isSpur
			ifTrue: [	
				Smalltalk wordSize = 4 ifTrue: [
					strm nextPutAll: '-32' ]]
			ifFalse: [
				strm nextPutAll: '-v3' ]].
	newName _ fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName _ DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.' ].
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAs: newName andQuit: false clearAllClassState: true! !

