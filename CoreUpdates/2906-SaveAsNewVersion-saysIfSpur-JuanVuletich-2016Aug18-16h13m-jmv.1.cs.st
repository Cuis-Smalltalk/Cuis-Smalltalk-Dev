'From Cuis 4.2 of 25 July 2013 [latest update: #2850] on 18 August 2016 at 4:13:11 pm'!

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/18/2016 16:13:01'!
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
		Smalltalk isSpur ifTrue: [
			strm nextPutAll: '-spur' ]].
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
', newName, '
curiously already present, even though there is
no corresponding .image file.   Please remedy
manually and then repeat your request.' ].
	"Try to clear all user state, including all class vars, preferences, etc"
	self saveAs: newName andQuit: false clearAllClassState: true! !

