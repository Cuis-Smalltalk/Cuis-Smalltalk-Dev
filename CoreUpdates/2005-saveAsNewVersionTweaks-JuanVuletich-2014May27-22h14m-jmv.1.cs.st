'From Cuis 4.2 of 25 July 2013 [latest update: #2004] on 27 May 2014 at 10:33:19.690686 pm'!

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 5/27/2014 22:27'!
versionMajor
	^versionMajor! !

!SystemVersion methodsFor: 'accessing' stamp: 'jmv 5/27/2014 22:27'!
versionMinor
	^versionMinor! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 5/27/2014 22:32'!
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
	newName _ fileName, FileDirectory dot,  FileDirectory imageSuffix.
	(FileDirectory default includesKey: newName) ifTrue: [
		newName _ FileDirectory default
			nextNameFor: fileName
			extension: FileDirectory imageSuffix ].
	changesName _ self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	(FileDirectory default includesKey: changesName) ifTrue: [
		^ self inform:
'There is already .changes file of the desired name,
', newName, '
curiously already present, even though there is
no corresponding .image file.   Please remedy
manually and then repeat your request.' ].
	self saveAs: newName.! !

