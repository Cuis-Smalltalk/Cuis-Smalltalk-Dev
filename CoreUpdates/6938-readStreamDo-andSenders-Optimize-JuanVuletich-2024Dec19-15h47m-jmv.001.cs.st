'From Cuis7.3 [latest update: #6937] on 19 December 2024 at 3:56:36 pm'!

!ReferenceStream class methodsFor: 'as yet unclassified' stamp: 'jmv 12/19/2024 15:54:10'!
restoreFromFile: aFileEntry
"
 Note about importing String objects from Squeak: 
	
To make sure Strings can be moved from Squeak use 
           aString utf8Encoded 
to convert the strings to ByteArrays in Squeak before you export the object collection.

In Cuis you need to do then
           UnicodeString fromUtf8Bytes: aByteArray
to get them back.
"
	^aFileEntry readStreamDo: [ :stream |
		(self on: stream) next ].! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 12/19/2024 15:50:09'!
fromFileEntry: aFileEntry
	"Read a Form or ColorForm from the given file."

	^aFileEntry readStreamDo: [ :stream |
		self fromBinaryStream: stream binary ]! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 12/19/2024 15:54:48'!
browsePackageContents: aFileEntry
	"Opens a changeList on a fileStream"
	| changeList packageFile |
	packageFile := CodePackageFile onFileEntry: aFileEntry.
	changeList := aFileEntry readStreamDo: [ :stream |
		self new scanFile: stream from: 0 to: stream size ].
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference stringVersion ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new className: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aFileEntry name! !


!FileEntry methodsFor: 'actions-file' stamp: 'jmv 12/19/2024 15:47:13'!
binaryContents
	^self readStreamDo: [ :stream |
		stream binary contents ].! !

!FileEntry methodsFor: 'actions-file' stamp: 'jmv 12/19/2024 15:48:35'!
textContents
	^self readStreamDo: [ :stream |
		stream contents ].! !

!FileEntry methodsFor: 'accessing-stream' stamp: 'jmv 12/19/2024 15:51:26'!
readStreamDo: blockWithArg 
	"Raise FileDoesNotExistException if not found."
	| stream |
	stream := self readStream.
	^ [ blockWithArg value: stream ]
		ensure: [
			stream
				ifNotNil: [ :s | s close ]]! !

