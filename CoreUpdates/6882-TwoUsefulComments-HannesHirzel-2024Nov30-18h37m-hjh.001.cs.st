'From Cuis7.1 [latest update: #6858] on 30 November 2024 at 6:39:45 pm'!

!ReferenceStream class methodsFor: 'as yet unclassified' stamp: 'hjh 11/30/2024 18:38:35'!
dumpOnFile: aFileEntry object: anObject
	"Warning: If the file given by aFileEntry exists, it will be overwritten."
	
	aFileEntry forceWriteStreamDo: [ :stream |
		(self on: stream) nextPut: anObject ].! !

!ReferenceStream class methodsFor: 'as yet unclassified' stamp: 'hjh 11/30/2024 18:37:58'!
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
	| answer |
	aFileEntry readStreamDo: [ :stream |
		answer := (self on: stream) next ].
	^answer! !

