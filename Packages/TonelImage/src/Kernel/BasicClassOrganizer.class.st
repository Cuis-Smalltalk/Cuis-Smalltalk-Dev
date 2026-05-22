Class {
	#name : 'BasicClassOrganizer',
	#superclass : 'Categorizer',
	#instVars : [
		'subject',
		'classComment',
		'commentStamp'
	],
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'accessing' }
BasicClassOrganizer >> classComment [
	classComment
		ifNil: [^ ''].
	^ classComment text ifNil: ['']
]

{ #category : 'accessing' }
BasicClassOrganizer >> classComment: aString  [
	"Store the comment, aString, associated with the object that refers to the 
	receiver."

	aString ifNil: [ ^classComment := nil ].
	
	aString isRemote
		ifTrue: [classComment := aString]
		ifFalse: [aString isEmpty
			ifTrue: [classComment := nil]
			ifFalse: [
				self error: 'use aClass classComment:'.
				classComment := RemoteString newString: aString onFileNumber: 2]]
				"Later add priorSource and date and initials?"
]

{ #category : 'accessing' }
BasicClassOrganizer >> classComment: aString  stamp: aStamp [
	"Store the comment, aString, associated with the object that refers to the receiver."

	self commentStamp: aStamp.
	aString isRemote
		ifTrue: [classComment := aString]
		ifFalse: [(aString isNil or: [aString isEmpty])
			ifTrue: [classComment := nil]
			ifFalse:
				[self error: 'use aClass classComment:'.
				classComment := RemoteString newString: aString onFileNumber: 2]]
				"Later add priorSource and date and initials?"
]

{ #category : 'accessing' }
BasicClassOrganizer >> commentRemoteStr [
	^ classComment
]

{ #category : 'accessing' }
BasicClassOrganizer >> commentStamp [
	"Answer the comment stamp for the class"

	^ commentStamp
]

{ #category : 'accessing' }
BasicClassOrganizer >> commentStamp: aStamp [
	commentStamp := aStamp
]

{ #category : 'accessing' }
BasicClassOrganizer >> dateCommentLastSaved [
	"Answer a Date object indicating when my class comment was last saved.  If there is no date stamp, or one of the old-time <historical>  guys, return nil"
	"RecentMessageSet organization dateCommentLastSaved"

	| aStamp tokens |
	(aStamp := self commentStamp) isEmptyOrNil ifTrue: [^ nil].
	tokens := aStamp findBetweenSubStrs: ' 
'.  "space is expected delimiter, but cr is sometimes seen, though of mysterious provenance"
	^ tokens size > 1
		ifTrue: [
			[tokens second asDate] ifError: nil]
]

{ #category : 'fileIn/Out' }
BasicClassOrganizer >> fileOutCommentOn: aFileStream moveSource: moveSource toFile: fileIndex [
	"Copy the class comment to aFileStream.  If moveSource is true (as in compressChanges or compressSources, then update classComment to point to the new file."
	| fileComment |
	self hasClassComment ifTrue: [
		aFileStream newLine.
		fileComment := RemoteString newString: self classComment
						onFileNumber: fileIndex toFile: aFileStream.
		moveSource ifTrue: [classComment := fileComment].
		Smalltalk userChangesAppendDo: [ :stream |
			stream newLine; nextChunkPut: self classComment ]].
]

{ #category : 'private' }
BasicClassOrganizer >> hasClassComment [

	^classComment notNil and: [^classComment text notNil]
]

{ #category : 'accessing' }
BasicClassOrganizer >> hasNoComment [
	"Answer whether the class classified by the receiver has a comment."

	^classComment == nil
]

{ #category : 'accessing' }
BasicClassOrganizer >> hasSubject [
	^ self subject notNil
]

{ #category : 'fileIn/Out' }
BasicClassOrganizer >> objectForDataStream: refStrm [
	| dp |
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	self hasSubject ifTrue: [
		(self subject class isMeta) ifTrue: [
			dp := DiskProxy global: self subject name selector: #organization args: #().
			refStrm replace: self with: dp.
			^ dp]].
	^ self	"in desparation"

]

{ #category : 'copying' }
BasicClassOrganizer >> postCopy [
	super postCopy.

	"I guess not..."
	"subject := subject copy."

	classComment := classComment copy.
	commentStamp := commentStamp copy
]

{ #category : 'fileIn/Out' }
BasicClassOrganizer >> putCommentOnFile: aFileStream numbered: sourceIndex moveSource: moveSource forClass: aClass [
	"Store the comment about the class onto file, aFileStream."
	| header |
	self hasClassComment ifTrue: [
		aFileStream newLine; nextPut: $!.
		header := String streamContents: [:strm | 
				strm nextPutAll: aClass name;
				nextPutAll: ' commentStamp: '.
				commentStamp ifNil: [commentStamp := '<historical>'].
				commentStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: '0'].
		aFileStream nextChunkPut: header.
		aClass organization fileOutCommentOn: aFileStream
				moveSource: moveSource toFile: sourceIndex.
		aFileStream newLine]
]

{ #category : 'private' }
BasicClassOrganizer >> setSubject: aClassDescription [
	subject := aClassDescription
]

{ #category : 'accessing' }
BasicClassOrganizer >> subject [
	^ subject.
]
