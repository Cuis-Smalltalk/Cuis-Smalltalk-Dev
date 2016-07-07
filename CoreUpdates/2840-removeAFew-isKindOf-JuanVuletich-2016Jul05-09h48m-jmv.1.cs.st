'From Cuis 4.2 of 25 July 2013 [latest update: #2839] on 5 July 2016 at 9:49:57.483253 am'!

!String methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:01'!
isRemote
	^false! !


!RemoteString methodsFor: 'testing' stamp: 'jmv 7/4/2016 22:00'!
isRemote
	^true! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 7/4/2016 22:02'!
classComment: aString stamp: aStamp
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| ptr header oldCommentRemoteStr |
	aString isRemote ifTrue: [
		SystemChangeNotifier uniqueInstance classCommented: self.
		^ self organization classComment: aString stamp: aStamp].

	oldCommentRemoteStr _ self organization commentRemoteStr.
	(aString size = 0) & (oldCommentRemoteStr == nil) ifTrue: [^ self organization classComment: nil].
		"never had a class comment, no need to write empty string out"

	ptr _ oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].
	SourceFiles ifNotNil: [ | file |
		(file _ SourceFiles at: 2) ifNotNil: [
			file setToEnd; newLine; nextPut: $!!.	"directly"
			"Should be saying (file command: 'H3') for HTML, but ignoring it here"
			header _ String streamContents: [:strm | strm nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
				aStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
			file nextChunkPut: header]].
	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.
	SystemChangeNotifier uniqueInstance classCommented: self.
! !


!BasicClassOrganizer methodsFor: 'accessing' stamp: 'jmv 7/4/2016 22:01'!
classComment: aString 
	"Store the comment, aString, associated with the object that refers to the 
	receiver."

	aString isRemote
		ifTrue: [classComment _ aString]
		ifFalse: [(aString == nil or: [aString size = 0])
			ifTrue: [classComment _ nil]
			ifFalse: [
				self error: 'use aClass classComment:'.
				classComment _ RemoteString newString: aString onFileNumber: 2]]
				"Later add priorSource and date and initials?"! !

!BasicClassOrganizer methodsFor: 'accessing' stamp: 'jmv 7/4/2016 22:01'!
classComment: aString  stamp: aStamp
	"Store the comment, aString, associated with the object that refers to the receiver."

	self commentStamp: aStamp.
	aString isRemote
		ifTrue: [classComment _ aString]
		ifFalse: [(aString == nil or: [aString size = 0])
			ifTrue: [classComment _ nil]
			ifFalse:
				[self error: 'use aClass classComment:'.
				classComment _ RemoteString newString: aString onFileNumber: 2]]
				"Later add priorSource and date and initials?"! !

