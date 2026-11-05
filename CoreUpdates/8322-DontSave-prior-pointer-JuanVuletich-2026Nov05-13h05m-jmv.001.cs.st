'From Cuis7.9 [latest update: #8321] on 5 November 2026 at 1:06:43 pm'!

!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 5/Nov/2026, 3:43:11 pm (UTC)'!
putCommentOnFile: aFileStream numbered: sourceIndex forClass: aClass
	"Store the comment about the class onto file, aFileStream."
	| header |
	self hasClassComment ifTrue: [
		aFileStream newLine; nextPut: $!!.
		header := String streamContents: [:strm | 
				strm nextPutAll: aClass name;
				nextPutAll: ' commentStamp: '.
				commentStamp ifNil: [commentStamp := '<historical>'].
				commentStamp storeOn: strm ].
		aFileStream nextChunkPut: header.
		aClass organization fileOutCommentOn: aFileStream toFile: sourceIndex.
		aFileStream newLine ].! !

