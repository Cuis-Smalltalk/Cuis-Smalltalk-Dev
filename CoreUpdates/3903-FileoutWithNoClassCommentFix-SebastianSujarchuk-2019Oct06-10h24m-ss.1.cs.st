'From Cuis 5.0 [latest update: #3901] on 6 October 2019 at 10:24:55 am'!

!BasicClassOrganizer methodsFor: 'private' stamp: 'ss 10/3/2019 19:59:34'!
hasClassComment

	^classComment notNil and: [^classComment text notNil]! !


!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'ss 10/3/2019 19:59:25'!
fileOutCommentOn: aFileStream moveSource: moveSource toFile: fileIndex
	"Copy the class comment to aFileStream.  If moveSource is true (as in compressChanges or compressSources, then update classComment to point to the new file."
	| fileComment |
	self hasClassComment ifTrue: [
		aFileStream newLine.
		fileComment _ RemoteString newString: self classComment
						onFileNumber: fileIndex toFile: aFileStream.
		moveSource ifTrue: [classComment _ fileComment].
		Smalltalk defaultUserChangesName asFileEntry appendStreamDo: [ :stream |
			stream newLine; nextChunkPut: self classComment ]]! !

!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'ss 10/3/2019 19:58:31'!
putCommentOnFile: aFileStream numbered: sourceIndex moveSource: moveSource forClass: aClass
	"Store the comment about the class onto file, aFileStream."
	| header |
	self hasClassComment ifTrue: [
		aFileStream newLine; nextPut: $!!.
		header _ String streamContents: [:strm | 
				strm nextPutAll: aClass name;
				nextPutAll: ' commentStamp: '.
				commentStamp ifNil: [commentStamp _ '<historical>'].
				commentStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: '0'].
		aFileStream nextChunkPut: header.
		aClass organization fileOutCommentOn: aFileStream
				moveSource: moveSource toFile: sourceIndex.
		aFileStream newLine]! !

