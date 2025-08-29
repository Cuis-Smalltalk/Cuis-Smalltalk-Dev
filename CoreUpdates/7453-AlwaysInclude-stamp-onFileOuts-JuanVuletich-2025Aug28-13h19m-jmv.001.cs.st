'From Cuis7.5 [latest update: #7452] on 28 August 2025 at 1:19:51 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/28/2025 13:16:26'!
printCategoryChunk: category on: aFileStream withStamp: changeStamp libraryName: aStringOrNil priorMethod: priorMethod
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	Always include a stamp, even if it is empty.
	It may also have a libraryName."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		aStringOrNil ifNotNil: [
			strm nextPutAll: ' library: '; print: aStringOrNil ].
		strm nextPutAll: ' stamp: '; print: (changeStamp ifNil: ['']).
		priorMethod notNil ifTrue: [
			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer].
		]).! !

