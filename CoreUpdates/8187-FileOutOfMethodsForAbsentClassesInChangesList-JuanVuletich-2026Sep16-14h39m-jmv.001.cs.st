'From Cuis7.9 [latest update: #8185] on 16 September 2026 at 2:45:44 pm'!

!ClassDescription class methodsFor: 'utilities' stamp: 'jmv 16/Sep/2026, 4:54:58 pm (UTC)'!
printCategoryChunk: className category: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	Always include a stamp, even if it is empty."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: className; nextPutAll: ' methodsFor: '; print: category asString.
		strm nextPutAll: ' stamp: '; print: (changeStamp ifNil: ['']).
		priorMethod notNil ifTrue: [
			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer].
		]).! !

!ClassDescription class methodsFor: 'utilities' stamp: 'jmv 16/Sep/2026, 4:53:37 pm (UTC)'!
printPropertiesChunk: anAdditionalMethodStateOrArray on: aFileStream
	"Print a method properties chunk."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm | 
		strm nextPutAll: 'methodProperties: '.
		anAdditionalMethodStateOrArray storePropertiesOn: strm ]).
! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 16/Sep/2026, 5:04:29 pm (UTC)'!
printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod

	ClassDescription
		printCategoryChunk: self name
		category: category
		on: aFileStream
		withStamp: changeStamp
		priorMethod: priorMethod.! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 16/Sep/2026, 5:03:31 pm (UTC)'!
printPropertiesChunk: anAdditionalMethodStateOrArray on: aFileStream

	ClassDescription printPropertiesChunk: anAdditionalMethodStateOrArray on: aFileStream.! !


!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 16/Sep/2026, 5:44:19 pm (UTC)'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	ClassDescription
		printCategoryChunk: self changeClassName category: category on: aFileStream
		withStamp: stamp priorMethod: nil.
	properties isEmptyOrNil ifFalse: [
		ClassDescription printPropertiesChunk: properties on: aFileStream ].
	aFileStream newLine.
	aFileStream nextChunkPut: self string.
	aFileStream nextChunkPut: ' '; newLine.
	aFileStream newLine.! !

