'From Cuis7.9 [latest update: #8237] on 25 September 2026 at 3:20:24 pm'!

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 30/Apr/2026, 12:39:51 pm (UTC)'!
changeClassName
	^class! !

!MethodDefinitionChange methodsFor: 'accessing' stamp: 'jmv 25/Sep/2026, 6:15:32 pm (UTC)'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	| cName |
	cName := self changeClassName.
	self isMetaClassChange ifTrue: [
		cName := cName, ' class' ].
	ClassDescription
		printCategoryChunk: cName category: category on: aFileStream
		withStamp: stamp priorMethod: nil.
	properties isEmptyOrNil ifFalse: [
		ClassDescription printPropertiesChunk: properties on: aFileStream ].
	aFileStream newLine.
	aFileStream nextChunkPut: self string.
	aFileStream nextChunkPut: ' '; newLine.
	aFileStream newLine.! !

