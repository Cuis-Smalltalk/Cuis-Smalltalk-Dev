'From Cuis7.5 [latest update: #7443] on 27 August 2025 at 11:09:55 am'!

!ChangeRecord methodsFor: 'accessing' stamp: 'jmv 8/27/2025 11:07:10'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	type == #method
		ifTrue: [
			self changeClass printCategoryChunk: category on: aFileStream
				withStamp: stamp libraryName: libraryName priorMethod: nil.
			aFileStream newLine ].

	type == #preamble ifTrue: [ aFileStream nextPut: $!! ].

	type == #classComment
		ifTrue: [
			aFileStream nextPut: $!!.
			aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.
			aFileStream newLine ].
		
	type == #classDefinition ifTrue: [
		aFileStream nextPut: $!!.
		aFileStream nextChunkPut: 
			'classDefinition: ', 
			(self isMetaClassChange ifTrue: [self changeClassName, ' class'] ifFalse: [self changeClassName]) printString,
			' category: ', self category printString.
		aFileStream newLine ].

	aFileStream nextChunkPut: self string.
	
	type == #method ifTrue: [ aFileStream nextChunkPut: ' '; newLine ].
	type == #classComment ifTrue: [ aFileStream newLine ].
	aFileStream newLine.! !

