'From Cuis7.5 [latest update: #7444] on 27 August 2025 at 10:27:15 am'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/27/2025 10:26:42'!
printCategoryChunk: category on: aFileStream withStamp: changeStamp libraryName: aStringOrNil priorMethod: priorMethod
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	If it has a prior source link, it MUST have a stamp, even if it is empty.
	It may also have a libraryName."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		aStringOrNil ifNotNil: [
			strm nextPutAll: ' library: '; print: aStringOrNil ].
		(changeStamp notNil and: [
			changeStamp notEmpty or: [priorMethod notNil]]) ifTrue: [
			strm nextPutAll: ' stamp: '; print: changeStamp].
		priorMethod notNil ifTrue: [
			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer].
		]).! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 8/27/2025 09:42:21'!
getPreamble
	| file preamble |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file := SourceFiles at: self fileIndex.
	file ifNil: [^ ''].  "sources file not available"
	"file does not exist happens in secure mode"
	[
		file name asFullFileEntry readStreamDo: [ :stream | | p |
			preamble := ''.
			p := 0 max: self filePosition.
			p < stream size ifTrue: [
				"It appears that on Linux systems, immediately after adding a
				new method, the OS file cache may not be updated, so contents
				may still not be accessible. Protect against that."
				stream position: p.
				"Skip back blank space."
				stream backChunk.
				"Find and answer preamble chunk."
				preamble := stream backChunk ]]
	] on: FileDoesNotExistException do: [ :ex | preamble := '' ].
	^ preamble! !

