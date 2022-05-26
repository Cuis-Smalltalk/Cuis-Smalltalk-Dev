'From Cuis 6.0 [latest update: #5178] on 20 May 2022 at 11:19:02 am'!

!PositionableStream methodsFor: 'filein/out' stamp: 'jmv 5/20/2022 11:06:18'!
copyPreamble: preamble oldPreamble: oldPreamble
	"Look for a changeStamp for this method.
	Write a method preamble, with that stamp if found."
	| terminator stamp i |
	terminator := $!!.

	"Look back to find stamp in old preamble, such as...
	Polygon methodsFor: 'private' stamp: 'di 6/25/97 21:42' prior: 34957598!! "
	stamp := String new.
	(i := oldPreamble 
		findLastOccurrenceOfString: 'stamp:'
		startingAt: 1) > 0 ifTrue: 
		[ stamp := (oldPreamble 
			copyFrom: i + 8
			to: oldPreamble size) copyUpTo: $' ].

	"Write the new preamble, with old stamp if any."
	self
		newLine;
		nextPut: terminator.
	self nextChunkPut: (String streamContents: 
			[ :strm | 
			strm nextPutAll: preamble.
			stamp size > 0 ifTrue: 
				[ strm
					nextPutAll: ' stamp: ';
					print: stamp ] ]).
	self newLine! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 5/20/2022 11:11:11'!
printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.  If moveSource true, then also set the source code pointer of the method."
	| preamble compiledMethod oldPos newPos sourceFile endPos |
	doPreamble 
		ifTrue: [preamble _ self name , ' methodsFor: ' ,
					(self organization categoryOfElement: selector) asPlainString printString]
		ifFalse: [preamble _ ''].
	compiledMethod _ self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ outStream].

	((compiledMethod fileIndex = 0
		or: [(SourceFiles at: compiledMethod fileIndex) == nil])
		or: [(oldPos _ compiledMethod filePosition) = 0])
	ifTrue: [
		"The source code is not accessible.  We must decompile..."
		preamble size > 0 ifTrue: [ outStream newLine; nextPut: $!!; nextChunkPut: preamble; newLine].
		outStream nextChunkPut: compiledMethod decompileString]
	ifFalse: [
		sourceFile _ SourceFiles at: compiledMethod fileIndex.
		preamble size > 0
			ifTrue: [   "Copy the preamble"
				outStream copyPreamble: preamble oldPreamble: compiledMethod getPreamble ].
		sourceFile position: oldPos.
		"Copy the method chunk"
		fileIndex = 0 ifFalse: [
			outStream padTo: SourceFiles pointerScaleForWriting put: $  ].
		newPos _ outStream position.
		outStream copyMethodChunkFrom: sourceFile.
		moveSource ifTrue: [    "Set the new method source pointer"
			endPos _ outStream position.
			compiledMethod checkOKToAdd: endPos - newPos at: newPos in: compiledMethod fileIndex.
			compiledMethod setSourcePosition: newPos inFile: fileIndex]].
	preamble notEmpty ifTrue: [ outStream nextChunkPut: ' ' ].
	^ outStream newLine! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 5/20/2022 11:14:03'!
getPreamble
	| file preamble |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file _ SourceFiles at: self fileIndex.
	file ifNil: [^ ''].  "sources file not available"
	"file does not exist happens in secure mode"
	[
		file name asFileEntry readStreamDo: [ :stream | | p |
			preamble _ ''.
			p _ 0 max: self filePosition.
			p < stream size ifTrue: [
				stream position: p.
				"Skip back blank space."
				stream backChunk.
				"Find and answer preamble chunk."
				preamble _ stream backChunk ]]
	] on: FileDoesNotExistException do: [ :ex | preamble _ '' ].
	^ preamble! !

!methodRemoval: PositionableStream #copyPreamble:from:at: stamp: 'jmv 5/20/2022 11:18:45'!
PositionableStream removeSelector: #copyPreamble:from:at:!
!methodRemoval: CompiledMethod #sourceFileStream stamp: 'jmv 5/20/2022 11:18:45'!
CompiledMethod removeSelector: #sourceFileStream!
!methodRemoval: CompiledMethod #getPreambleFrom:at: stamp: 'jmv 5/20/2022 11:18:45'!
CompiledMethod removeSelector: #getPreambleFrom:at:!
!methodRemoval: CompiledMethod #sourceClass stamp: 'jmv 5/20/2022 11:18:45'!
CompiledMethod removeSelector: #sourceClass!
