'From Cuis 5.0 of 7 November 2016 [latest update: #3639] on 6 March 2019 at 1:08:21 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/6/2019 13:07:59'!
printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.  If moveSource true, then also set the source code pointer of the method."
	| preamble method oldPos newPos sourceFile endPos |
	doPreamble 
		ifTrue: [preamble _ self name , ' methodsFor: ' ,
					(self organization categoryOfElement: selector) asString printString]
		ifFalse: [preamble _ ''].
	method _ self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ outStream].

	((method fileIndex = 0
		or: [(SourceFiles at: method fileIndex) == nil])
		or: [(oldPos _ method filePosition) = 0])
	ifTrue: [
		"The source code is not accessible.  We must decompile..."
		preamble size > 0 ifTrue: [ outStream newLine; nextPut: $!!; nextChunkPut: preamble; newLine].
		outStream nextChunkPut: method decompileString]
	ifFalse: [
		sourceFile _ SourceFiles at: method fileIndex.
		preamble size > 0
			ifTrue:    "Copy the preamble"
				[outStream copyPreamble: preamble from: sourceFile at: oldPos]
			ifFalse:
				[sourceFile position: oldPos].
		"Copy the method chunk"
		fileIndex = 0 ifFalse: [
			outStream padTo: SourceFiles pointerScaleForWriting put: $  ].
		newPos _ outStream position.
		outStream copyMethodChunkFrom: sourceFile.
		moveSource ifTrue: [    "Set the new method source pointer"
			endPos _ outStream position.
			method checkOKToAdd: endPos - newPos at: newPos in: method fileIndex.
			method setSourcePosition: newPos inFile: fileIndex]].
	preamble size > 0 ifTrue: [ outStream nextChunkPut: ' ' ].
	^ outStream newLine! !

