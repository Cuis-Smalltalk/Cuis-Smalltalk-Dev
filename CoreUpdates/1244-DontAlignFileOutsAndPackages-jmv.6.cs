'From Cuis 4.0 of 3 April 2012 [latest update: #1241] on 4 April 2012 at 4:04:56 pm'!

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 4/4/2012 15:47'!
printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.  If moveSource true, then also set the source code pointer of the method."
	| preamble method oldPos newPos sourceFile endPos |
	doPreamble 
		ifTrue: [preamble _ self name , ' methodsFor: ' ,
					(self organization categoryOfElement: selector) asString printString]
		ifFalse: [preamble _ ''].
	method _ self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR!!  THIS SCRIPT IS MISSING ** '; newLine; newLine.
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
		sourceFile skipSeparators.      "The following chunk may have ]style["
		sourceFile peek == $] ifTrue: [
			outStream newLine; copyMethodChunkFrom: sourceFile].
		moveSource ifTrue: [    "Set the new method source pointer"
			endPos _ outStream position.
			method checkOKToAdd: endPos - newPos at: newPos in: method fileIndex.
			method setSourcePosition: newPos inFile: fileIndex]].
	preamble size > 0 ifTrue: [ outStream nextChunkPut: ' ' ].
	^ outStream newLine! !


!RemoteString methodsFor: 'private' stamp: 'jmv 4/4/2012 15:47'!
string: aStringOrText onFileNumber: fileNumber toFile: aFileStream
	"Store this as the receiver's text if source files exist."

	| position |
	fileNumber = 0 ifFalse: [
		aFileStream padTo: SourceFiles pointerScaleForWriting put: $  ].
	position _ aFileStream position.
	self fileNumber: fileNumber position: position.
	aFileStream nextChunkPut: aStringOrText asString! !

