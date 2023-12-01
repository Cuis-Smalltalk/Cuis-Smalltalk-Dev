'From Cuis6.0 [latest update: #6111] on 30 November 2023 at 6:53:58 pm'!

!PreferenceSet methodsFor: 'accessing' stamp: 'jmv 11/30/2023 18:48:15'!
at: symbolName ifAbsentAddDescription: aString category: categorySymbol type: aType value: aValue
	^ (contents
		at: symbolName ifAbsent: [
			self
				name: symbolName
				description: aString
				category: categorySymbol
				type: aType
				value: aValue ]) value! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 11/30/2023 18:51:06'!
printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.  If moveSource true, then also set the source code pointer of the method."
	| preamble compiledMethod oldPos newPos sourceFile endPos methodCode codeStyler |
	doPreamble 
		ifTrue: [preamble := self name , ' methodsFor: ' ,
					(self organization categoryOfElement: selector) asPlainString printString]
		ifFalse: [preamble := ''].
	compiledMethod := self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ outStream].

	((compiledMethod fileIndex = 0
		or: [(SourceFiles at: compiledMethod fileIndex) == nil])
		or: [(oldPos := compiledMethod filePosition) = 0])
	ifTrue: [
		"The source code is not accessible.  We must decompile..."
		preamble size > 0 ifTrue: [ outStream newLine; nextPut: $!!; nextChunkPut: preamble; newLine].
		outStream nextChunkPut: compiledMethod decompileString]
	ifFalse: [
		sourceFile := SourceFiles at: compiledMethod fileIndex.
		preamble size > 0
			ifTrue: [   "Copy the preamble"
				outStream copyPreamble: preamble oldPreamble: compiledMethod getPreamble ].
		sourceFile position: oldPos.
		"Copy the method chunk"
		fileIndex = 0 ifFalse: [
			outStream padTo: SourceFiles pointerScaleForWriting put: $  ].
		newPos := outStream position.
		"Copy method chunk"
		methodCode := sourceFile nextChunk.

		"Convert method code to use ANSI assignments"
		(Preferences at: #fileOutANSIassignment) ifTrue: [
			codeStyler := SHTextStylerST80 new.
			codeStyler classOrMetaClass: compiledMethod methodClass.
			codeStyler instVarNamed: 'formattedText' put: methodCode asText.
			codeStyler getReady.
			codeStyler parseText.
			codeStyler replaceStringForRangesWithType: #assignment with: ':=' offset: 0.
			methodCode := codeStyler convertedAndStyledText string ].

		outStream nextChunkPut: methodCode.
		moveSource ifTrue: [    "Set the new method source pointer"
			endPos := outStream position.
			compiledMethod checkOKToAdd: endPos - newPos at: newPos in: compiledMethod fileIndex.
			compiledMethod setSourcePosition: newPos inFile: fileIndex]].
	preamble notEmpty ifTrue: [ outStream nextChunkPut: ' ' ].
	^ outStream newLine! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 11/30/2023 18:53:53'!
saveNumberedRelease
	"Save the image/changes using the next release version number."
	"
	Smalltalk saveNumberedRelease
	"
	| versionMajor versionMinor previousValue |
	versionMajor := 6.
	versionMinor := 1.
	SystemVersion newVersionMajor: versionMajor versionMinor: versionMinor.
	"While condensing sources, convert all variable assignment to ANSI Smalltalk form, i.e. :="
	previousValue := Preferences at: #fileOutANSIassignment.
	Preferences at: #fileOutANSIassignment put: true.
	Smalltalk condenseSources: SystemVersion current versionString.
	Preferences at: #fileOutANSIassignment put: previousValue.
	self setDefaultGCParameters.

	(self filenameForUpdatedImage: false) ifNotNil: [ :newName |
		SystemVersion current beNumberedRelease.
		"Try to clear all user state, including all class vars, preferences, etc"
		self saveAndStayAs: newName clearAllClassState: true ].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

Preferences at: #fileOutANSIassignment ifAbsentAddDescription: 'Whether assignments are converted to ANSI style :=' category: #programming type: Boolean value: false!

