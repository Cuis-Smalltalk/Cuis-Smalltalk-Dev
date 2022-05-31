'From Cuis 6.0 [latest update: #5204] on 30 May 2022 at 11:02:28 am'!

!ByteArray methodsFor: 'testing' stamp: 'jmv 5/26/2022 16:04:32'!
is: aSymbol
	^#ByteArray = aSymbol or: [ super is: aSymbol ]! !


!Stream methodsFor: 'testing' stamp: 'jmv 5/30/2022 10:53:58'!
isWriteOnly
	"To be redefined in subclasses that can't do #next"
	^ false! !


!ReadStream methodsFor: 'accessing' stamp: 'jmv 5/28/2022 17:15:19'!
next: anInteger putAll: aString startingAt: startIndex
	self shouldNotImplement! !

!ReadStream methodsFor: 'accessing' stamp: 'jmv 5/28/2022 17:16:40'!
nextPutAll: aCollection
	self shouldNotImplement! !


!WriteStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 10:57:04'!
next: n
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super next: n! !

!WriteStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 10:58:13'!
next: n into: aCollection startingAt: startIndex
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super next: n into: aCollection startingAt: startIndex! !

!WriteStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 10:59:06'!
nextWordsInto: aBitmap
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super nextWordsInto: aBitmap! !

!WriteStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 10:59:34'!
peek
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super peek! !

!WriteStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 10:59:58'!
upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
	self isWriteOnly ifTrue: [
		self error: 'This is a Write-Only Stream' ].
	^super upTo: delimiter delimiterIsTerminator: delimiterIsTerminator! !

!WriteStream methodsFor: 'testing' stamp: 'jmv 5/30/2022 10:54:20'!
isWriteOnly
	^true! !


!ReadWriteStream methodsFor: 'testing' stamp: 'jmv 5/30/2022 10:54:37'!
isWriteOnly
	^false! !


!TextModel methodsFor: 'undoable commands' stamp: 'jmv 5/30/2022 10:15:38'!
logUndoAndReplaceFrom: start to: stop with: replacement
	"As requested."

	| command now |
	"Time millisecondClockValue rolls over and is generally not adviced.
	But here, we don't care. A user edit doing during rollover would be split  in two, as if the user did a pause.
	Not a problem."
	
	now _ Time millisecondClockValue.
	command _ self commandForReplaceFrom: start to: stop with: replacement.
	(stop+1 = start and: [ lastEditTimeStamp notNil and: [ now - lastEditTimeStamp < 1000 and: [start = undoRedoCommands peekLast stopPosition] ]])
		ifTrue: [
			"Don't use the command we just built"
			undoRedoCommands peekLast appendToNew: replacement
			]
		ifFalse: [
			undoRedoCommands
				nextPut: command;
				truncateAtPosition.	"To disable redo of previous commands, now invalidated."
			].
	command doOn: self.
	lastEditTimeStamp _ now! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 10:18:25'!
untilAnySatisfying: aBlock
	| c |
	^self collectionSpecies streamContents: [ :write |
		[
			self atEnd not and: [
				c := self peek.
				(aBlock value: c) not]]
			whileTrue: [write nextPut: self next]].! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 5/30/2022 10:28:24'!
debugIt

	self lineSelectAndEmptyCheck: [^self].
	self 
		afterCompiling: self selection
		do: [ :compiler :method :receiver :context | method ifNotNil: [ self debug: method receiver: receiver in: context ]]
		ifFail: [].! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 5/30/2022 10:28:37'!
evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile
	"Treat the current selection as an expression; evaluate it and return the result
	3 +4
	"	
	
	self lineSelectAndEmptyCheck: [^ ''].
	^self evaluate: self selection andDo: aBlock ifFail: failBlock profiled: doProfile! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 5/30/2022 10:43:50'!
fileItIn
	"Make a Stream on the text selection and fileIn it."

	self selection fileIn! !

!methodRemoval: TextEditor #selectionAsStream stamp: 'jmv 5/30/2022 10:55:41'!
TextEditor removeSelector: #selectionAsStream!
!methodRemoval: RWBinaryOrTextStream #upToEnd stamp: 'jmv 5/30/2022 10:55:41'!
RWBinaryOrTextStream removeSelector: #upToEnd!
!methodRemoval: ReadWriteStream class #readingOn: stamp: 'jmv 5/30/2022 10:55:41'!
ReadWriteStream class removeSelector: #readingOn:!
!methodRemoval: WriteStream class #on:from:to: stamp: 'jmv 5/30/2022 10:55:41'!
WriteStream class removeSelector: #on:from:to:!
!methodRemoval: WriteStream #on:from:to: stamp: 'jmv 5/30/2022 10:55:41'!
WriteStream removeSelector: #on:from:to:!
!methodRemoval: WriteStream #nextPutAllString:withAttributes: stamp: 'jmv 5/30/2022 10:55:41'!
WriteStream removeSelector: #nextPutAllString:withAttributes:!
!methodRemoval: PositionableStream class #on:from:to: stamp: 'jmv 5/30/2022 10:55:41'!
PositionableStream class removeSelector: #on:from:to:!
!methodRemoval: PositionableStream #upToEnd stamp: 'jmv 5/30/2022 10:55:41'!
PositionableStream removeSelector: #upToEnd!
!methodRemoval: PositionableStream #last stamp: 'jmv 5/30/2022 10:55:41'!
PositionableStream removeSelector: #last!
!methodRemoval: Stream #upToEnd stamp: 'jmv 5/30/2022 10:55:41'!
Stream removeSelector: #upToEnd!
