'From Cuis7.9 [latest update: #8286] on 26 October 2026 at 3:21:31 pm'!

!Object methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 6:08:15 pm (UTC)'!
limitsPrintLength
	"Most objects fully print themselves to a reasonable sized answers.
	Some won't, and require an external limit. See implementors and senders."

	^false! !


!Collection methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 6:09:11 pm (UTC)'!
limitsPrintLength
	"Very large collections could answer extremely long and useless #printString and #printText.
	Limit the size of the answer. See senders."

	^true! !


!Object methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 6:05:13 pm (UTC)'!
printForInspectorOn: aStream limitedTo: sizeLimit indent: indent
	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  Limit is the length limit for each inst var."

	self class allInstVarNames withIndexDo: [ :title :index |
		indent timesRepeat: [aStream tab].
		aStream nextPutAll: title;
		 nextPut: $:;
		 space;
		 tab;
		 nextPutAll: 
			((self instVarAt: index) printAs: Text limitedTo: (sizeLimit - 3 - title size max: 5));
		 newLine ]! !

!Object methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 6:06:15 pm (UTC)'!
printString
	"Answer a String whose characters are a description of the receiver. 
	If you want to print without a character limit, use fullPrintString.

	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.

	Usually you should not reimplement this method in subclasses, but #printOn:

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self limitsPrintLength
		ifTrue: [ self printAs: String limitedTo: 50000 ]
		ifFalse: [ String streamContents: [:str | self printOn: str ]]! !

!Object methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 6:05:23 pm (UTC)'!
printText

	^ self limitsPrintLength
		ifTrue: [ self printAs: Text limitedTo: 50000 ]
		ifFalse: [ Text streamContents: [:str | self printOn: str ]]! !

!Object methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 6:06:23 pm (UTC)'!
shortPrintString
	"Answer a String whose characters are a description of the receiver.
	This is a short one, good for showing users (for example, in ObjectExplorers).
	If you want to print without a character limit, use fullPrintString.

	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.

	Usually you should not reimplement this method in subclasses, but #printOn:

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printAs: String limitedTo: 128.! !


!MethodNode methodsFor: 'converting' stamp: 'jmv 26/Oct/2026, 6:03:03 pm (UTC)'!
decompileString 
	"Answer a string description of the parse tree whose root is the receiver."

	^self printString! !

!MethodNode methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 6:03:09 pm (UTC)'!
sourceText

	^ sourceText ifNil: [self printString]! !


!ContextPart methodsFor: 'debugger access' stamp: 'jmv 26/Oct/2026, 5:54:56 pm (UTC)'!
tempsAndValuesLimitedTo: sizeLimit indent: indent
	"Return a string of the temporary variables and their current values"

	| tempNames title |
	^ String streamContents: [ :strm |
		tempNames := self tempNames.
		1 to: self size do: [ :index | | tempString limit |
			title := tempNames size = self size ifTrue: [tempNames at: index]  ifFalse: [ 'argOrTemp', index printString ].
			indent timesRepeat: [strm tab].
			strm nextPutAll: title; nextPut: $:; space; tab.
			tempString := (self tempAt: index) printString.
			limit := (sizeLimit -3 -title size max: 5).
			tempString size > limit ifTrue: [
				tempString := tempString first: limit ].
			strm nextPutAll: tempString.
			strm newLine ]].! !


!Inspector methodsFor: 'selecting' stamp: 'jmv 26/Oct/2026, 6:05:32 pm (UTC)'!
selectionPrintString
	"Returns the current selection as a string"
	
	^self shouldAssumeSelectionIsPrinted 
		ifTrue: [ self selection ]
		ifFalse: [ self safelyPrintWith: [ self selection printAs: Text limitedTo: self printStringLimit ] ].! !


!Integer methodsFor: 'printing' stamp: 'jmv 26/Oct/2026, 5:44:37 pm (UTC)'!
printAsLiteralOn: aStream
	^self printOn: aStream! !


!Morph methodsFor: 'menus' stamp: 'jmv 26/Oct/2026, 5:52:30 pm (UTC)'!
exportAsBMP
	"Export the receiver's image as a BMP."
	self request: 'Enter file name'
		initialAnswer: (self printString squeezedTo: 20),'.bmp'
		do: [:aString| (self imageForm: 32) writeBMPfileNamed: aString]! !

!Morph methodsFor: 'menus' stamp: 'jmv 26/Oct/2026, 5:52:20 pm (UTC)'!
exportAsJPEG
	"Export the receiver's image as a JPEG."
	self request: 'Enter file name'
		initialAnswer: (self printString squeezedTo: 20),'.jpeg'
		do: [:aString| (self imageForm: 32) writeJPEGfileNamed: aString]! !

!methodRemoval: Color #printString stamp: 'jmv 26/Oct/2026, 6:20:59 pm (UTC)'!
Color removeSelector: #printString!
!methodRemoval: Number #printString stamp: 'jmv 26/Oct/2026, 6:20:59 pm (UTC)'!
Number removeSelector: #printString!
!methodRemoval: Object #printTextLimitedTo: stamp: 'jmv 26/Oct/2026, 6:20:59 pm (UTC)'!
Object removeSelector: #printTextLimitedTo:!
!methodRemoval: Object #printStringLimitedTo: stamp: 'jmv 26/Oct/2026, 6:20:59 pm (UTC)'!
Object removeSelector: #printStringLimitedTo:!
