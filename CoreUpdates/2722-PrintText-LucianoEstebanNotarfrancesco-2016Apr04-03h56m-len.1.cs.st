'From Cuis 4.2 of 25 July 2013 [latest update: #2717] on 4 April 2016 at 4:03:02.463138 am'!

!Object methodsFor: 'printing' stamp: 'len 4/3/2016 02:24'!
printText
	^ Text streamContents: [:aStream| self printOn: aStream]! !


!TextEditor methodsFor: 'new selection' stamp: 'len 4/3/2016 07:41'!
insertAndSelect: aString at: anInteger
	"This is a user command, and generates undo"

	| newText |
	newText _ (aString is: #Text) ifTrue: [aString] ifFalse: [Text string: aString attributes: emphasisHere].
	self deselectAndPlaceCursorAt: anInteger.
	self replaceSelectionWith: newText.
	self selectFrom: anInteger to: anInteger + newText size - 1! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'len 4/3/2016 07:42'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	self
		evaluateSelectionAndDo: [ :result |
			self afterSelectionInsertAndSelect: ' ', result printText ]
		ifFail: [ morph flash ]! !


!Morph methodsFor: 'printing' stamp: 'len 4/3/2016 03:52'!
printOn: aStream 
	"Add the identity of the receiver to a stream"
	((aStream isKindOf: TextStream) and: [self owner isNil])
		ifTrue:
			[aStream withAttribute: (TextAnchor new anchoredFormOrMorph: self) do: [aStream nextPut: $*].
			^ self].
	super printOn: aStream. "a(n) className"
	aStream 
		nextPut: $(;
		print: self identityHash;
		nextPut: $).
	self valueOfProperty: #morphName ifPresentDo: [:x | aStream nextPutAll: x asString]! !

