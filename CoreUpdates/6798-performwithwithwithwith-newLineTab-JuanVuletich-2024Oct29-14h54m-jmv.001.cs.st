'From Cuis7.1 [latest update: #6797] on 29 October 2024 at 3:03:38 pm'!

!Object methodsFor: 'message handling' stamp: 'jmv 10/29/2024 14:54:47'!
perform: aSymbol with: firstObject with: secondObject with: thirdObject with: fourth
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not three.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol
		withArguments: (Array with: firstObject with: secondObject with: thirdObject with: fourth)! !


!WriteStream methodsFor: 'character writing' stamp: 'jmv 10/29/2024 14:55:55'!
newLineTab
	"Append a newLine character, followed by a tab character, to the receiver."

	self
		nextPut: Character newLineCharacter;
		nextPut: Character tab.! !

