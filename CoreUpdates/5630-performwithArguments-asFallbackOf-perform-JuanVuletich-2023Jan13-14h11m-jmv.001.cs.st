'From Cuis 6.0 [latest update: #5629] on 13 January 2023 at 2:12:14 pm'!

!Object methodsFor: 'message handling' stamp: 'di 3/26/1999 07:52'!
perform: aSymbol 
	"Send the unary selector, aSymbol, to the receiver.
	Fail if the number of arguments expected by the selector is not zero.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array new: 0)! !

!Object methodsFor: 'message handling' stamp: 'di 3/26/1999 07:52'!
perform: aSymbol with: anObject 
	"Send the selector, aSymbol, to the receiver with anObject as its argument.
	Fail if the number of arguments expected by the selector is not one.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: anObject)! !

!Object methodsFor: 'message handling' stamp: 'di 3/26/1999 07:52'!
perform: aSymbol with: firstObject with: secondObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not two.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: (Array with: firstObject with: secondObject)! !

!Object methodsFor: 'message handling' stamp: 'di 3/26/1999 07:51'!
perform: aSymbol with: firstObject with: secondObject with: thirdObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not three.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol
		withArguments: (Array with: firstObject with: secondObject with: thirdObject)! !

