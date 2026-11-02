'From Cuis7.9 [latest update: #8306] on 2 November 2026 at 12:02:18 pm'!

!SmallInteger methodsFor: 'printing' stamp: 'jmv 2/Nov/2026, 3:00:25 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
printOn: aStream
	"Simplified version for SmallInteger and base 10.
	Needed for #MinimalKernel01, but doesn't hurt in the full system,
	as behavior is the same as inherited from Number."

	| base n numberOfDigits totalLength divisor |
	base := 10.
	self < 0
		ifTrue: [
			n := self negated.
			aStream nextPut: $- .
			totalLength := 1 ]
		ifFalse: [
			n := self.
			totalLength := 0 ].
	numberOfDigits := n decimalDigitLength.
	totalLength := totalLength + numberOfDigits.
	divisor := (base raisedToInteger: numberOfDigits - 1).
	[ divisor > 0 ] whileTrue: [
		| digit |
		digit := n // divisor.
		aStream nextPut: ('0123456789' at: digit + 1).
		n := n - (digit * divisor).
		divisor := divisor // base ]! !


!ProtoObject methodsFor: 'error handling' stamp: 'jmv 22/May/2026, 6:43:52 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
doesNotUnderstand: aMessage
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"
	| exception context count |

	"Use the MessageNotUnderstood exception if present"
	exception := Exception deepSubclassNamed: 'MessageNotUnderstood'.
	exception notNil ifTrue:
		[ ^ exception new 
			message: aMessage;
			receiver: self;
			signal ].

	"Or do the best we can in a bare bones system"
	StdIOWriteStream stdout flush.
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Message not understood.'; newLine;
		nextPutAll: 'Receiver: '.
	self printOn: StdIOWriteStream stdout.
	StdIOWriteStream stdout
		nextPutAll: ' selector: #';
		nextPutAll: aMessage selector; newLine;
		nextPutAll: '------ Call Stack: '; newLine;
		flush.

	context := thisContext sender.
	count := 0.
	[context notNil and: [ count < 20 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit now.';
		newLine;
		flush.

	Smalltalk quitPrimitive: 127.! !


!Integer methodsFor: 'enumerating' stamp: 'ST-80 31/May/1983, 12:10:35 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by
	the receiver."

	| count |
	count := 1.
	[count <= self]
		whileTrue: 
			[aBlock value.
			count := count + 1]! !

