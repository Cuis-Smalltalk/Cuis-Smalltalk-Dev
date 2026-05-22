'From Cuis7.7 [latest update: #7961] on 22 May 2026 at 3:44:40 pm'!

!ProtoObject methodsFor: 'error handling' library: #MinimalKernel01 stamp: 'jmv 22/May/2026 15:43:52'!
doesNotUnderstand: aMessage
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"
	| exception context count |

	"Use the MEssageNotUnderstood exception if present"
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

!ProtoObject methodsFor: 'error handling' library: #MinimalKernel01 stamp: 'jmv 22/May/2026 15:43:56'!
error: aString 
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"

	| context count |
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Error: ';
		nextPutAll: aString; newLine;
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
		nextPutAll: 'Will exit Cuis now.';
		newLine;
		flush.
	Smalltalk quitPrimitive: 128.! !

