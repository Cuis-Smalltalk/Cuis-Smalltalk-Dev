'From Cuis7.3 [latest update: #7178] on 19 May 2025 at 6:25:21 pm'!

!Behavior methodsFor: '[mk] class hierarchy' stamp: 'jmv 5/19/2025 18:21:34'!
deepSubclassNamed: aName
	"Or nil"
	self allSubclassesDo: [ :cls | cls name = aName ifTrue: [ ^cls ]].
	^nil.! !


!ProtoObject methodsFor: '[mk] error handling' stamp: 'jmv 5/19/2025 18:24:23'!
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
	[context notNil and: [ count < 10 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit now.';
		newLine;
		flush.

	Smalltalk quitPrimitive: 127.! !

