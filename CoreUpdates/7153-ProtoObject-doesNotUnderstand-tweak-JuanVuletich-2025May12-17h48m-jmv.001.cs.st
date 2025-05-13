'From Cuis7.3 [latest update: #7152] on 12 May 2025 at 5:50:58 pm'!

!ProtoObject methodsFor: '[mk] system primitives' stamp: 'jmv 5/12/2025 17:50:41'!
doesNotUnderstand: aMessage
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"
	| context count |
	StdIOWriteStream stdout flush.
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Message not understood.'; newLine;
		nextPutAll: 'Receiver: '.

	self class isMeta
		ifFalse: [ StdIOWriteStream stdout nextPutAll: 'an instance of ' ]
		ifTrue: [ StdIOWriteStream stdout nextPutAll: 'class ' ].
	self class theNonMetaClass printNameOn: StdIOWriteStream stdout.

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

