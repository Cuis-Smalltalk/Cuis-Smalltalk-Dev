'From Cuis7.5 [latest update: #7576] on 16 September 2025 at 11:08:10 am'!

!ClassBuilder methodsFor: 'validation' library: #MinimalKernel05ClassAndMethodCreation stamp: 'jmv 9/16/2025 11:07:52'!
checkInstSize: instSize errorMessage: errorMessage
		
	instSize > self class maxInstSize ifTrue:
		[self error: errorMessage.
		 ^false].
	Smalltalk isDevelopmentEnvironmentPresent ifTrue:
		[instSize > self class maxAccessibleInstSize ifTrue:
			[^self confirm: 'To access ', instSize printString, ' instance variables\requires the SistaV1 bytecode set.\See #preferredBytecodeSetEncoderClass:.' withNewLines]].
	^true! !

