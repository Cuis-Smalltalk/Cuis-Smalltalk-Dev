'From Cuis 5.0 of 7 November 2016 [latest update: #3145] on 28 August 2017 at 1:59:42 pm'!

!CompiledMethod class methodsFor: 'method encoding' stamp: 'HAW 8/28/2017 13:59:31'!
headerFlagForEncoder: anEncoder
	
	(anEncoder class includesBehavior: PrimaryBytecodeSetEncoderClass) ifTrue: [^0].
	(anEncoder class includesBehavior: SecondaryBytecodeSetEncoderClass) ifTrue: [^SmallInteger minVal].
	
	self error: 'The encoder is not one of the two installed bytecode sets'! !

