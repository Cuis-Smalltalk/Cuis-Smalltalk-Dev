'From Cuis 6.0 [latest update: #5377] on 7 July 2022 at 9:19:09 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'jmv 7/7/2022 20:14:13'!
changeImageNameTo: aString
	self imageName: aString asByteString.
	LastImageName _ self imageName! !


!Parser methodsFor: 'primitives' stamp: 'jmv 7/7/2022 20:36:18'!
primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil
	"Create named primitive with optional error code."
	
	(aNameString isString and: [ aModuleStringOrNil isNil or: [ aModuleStringOrNil isString ] ])
		ifFalse: [ ^ self expected: 'Named primitive' ].
	self allocateLiteral: (Array 
		with: (aModuleStringOrNil isNil 
			ifFalse: [ aModuleStringOrNil asByteString asSymbol ])
		with: aNameString asByteString asSymbol
		with: 0 with: 0).
	errorCodeVariableOrNil ifNotNil:
		[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
	^117! !

