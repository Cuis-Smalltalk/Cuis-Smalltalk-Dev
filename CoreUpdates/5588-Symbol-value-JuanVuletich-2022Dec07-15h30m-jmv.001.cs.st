'From Cuis 6.0 [latest update: #5583] on 7 December 2022 at 3:37:46 pm'!

!Symbol methodsFor: 'evaluating' stamp: 'jmv 12/7/2022 15:36:29'!
value: anObject 
	self numArgs = 0 ifFalse: [
		^ self error: 'incorrect number of arguments'		].
	^anObject perform: self! !


!UnicodeSymbol methodsFor: 'evaluating' stamp: 'jmv 12/7/2022 15:36:39'!
value: anObject 
	self numArgs = 0 ifFalse: [
		^ self error: 'incorrect number of arguments'		].
	^anObject perform: self! !


!Object methodsFor: 'message handling' stamp: 'jmv 11/29/2022 17:59:32'!
perform: aSymbol 
	"Send the unary selector, aSymbol, to the receiver.
	Fail if the number of arguments expected by the selector is not zero.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83 error: errorCode>
	self error: errorCode.! !

!Object methodsFor: 'message handling' stamp: 'jmv 11/29/2022 17:59:28'!
perform: aSymbol with: anObject 
	"Send the selector, aSymbol, to the receiver with anObject as its argument.
	Fail if the number of arguments expected by the selector is not one.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83 error: errorCode>
	self error: errorCode.! !

!Object methodsFor: 'message handling' stamp: 'jmv 11/29/2022 17:59:25'!
perform: aSymbol with: firstObject with: secondObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not two.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83 error: errorCode>
	self error: errorCode.! !

!Object methodsFor: 'message handling' stamp: 'jmv 11/29/2022 17:59:20'!
perform: aSymbol with: firstObject with: secondObject with: thirdObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not three.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83 error: errorCode>
	self error: errorCode.! !

!Object methodsFor: 'message handling' stamp: 'jmv 11/29/2022 17:59:16'!
perform: selector withArguments: argArray 
	"Send the selector, aSymbol, to the receiver with arguments in argArray.
	Fail if the number of arguments expected by the selector 
	does not match the size of argArray.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 84 error: errorCode>
	self error: errorCode.! !

!Object methodsFor: 'message handling' stamp: 'jmv 12/7/2022 15:35:16'!
perform: selector withArguments: argArray inSuperclass: lookupClass
	"NOTE:  This is just like perform:withArguments:, except that
	the message lookup process begins, not with the receivers's class,
	but with the supplied superclass instead.  It will fail if lookupClass
	cannot be found among the receiver's superclasses.
	Primitive. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 100 error: errorCode>
	(selector isSymbol)
		ifFalse: [^ self error: 'selector argument must be a Symbol'].
	(selector numArgs = argArray size)
		ifFalse: [^ self error: 'incorrect number of arguments'].
	(self class == lookupClass or: [self class inheritsFrom: lookupClass])
		ifFalse: [^ self error: 'lookupClass is not in my inheritance chain'].
	self error: errorCode.! !


!Symbol reorganize!
('accessing' at:put: precedence replaceFrom:to:with:startingAt:)
('comparing' =)
('copying' shallowCopy)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('printing' print storeOn:)
('system primitives' flushCache)
('private' errorNoModification species string:)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isSymbol isTestSelector isUnary isValidBinarySelector isValidKeywordSelector isValidSelector isValidUnarySelector testSelectorPrefix)
('uCompletion' separateKeywords)
('evaluating' value:)
!


!UnicodeSymbol reorganize!
('accessing' precedence)
('comparing' =)
('copying' shallowCopy)
('converting' asPlainString asString asSymbol capitalized numArgs: uncapitalized)
('testing' isInfix isInitializePvtSelector isKeyword isPvtSelector isSymbol isTestSelector isUnary isValidBinarySelector isValidKeywordSelector isValidSelector isValidUnarySelector testSelectorPrefix)
('printing' isLiteral print storeOn:)
('uCompletion' separateKeywords)
('private' species)
('system primitives' flushCache)
('object serialization' comeFullyUpOnReload:)
('evaluating' value:)
!

