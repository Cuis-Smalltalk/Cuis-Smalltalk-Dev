'From Cuis 6.0 [latest update: #5994] on 4 September 2023 at 10:35:23 am'!

!CompiledMethod methodsFor: 'testing' stamp: 'jmv 9/4/2023 09:52:00'!
isLinkedNamedPrimitive
	"Answer if the receiver invokes a named primitive, and the method is linked to an actual primitive.
	 For example if the method hasn't yet been used in the current session, it won't be linked
	For additional details, In VMMaker package check references to 
	ExternalCallLiteralFlagsIndex
	ExternalCallLiteralTargetFunctionIndex
	(Apparently -1 means failure, most likely named primitive not found)
	"
	^self isNamedPrimitive and: [(self literalAt: 1) fourth ~= 0]
	
	"
	Smalltalk browseAllSelect: [:m| m isLinkedNamedPrimitive]
	"! !

!CompiledMethod methodsFor: 'testing' stamp: 'jmv 9/4/2023 09:26:17'!
isNamedPrimitive
	"Answer if the receiver invokes a named primitive."
	^self primitive = 117
	
	"
	Smalltalk browseAllSelect: [:m| m isNamedPrimitive]
	"! !

