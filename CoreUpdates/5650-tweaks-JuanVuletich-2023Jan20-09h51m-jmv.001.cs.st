'From Cuis 6.0 [latest update: #5636] on 20 January 2023 at 10:03:44 am'!

!Parser methodsFor: 'primitives' stamp: 'jmv 8/22/2021 14:01:58'!
externalFunctionDeclaration
	"Parse the function declaration for a call to an external library."
	| descriptorClass callType modifier retType externalName args argType module fn |
	descriptorClass := Smalltalk at: #ExternalFunction ifAbsent: [ ^ false ].
	callType := descriptorClass callingConventionFor: here.
	callType == nil ifTrue:[^false].
	[modifier := descriptorClass callingConventionModifierFor: token.
	 modifier notNil] whileTrue:
		[self advance.
		 callType := callType bitOr: modifier].
	"Parse return type"
	self advance.
	retType := self externalType: descriptorClass.
	retType == nil ifTrue: [^self expected:'return type'].
	"Parse function name or index"
	externalName := here.
	(self match: #string) 
		ifTrue:[externalName := externalName asSymbol]
		ifFalse:[(self match:#number) ifFalse:[^self expected:'function name or index']].
	(self match: #leftParenthesis) ifFalse:[^self expected:'argument list'].
	args := WriteStream on: #().
	[self match: #rightParenthesis] whileFalse:[
		argType := self externalType: descriptorClass.
		argType == nil ifTrue:[^self expected:'argument'].
		argType isVoid & argType isPointerType not ifFalse:[args nextPut: argType]].
	(self matchToken: 'module:') ifTrue:[
		module := here.
		(self match: #string) ifFalse:[^self expected: 'String'].
		module := module asSymbol].
	Smalltalk at: #ExternalLibraryFunction ifPresent:[:xfn|
		fn := xfn name: externalName 
				module: module 
				callType: callType
				returnType: retType
				argumentTypes: args contents.
		self allocateLiteral: fn].
	(self matchToken: 'error:')
		ifTrue:
			[| errorCodeVariable |
			 errorCodeVariable := here.
			(hereType == #string
			 or: [hereType == #word]) ifFalse:[^self expected: 'error code (a variable or string)'].
			 self advance.
			 self addPragma: (Pragma keyword: #primitive:error: arguments: (Array with: 120 with: errorCodeVariable)).
			 fn ifNotNil: [fn setErrorCodeName: errorCodeVariable]]
		ifFalse:
			[self addPragma: (Pragma keyword: #primitive: arguments: #(120))].
	^true! !

!Parser methodsFor: 'primitives' stamp: 'eem 12/1/2008 09:18'!
primitive: aNameString module: aModuleStringOrNil error: errorCodeVariableOrNil
	"Create named primitive with optional error code."
	
	(aNameString isString and: [ aModuleStringOrNil isNil or: [ aModuleStringOrNil isString ] ])
		ifFalse: [ ^ self expected: 'Named primitive' ].
	self allocateLiteral: (Array 
		with: (aModuleStringOrNil isNil 
			ifFalse: [ aModuleStringOrNil asSymbol ])
		with: aNameString asSymbol
		with: 0 with: 0).
	errorCodeVariableOrNil ifNotNil:
		[encoder floatTemp: (encoder bindTemp: errorCodeVariableOrNil) nowHasDef].
	^117! !

