'From Cuis 4.0 of 21 April 2012 [latest update: #1495] on 4 December 2012 at 10:53:50 pm'!

!VariableNode methodsFor: 'code generation' stamp: 'eem 11/2/2012 10:15'!
sizeCodeForReturn: encoder
	encoder
		if: code
		isSpecialLiteralForReturn:
			[:specialLiteral|
			 ^encoder sizeReturnSpecialLiteral: specialLiteral].
	(self code = LdSelf or: [self code = LdSuper]) ifTrue:
		[^encoder sizeReturnReceiver].
	^super sizeCodeForReturn: encoder! !


!VariableNode reorganize!
('visiting' accept:)
('initialize-release' asStorableNode: name: name:index:type: name:key:code: name:key:index:type:)
('testing' assignmentCheck:at: index isSelfPseudoVariable isUndeclared isVariableNode isVariableReference type)
('code generation (closures)' beingAssignedToAnalyseTempsWithin:rootNode:assignmentPools:)
('tiles' currentValueIn: variableGetterBlockIn:)
('code generation (new scheme)' emitCodeForLoad:encoder: emitCodeForReturn:encoder: emitCodeForStore:encoder: emitCodeForStorePop:encoder: emitCodeForValue:encoder: sizeCodeForStore: sizeCodeForStorePop: sizeCodeForValue:)
('code generation' fieldOffset sizeCodeForReturn:)
('accessing' name)
('printing' printOn:indent: printWithClosureAnalysisOn:indent:)
!

