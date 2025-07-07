'From Cuis7.5 [latest update: #7335] on 7 July 2025 at 2:19:40 pm'!

!MethodSet methodsFor: 'filtering' stamp: 'jmv 7/7/2025 13:33:18'!
filterToNotSendersOf
	"Filter the receiver's list down to only those items which do not send a given selector."
	
	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol |
				self filterFrom:
					[:aClass :aSelector | | aMethod |
						(aMethod := aClass compiledMethodAt: aSelector) isNil or:
							[(aMethod hasLiteral: aSymbol) not]]]]! !

!MethodSet methodsFor: 'filtering' stamp: 'jmv 7/7/2025 13:33:23'!
filterToSendersOf
	"Filter the receiver's list down to only those items which send a given selector."
	
	self request: 'Enter selector:' initialAnswer: '' verifying: [:aString| aString notEmpty] do: [:aString|
		Symbol hasInterned: aString withBlanksTrimmed ifTrue:
			[:aSymbol | 
				self filterFrom:
					[:aClass :aSelector | | aMethod |
						(aMethod := aClass compiledMethodAt: aSelector) notNil and:
							[aMethod hasLiteral: aSymbol]]]]! !


!Behavior methodsFor: 'testing method dictionary' stamp: 'jmv 7/7/2025 14:18:29'!
whichSelectorsReferTo: literal special: specialFlag byte: specialByte
	"Answer a set of selectors whose methods access the argument as a literal."

	| who |
	who := Set new.
	self selectorsAndMethodsDo: [ :sel :method |
		(method hasLiteral: literal)
			ifTrue: [
				((literal isVariableBinding) not
					or: [method sendsToSuper not
					"N.B. (method indexOfLiteral: literal) < method numLiterals copes with looking for
					Float bindingOf: #NaN, since (Float bindingOf: #NaN) ~= (Float bindingOf: #NaN)."
					or: [(method indexOfLiteral: literal) ~= 0]])
						ifTrue: [who add: sel]]].

	"For special selectors, look for the literal in the source code.
	Otherwise, for example, searching for senders of #== will include senders of #ifNil.
	Except for #at:put:, because it has two arguments and won't find it in the source code like that."
	^ (specialByte isNil or: [ literal = #at:put: ]) 
		ifTrue: [ who ]
		ifFalse: [ who select: [ :sel | ((self sourceCodeAt: sel) findString: literal) > 0]]! !


!CompiledMethod methodsFor: 'testing' stamp: 'jmv 7/7/2025 13:33:52'!
sendsOrRefersTo: aSelector

	^ (self hasLiteral: aSelector) or: [ self sendsSelector: aSelector ]! !

!methodRemoval: AdditionalMethodState #hasLiteralThorough: stamp: 'jmv 7/7/2025 13:34:53'!
AdditionalMethodState removeSelector: #hasLiteralThorough:!
!methodRemoval: CompiledMethod #hasLiteralThorough: stamp: 'jmv 7/7/2025 13:34:48'!
CompiledMethod removeSelector: #hasLiteralThorough:!
!methodRemoval: Behavior #rejectSelectorsFrom:thatReferenceTo:byte: stamp: 'jmv 7/7/2025 13:42:35'!
Behavior removeSelector: #rejectSelectorsFrom:thatReferenceTo:byte:!
!methodRemoval: Behavior #thoroughWhichSelectorsReferTo:special:byte: stamp: 'jmv 7/7/2025 13:37:53'!
Behavior removeSelector: #thoroughWhichSelectorsReferTo:special:byte:!
