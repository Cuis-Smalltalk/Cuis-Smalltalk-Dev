'From Cuis 4.2 of 25 July 2013 [latest update: #1905] on 25 December 2013 at 8:40:08.254816 pm'!

!Collection methodsFor: 'converting' stamp: 'ls 1/3/1999 13:48'!
asCharacterSet
	"Answer a CharacterSet whose elements are the unique elements of the receiver.  The receiver should only contain characters"

	^CharacterSet newFrom: self
! !

!Collection methodsFor: 'enumerating' stamp: 'jmv 5/20/2011 14:18'!
groupBy: keyBlock having: selectBlock 
	"Like in SQL operation - Split the receivers contents into collections of 
	elements for which keyBlock returns the same results, and return those 
	collections allowed by selectBlock. keyBlock should return an Integer."
	| result |
	result _ Dictionary new.
	self do: [ :e |
		(result at: (keyBlock value: e) ifAbsentPut: OrderedCollection new) add: e ].
	^ result select: selectBlock! !

!Collection methodsFor: 'math functions' stamp: 'TAG 11/6/1998 15:53'!
reciprocal
	"Return the receiver full of reciprocated elements"
	^ self collect: [:a | a reciprocal]! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 9/29/2008 15:02'!
case: dist
	"statements = keyStmts CascadeFlag keyValueBlock ... keyStmts"

	| nextCase thenJump stmtStream elements b node cases otherBlock myExits |
	nextCase := pc + dist.

	"Now add CascadeFlag & keyValueBlock to statements"
	statements addLast: stack removeLast.
	stack addLast: CaseFlag. "set for next pop"
	statements addLast: (self blockForCaseTo: nextCase).

	stack last == CaseFlag
		ifTrue: "Last case"
			["ensure jump is within block (in case thenExpr returns weirdly I guess)"
			stack removeLast. "get rid of CaseFlag"
			stmtStream := ReadStream on: (self popTo: stack removeLast).
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == CascadeFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: (Array with: stmtStream next)).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			"try find the end of the case"
			myExits := caseExits removeLast: elements size.
			myExits := myExits reject: [ :e | e isNil or: [ e < 0 or: [ e > method endPC ] ] ].
			thenJump := myExits isEmpty
							ifTrue: [ nextCase ]
							ifFalse: [ myExits max ].
			
			otherBlock := self blockTo: thenJump.
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf:otherwise: code: #macro)
					arguments: (Array with: cases with: otherBlock))].! !


!MessageAsTempNode methodsFor: 'access to remote temps' stamp: 'di 3/22/1999 09:38'!
asStorableNode: encoder
	"This node is a message masquerading as a temporary variable.
	It currently has the form {homeContext tempAt: offset}.
	We need to generate code for {expr storeAt: offset inTempFrame: homeContext},
	where the expr, the block argument, is already on the stack.
	This, in turn will get turned into {homeContext tempAt: offset put: expr}
	at runtime if nobody disturbs storeAt:inTempFrame: in Object (not clean)"
	^ MessageAsTempNode new
		receiver: nil  "suppress code generation for receiver already on stack"
		selector: #storeAt:inTempFrame:
		arguments: (arguments copyWith: receiver)
		precedence: precedence
		from: encoder! !

