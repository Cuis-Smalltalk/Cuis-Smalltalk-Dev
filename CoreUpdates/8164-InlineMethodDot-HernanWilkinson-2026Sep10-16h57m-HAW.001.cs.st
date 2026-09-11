'From Cuis7.9 [latest update: #8163] on 10 September 2026 at 6:00:34 pm'!

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 10/Sep/2026, 7:57:20 pm (UTC)'!
isStatementEndedAfter: aSourceCodeInterval in: aSourceCode

	| index |

	index := aSourceCodeInterval last + 1.
	[ index <= aSourceCode size and: [ (aSourceCode at: index) isSeparator ] ] whileTrue: [ index := index + 1 ].

	^index > aSourceCode size or: [ '.]' includes: (aSourceCode at: index) ]! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 10/Sep/2026, 7:58:34 pm (UTC)'!
isStatementTerminatedIn: aSourceCodeInterval of: aSourceCode

	^(aSourceCode copyFrom: aSourceCodeInterval first to: aSourceCodeInterval last) withBlanksTrimmed endsWith: '.'! !

!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 10/Sep/2026, 7:58:32 pm (UTC)'!
shouldDropThePeriodOf: newSourceCode replacing: aSourceCodeInterval in: aSourceCode

	^(newSourceCode endsWith: '.')
		and: [ (self isStatementTerminatedIn: aSourceCodeInterval of: aSourceCode) not
		and: [ self isStatementEndedAfter: aSourceCodeInterval in: aSourceCode ] ]! !


!InlineMethod methodsFor: 'applying primitives - private' stamp: 'HAW 10/Sep/2026, 7:58:29 pm (UTC)'!
replaceRange: aSourceCodeInterval withNewSourceCode: newSourceCode inMethod: aCompiledMethod

	| currentUpdates sourceCodeReplacement |

	sourceCodeReplacement := (self shouldDropThePeriodOf: newSourceCode replacing: aSourceCodeInterval in: aCompiledMethod sourceCode)
		ifTrue: [ newSourceCode copyUpToLast: $. ]
		ifFalse: [ newSourceCode ].
	"It's important to use < and not <= because temporaries declarations are added to the changes collection
	after the changes concerning the inlining of the implementor code, but they need to be replaced first - Fernando"
	currentUpdates := updatedSendersCode at: aCompiledMethod methodReference ifAbsent: (SortedCollection 
		sortBlock: [ :left :right | left key first < right key first ]).
	currentUpdates add: (aSourceCodeInterval -> sourceCodeReplacement).
	updatedSendersCode at: aCompiledMethod methodReference put: currentUpdates.
	! !

