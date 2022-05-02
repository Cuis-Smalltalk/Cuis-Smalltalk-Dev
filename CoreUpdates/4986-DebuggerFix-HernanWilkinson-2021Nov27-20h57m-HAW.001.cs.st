'From Cuis 5.0 [latest update: #4985] on 28 November 2021 at 3:04:22 pm'!

!Encoder class methodsFor: 'enconding' stamp: 'HAW 11/28/2021 14:46:40'!
doItInContextName

	^'originalContext'! !


!Parser methodsFor: 'expression types' stamp: 'HAW 11/28/2021 15:00:12'!
addKeywordPatternPartTo: selector keywordRanges: keywordRanges arguments: arguments

	self advanceWithRangeDo: [ :keywordAsString :range |
		selector nextPutAll: keywordAsString.
		keywordRanges add: range ].

	self argumentNameWithRangeDo: [ :argName :range |
		"I have to reference Encoder and not enconder becuase in some cases encoder is a parser!!!! - Hernan"
		(argName = Encoder doItInContextName and: [  selector contents = self class doItInSelector ])
			ifTrue: [ arguments addLast: (encoder addMultiRange: range for: (encoder lookupVariable: argName ifAbsent: [ self shouldNotHappen ]))]
			ifFalse: [ arguments addLast: (encoder bindArg: argName range: range)]]
! !


!Scanner class methodsFor: 'doIt selectors' stamp: 'HAW 11/28/2021 14:53:14'!
selectionDoItSourceCodeHeaderWithContext: hasContext into: stream

	stream
		nextPutAll: (hasContext ifTrue: [ Scanner doItInSelector, ' ', Encoder doItInContextName ] ifFalse: [ Scanner doItSelector ]);
		newLine;
		newLine! !


!Parser methodsFor: 'expression types' stamp: 'HAW 11/28/2021 14:47:22'!
doitPatternInContext: context

	^context
		ifNil: [{self class doItSelector. {}. 1. nil }]
		ifNotNil: [{self class doItInSelector. {encoder encodeVariable: Encoder doItInContextName}. 3. nil}]! !

!Encoder methodsFor: 'results' stamp: 'HAW 11/28/2021 14:50:03'!
unusedTempNames
	| unused |
	unused := OrderedCollection new.
	scopeTable associationsDo:
		[:assn | | name |
		(assn value isUnusedTemp) ifTrue:
			[name := assn value key.
			 name ~= Encoder doItInContextName ifTrue: [unused add: name]]].
	^ unused! !

!Encoder methodsFor: 'initialization' stamp: 'HAW 11/28/2021 14:49:19'!
init: aClass context: aContext notifying: req
	requestor := req.
	class := aClass.
	nTemps := 0.
	supered := false.
	self initScopeAndLiteralTables.
	"No Tweak Fields in Cuis"
	class variablesAndOffsetsDo: [ :variable "<String>" :offset "<Integer>" |
		scopeTable
			at: variable
			put: (offset >= 0
					ifTrue: [InstanceVariableNode new
								name: variable index: offset]
					ifFalse: [MaybeContextInstanceVariableNode new
								name: variable index: offset negated])].
	aContext ifNotNil: [
		| homeNode |
		homeNode := self bindTemp: Encoder doItInContextName.
		"0th temp = aContext passed as arg"
		aContext tempNames withIndexDo: [ :variable :index |
			variable ~= Encoder doItInContextName ifTrue: [
				scopeTable
					at: variable
					put: (MessageAsTempNode new
						receiver: homeNode
						selector: #namedTempAt:
						arguments: (Array with: (self encodeLiteral: index))
						precedence: 3
						from: self)]]].
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32! !

!methodRemoval: Encoder #doItInContextName stamp: 'HAW 11/28/2021 14:50:20'!
Encoder removeSelector: #doItInContextName!

!Encoder class reorganize!
('enconding' doItInContextName)
!
