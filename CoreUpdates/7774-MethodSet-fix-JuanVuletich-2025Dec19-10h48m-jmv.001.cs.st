'From Cuis7.5 [latest update: #7772] on 19 December 2025 at 10:49:42 am'!

!MethodSet methodsFor: 'message list' stamp: 'jmv 19/Dec/2025 10:15:37'!
sortByClassHierarchy
	"Sort the message-list by class / selector. List classes in hierarchical order."
	
	| aClass bClass classes classesAndPositions i |

	classes := Set new.
	messageList do: [ :methodReference |
		methodReference actualClass ifNotNil: [ :actualClass | classes add: actualClass ]].
	classesAndPositions := Dictionary new.
	i := 1.
	Smalltalk hierarchySorted: classes do: [ :each |
		classesAndPositions at: each put: i.
		i := i + 1 ].
	
	messageList := messageList sort: [ :a :b |
		(a classSymbol = b classSymbol and: [ a classIsMeta = b classIsMeta ])
			ifTrue: [
				"Same class (or metaclass). Class comment first, then sorted selectors."
				a methodSymbol == #Comment
					ifTrue: [ true ]
					ifFalse: [
						b methodSymbol == #Comment
							ifTrue: [ false ]
							ifFalse: [ a methodSymbol < b methodSymbol ]]]
			ifFalse: [
				"Different class / Metaclass."
				aClass := a actualClass.
				bClass := b actualClass.
				aClass isNil == bClass isNil
					ifTrue: [
						aClass isNil
							ifTrue: [a classSymbol < b classSymbol]
							ifFalse: [
								"Neither class is nil"
								a classIsMeta = b classIsMeta
									ifFalse: [a classIsMeta not] "Instance side first, class side then."
									ifTrue: [
										"Both class side, or both instance side. Sort by hierarchy."
										(classesAndPositions at: aClass) < (classesAndPositions at: bClass)]]]
					ifFalse: [ aClass isNil ]]].
	messageList do: [ :each |
		each indentLevel:
			(each actualClass theNonMetaClass allSuperclasses select: [ :c |
				classesAndPositions includesKey: c ]) size ].
	self changed: #messageList.! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 19/Dec/2025 10:09:58'!
sortByClassName
	"Sort the message-list by class / selector"
	
	messageList := messageList sort: [ :a :b |
		a classSymbol = b classSymbol
			ifTrue: [
				a methodSymbol == #Comment
					ifTrue: [ true ]
					ifFalse: [
						b methodSymbol == #Comment
							ifTrue: [ false ]
							ifFalse: [
								a classIsMeta = b classIsMeta
									ifTrue: [ a methodSymbol < b methodSymbol ]
									ifFalse: [ a classIsMeta not ]	]]]
			ifFalse: [ a classSymbol < b classSymbol ]
		].
	messageList do: [ :each | each removeStringVersionPrefix ].
	self changed: #messageList.! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 19/Dec/2025 10:04:03'!
sortByDate
	"Sort the message-list by date of time-stamp"

	| assocs aCompiledMethod aDate inOrder |
	assocs := messageList collect: [ :aRef |
		aDate := aRef methodSymbol == #Comment
			ifTrue: [
				aRef actualClass organization dateCommentLastSaved start]
			ifFalse: [
				aCompiledMethod := aRef actualClass compiledMethodAt: aRef methodSymbol ifAbsent: nil.
				aCompiledMethod ifNotNil: [aCompiledMethod dateAndTime]].
		aRef -> (aDate ifNil: [(Date fromString: '01/01/1996') start])].  "The dawn of Squeak history"
	inOrder := assocs asArray sort: [ :a :b | a value < b value].

	inOrder do: [ :each | each key prefixStringVersionWith: each value date yyyymmdd ].
	messageList := inOrder collect: [ :assoc | assoc key ].
	self changed: #messageList! !

!MethodSet methodsFor: 'message list' stamp: 'jmv 19/Dec/2025 10:10:28'!
sortBySelector
	"Sort the message-list by selector / class"
	
	messageList := messageList sort: [ :a :b |
		a methodSymbol == #Comment
			ifTrue: [ true ]
			ifFalse: [
				b methodSymbol == #Comment
					ifTrue: [ false ]
					ifFalse: [
						a methodSymbol = b methodSymbol
							ifTrue: [
								a classSymbol = b classSymbol
									ifTrue: [ a classIsMeta not ]
									ifFalse: [ a classSymbol < b classSymbol ]]
							ifFalse: [ a methodSymbol < b methodSymbol ]]]
		].
	messageList do: [ :each | each prefixStringVersionWith: each methodSymbol ].
	self changed: #messageList! !

