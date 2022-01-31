'From Cuis 6.0 [latest update: #5053] on 31 January 2022 at 1:48:30 pm'!

!MethodReference methodsFor: 'setting' stamp: 'len 1/30/2022 05:52:06'!
indentLevel: anInteger
	self removeStringVersionPrefix.
	stringVersion _ (String streamContents: [:aStream| anInteger timesRepeat: [aStream nextPutAll: '    ']]), stringVersion! !


!MessageSet methodsFor: 'message list' stamp: 'len 1/30/2022 05:51:23'!
sortByClassHierarchy
	"Sort the message-list by class / selector. List classes in hierarchical order."
	
	| aClass bClass classes classesAndPositions i |

	classes _ Set new.
	messageList do: [ :methodReference |
		methodReference actualClass ifNotNil: [ :actualClass | classes add: actualClass ]].
	classesAndPositions _ Dictionary new.
	i _ 1.
	Smalltalk hierarchySorted: classes do: [ :each |
		classesAndPositions at: each put: i.
		i _ i + 1 ].
	
	messageList _ messageList sort: [ :a :b |
		(a classSymbol = b classSymbol and: [ b classIsMeta = b classIsMeta ])
			ifTrue: [
				a methodSymbol == #Comment
					ifTrue: [ true ]
					ifFalse: [
						b methodSymbol == #Comment
							ifTrue: [ false ]
							ifFalse: [ a methodSymbol < b methodSymbol ]]]
			ifFalse: [
				aClass _ a actualClass.
				bClass _ b actualClass.
				aClass isNil == bClass isNil
					ifTrue: [
						aClass isNil
							ifTrue: [a classSymbol < b classSymbol]
							ifFalse: [(classesAndPositions at: aClass) < (classesAndPositions at: bClass)]]
					ifFalse: [aClass isNil]]].
	messageList do: [ :each |
		each indentLevel:
			(each actualClass theNonMetaClass allSuperclasses select: [ :c |
				classesAndPositions includesKey: c ]) size ].
	self changed: #messageList.! !


!MethodReference methodsFor: 'setting' stamp: 'len 1/30/2022 05:49:54'!
removeStringVersionPrefix
	| i prefixCoda |
	prefixCoda _ '] - '.
	i _ stringVersion findString: prefixCoda.
	i = 0 ifFalse: [stringVersion _ stringVersion copyFrom: i + prefixCoda size to: stringVersion size].
	stringVersion _ stringVersion withoutLeadingBlanks! !

