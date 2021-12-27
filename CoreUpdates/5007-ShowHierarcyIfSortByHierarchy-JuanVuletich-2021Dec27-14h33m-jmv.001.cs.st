'From Cuis 5.0 [latest update: #5006] on 27 December 2021 at 2:52:40 pm'!

!MessageSet methodsFor: 'message list' stamp: 'jmv 12/27/2021 14:50:27'!
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
			each prefixStringVersionWith: 
				(String
					new: each actualClass theNonMetaClass allSuperclasses size * 3
					withAll: $-) ].
	self changed: #messageList.! !

