'From Cuis 6.0 [latest update: #5041] on 16 January 2022 at 12:34:46 pm'!

!MessageSet methodsFor: 'message list' stamp: 'jmv 1/16/2022 12:33:04'!
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
	messageList do: [ :each | | d |
		d _ (each actualClass theNonMetaClass allSuperclasses select: [ :c |
			classesAndPositions includesKey: c ]) size.
		each prefixStringVersionWith: (
			String streamContents: [ :strm |
				d timesRepeat: [ strm nextPutAll: ' ---'].
				strm nextPut: Character space ]) ].
	self changed: #messageList.! !

