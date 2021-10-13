'From Cuis 5.0 [latest update: #4910] on 13 October 2021 at 11:00:17 am'!

!Array methodsFor: 'enumerating' stamp: 'jmv 10/13/2021 10:54:48'!
collect: collectBlock thenSelect: selectBlock
	"Evaluate collectBlock with each my elements as the argument. Collect the 
	resulting values into a collection that is like me, but only those elements for which
	selectBlock evaluates to true. Answer the new collection.
	Overriden for performance."

	| newElement |
	^ self species streamContents: [ :strm |
		1 to: self size do: [ :index |
			newElement _ collectBlock value: (self at: index).
			(selectBlock value: newElement)
				ifTrue: [ strm nextPut: newElement ]]]! !

!Array methodsFor: 'enumerating' stamp: 'jmv 10/13/2021 10:58:21'!
select: selectBlock thenCollect: collectBlock
	"Evaluate selectBlock with each my elements as the argument. For those who evaluate to true,
	collect the result of evaluating collectBlock on them into a collection that is like me.
	Answer the new collection.
	Overriden for performance."

	| each |
	^ self species streamContents: [ :strm |
		1 to: self size do: [ :index |
			each _ self at: index.
			(selectBlock value: each) ifTrue: [
				strm nextPut: (collectBlock value: each) ]]]! !


!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 10/13/2021 10:40:07'!
collect: collectBlock thenSelect: selectBlock
	"Evaluate collectBlock with each my elements as the argument. Collect the 
	resulting values into a collection that is like me, but only those elements for which
	selectBlock evaluates to true. Answer the new collection.
	Overriden for performance."

	| newCollection newElement |
	newCollection _ self species new.
	newCollection resetTo: 1.
	firstIndex to: lastIndex do: [ :index |
		newElement _ collectBlock value: (array at: index).
		(selectBlock value: newElement)
			ifTrue: [ newCollection addLast: newElement ]].
	^ newCollection! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 10/13/2021 10:42:23'!
select: selectBlock thenCollect: collectBlock
	"Evaluate selectBlock with each my elements as the argument. For those who evaluate to true,
	collect the result of evaluating collectBlock on them into a collection that is like me.
	Answer the new collection.
	Overriden for performance."

	| newCollection each |
	newCollection _ self species new.
	newCollection resetTo: 1.
	firstIndex to: lastIndex do: [ :index |
		each _ array at: index.
		(selectBlock value: each) ifTrue: [
			newCollection addLast: (collectBlock value: each) ]].
	^ newCollection! !


!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 10/13/2021 10:36:49'!
select: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| newCollection element |
	newCollection _ self species new.
	newCollection resetTo: 1.
	firstIndex to: lastIndex do: [ :index |
		(aBlock value: (element _ array at: index))
			ifTrue: [ newCollection addLast: element ]].
	^ newCollection! !


!Array reorganize!
('enumerating' collect:thenSelect: select:thenCollect:)
('comparing' literalEqual:)
('converting' asArray elementsExchangeIdentityWith: elementsForwardIdentityTo: elementsForwardIdentityTo:copyHash: evalStrings)
('printing' isLiteral printOn: storeOn:)
('private' hasLiteral: hasLiteralSuchThat: replaceFrom:to:with:startingAt:)
('accessing' atWrap: atWrap:put:)
('testing' identityIncludes: is: isArray)
!

