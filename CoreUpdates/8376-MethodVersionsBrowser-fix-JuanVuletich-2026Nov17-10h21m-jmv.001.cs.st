'From Cuis7.9 [latest update: #8376] on 17 November 2026 at 10:22:50 am'!

!MethodVersionsBrowser methodsFor: 'viewing access' stamp: 'jmv 17/Nov/2026, 1:22:29 pm (UTC)'!
toggleListIndex: newListIndex

	changeList notEmpty ifTrue: [
		listIndex := newListIndex.
		self changed: #listIndex.
		self acceptedContentsChanged ].! !

!MethodVersionsBrowser methodsFor: 'init & update' stamp: 'jmv 17/Nov/2026, 1:15:58 pm (UTC)'!
reformulateList
	| aMethod |
	"Some uncertainty about how to deal with lost methods here"
	aMethod := classOfMethod compiledMethodAt: selectorOfMethod ifAbsent: [^ self].
	
	self scanVersionsOf: aMethod class: classOfMethod theNonMetaClass meta: classOfMethod isMeta category: (classOfMethod whichCategoryIncludesSelector: selectorOfMethod) selector: selectorOfMethod.
	self changed: #list. "for benefit of mvc"
	changeList notEmpty ifTrue: [
		listIndex := 1 ].
	self changed: #listIndex.
	self acceptedContentsChanged
! !

