'From Cuis 5.0 [latest update: #4784] on 21 August 2021 at 8:10:52 pm'!

!HierarchicalListMorph methodsFor: 'private' stamp: 'jmv 11/14/2011 10:40'!
addSubmorphsAfter: parentMorph fromCollection: aCollection allowSorting: sortBoolean

	| priorMorph morphList newCollection |
	priorMorph _ nil.
	newCollection _ (sortBoolean and: [sortingSelector notNil]) ifTrue: [
		aCollection asOrderedCollection sort: [ :a :b | 
			(a perform: sortingSelector) <= (b perform: sortingSelector)]
	] ifFalse: [
		aCollection
	].
	morphList _ OrderedCollection new.
	newCollection do: [:item | 
		priorMorph _ self indentingItemClass basicNew 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: parentMorph indentLevel + 1.
		morphList add: priorMorph.
	].
	scroller addAllMorphs: morphList after: parentMorph.
	^morphList
	
! !

