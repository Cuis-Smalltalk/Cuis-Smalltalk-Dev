'From Cuis 5.0 [latest update: #4774] on 20 August 2021 at 10:09:12 am'!

!Object methodsFor: 'printing' stamp: 'jmv 8/20/2021 09:47:29'!
printString
	"Answer a String whose characters are a description of the receiver. 
	If you want to print without a character limit, use fullPrintString.

	This description is to be meaningful for a Smalltalk programmer and usually includes
	a hint on the class of the object.

	Usually you should not reimplement this method in subclasses, but #printOn:

	See the comments at:
		#printString
		#displayStringOrText
		#asString
		#storeString"

	^ self printStringLimitedTo: 200! !


!Inspector methodsFor: 'constants' stamp: 'jmv 8/20/2021 09:46:52'!
printStringLimit

	^1200! !


!HierarchicalListMorph methodsFor: 'private' stamp: 'jmv 8/20/2021 09:54:01'!
addSubmorphsAfter: parentMorph fromCollection: aCollection allowSorting: sortBoolean

	| priorMorph morphList newCollection limit warning |
	priorMorph _ nil.
	newCollection _ (sortBoolean and: [sortingSelector notNil]) ifTrue: [
		aCollection asOrderedCollection sort: [ :a :b | 
			(a perform: sortingSelector) <= (b perform: sortingSelector)]
	] ifFalse: [
		aCollection
	].
	"Limit the number of entries shown."
	limit _ 51.
	newCollection size > limit ifTrue: [
		warning _ 'Only the first ', (limit-1) printString, ' elements included.'.
		newCollection _ newCollection copyFrom: 1 to: limit.
		newCollection at: limit put: (ListItemWrapper with: warning) ].
	morphList _ OrderedCollection new.
	newCollection do: [ :item | 
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

