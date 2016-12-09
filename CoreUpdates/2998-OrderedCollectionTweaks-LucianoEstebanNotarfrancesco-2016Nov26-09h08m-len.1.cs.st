'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 29 November 2016 at 9:10:32 pm'!

!OrderedCollection methodsFor: 'converting' stamp: 'len 11/29/2016 08:54:14'!
asNewArray
	^ array copyFrom: firstIndex to: lastIndex! !


!OrderedCollection class methodsFor: 'instance creation' stamp: 'len 11/28/2016 19:18:39'!
newFrom: aCollection
	"Create a new collection containing all the elements from aCollection"

	^(self new: aCollection size)
		resetTo: 1;
		addAll: aCollection;
		yourself! !


!OrderedCollection methodsFor: 'enumerating' stamp: 'len 11/28/2016 10:50:21'!
collect: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into a collection that is like me. Answer the new 
	collection. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection _ self species new: self size.
	newCollection resetTo: 1.
	firstIndex to: lastIndex do: [ :index |
		newCollection addLast: (aBlock value: (array at: index))].
	^ newCollection! !

