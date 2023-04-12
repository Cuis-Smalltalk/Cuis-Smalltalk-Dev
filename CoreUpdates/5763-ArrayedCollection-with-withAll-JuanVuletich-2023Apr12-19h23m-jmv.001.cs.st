'From Cuis 6.0 [latest update: #5761] on 12 April 2023 at 7:23:39 pm'!

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 4/12/2023 15:23:14'!
with: firstElement withAll: aSequenceableCollection
	"Create a new collection containing firstElement, then all the elements from aSequenceableCollection.
	Array with: 1 withAll: (10 to: 20)
	"

	| s |
	s := aSequenceableCollection size+1.
	^ (self new: s)
		at: 1 put: firstElement;
		replaceFrom: 2		to: s		with: aSequenceableCollection! !

