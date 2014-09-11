'From Cuis 4.2 of 25 July 2013 [latest update: #2087] on 8 September 2014 at 11:42:08.236682 am'!

!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 9/3/2014 22:49'!
collect: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect the 
	resulting values into a collection that is like me. Answer the new 
	collection. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection _ self species new: self size.
	firstIndex to: lastIndex do: [ :index |
		newCollection addLast: (aBlock value: (array at: index))].
	^ newCollection! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 9/3/2014 22:50'!
do: aBlock 

	firstIndex to: lastIndex do: [ :index |
		aBlock value: (array at: index) ]! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 9/3/2014 22:52'!
reverseDo: aBlock 

	lastIndex to: firstIndex by: -1 do: [ :index |
		aBlock value: (array at: index) ]! !

!OrderedCollection methodsFor: 'enumerating' stamp: 'jmv 9/3/2014 22:53'!
select: aBlock 
	"Evaluate aBlock with each of my elements as the argument. Collect into
	a new collection like the receiver, only those elements for which aBlock
	evaluates to true."

	| newCollection element |
	newCollection _ self copyEmpty.
	firstIndex to: lastIndex do: [ :index |
		(aBlock value: (element _ array at: index))
			ifTrue: [ newCollection addLast: element ]].
	^ newCollection! !

!OrderedCollection methodsFor: 'removing' stamp: 'jmv 9/3/2014 22:54'!
remove: oldObject ifAbsent: absentBlock

	firstIndex to: lastIndex do: [ :index |
		oldObject = (array at: index)
			ifTrue: [
				self removeIndex: index.
				^ oldObject ]].
	^ absentBlock value! !

!OrderedCollection methodsFor: 'private' stamp: 'jmv 9/3/2014 22:55'!
find: oldObject
	"Answer an index in the range [firstIndex, lastIndex]"

	firstIndex to: lastIndex do: [ :index |
		(array at: index) = oldObject ifTrue: [ ^index ]].
	self errorNotFound: oldObject! !

