'From Cuis 4.2 of 25 July 2013 [latest update: #2169] on 20 February 2015 at 9:14:24.977977 pm'!

!Heap methodsFor: 'copying' stamp: 'SqR 2/20/2015 21:13'!
postCopy

	array _ array copy.
	tally _ tally copy! !


!SortedCollection methodsFor: 'copying' stamp: 'SqR 2/20/2015 21:11'!
postCopy

	array _ array copy.
	firstIndex _ firstIndex copy.
	lastIndex _ lastIndex copy! !


!ActiveModel methodsFor: 'copying' stamp: 'SqR 2/20/2015 21:06'!
postCopy

	actionMap _ nil! !


!Behavior methodsFor: 'copying' stamp: 'SqR 2/20/2015 21:09'!
postCopy

	self methodDict: self methodDict copy! !


!Heap methodsFor: 'copying' stamp: 'SqR 2/20/2015 20:54'!
copy

	^self shallowCopy postCopy! !


!SortedCollection methodsFor: 'copying' stamp: 'SqR 2/20/2015 20:51'!
copy

	^self shallowCopy postCopy! !


!Dictionary methodsFor: 'private' stamp: 'SqR 2/20/2015 21:06'!
postCopy
	"Must copy the associations, or later store will affect both the
original and the copy"

	array _ array collect: [:each | each copy]! !


!ColorForm methodsFor: 'copying' stamp: 'SqR 2/20/2015 21:10'!
postCopy
	"Just to make visible the fact that we are sharing the colors... Is it ok to do this?"
	super postCopy.
	self colors: colors! !


!SharedQueue methodsFor: 'copying' stamp: 'SqR 2/20/2015 21:11'!
postCopy
	contentsArray := contentsArray copy.
	accessProtect := Semaphore forMutualExclusion.
	readSynch := Semaphore new! !

!methodRemoval: Object #copyTwoLevel!
Object removeSelector: #copyTwoLevel!
