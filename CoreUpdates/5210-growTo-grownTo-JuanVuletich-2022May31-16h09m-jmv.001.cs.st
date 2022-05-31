'From Cuis 6.0 [latest update: #5209] on 31 May 2022 at 4:10:07 pm'!

!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 5/31/2022 15:53:55'!
grownTo: newSize

	| grownCollection |
	grownCollection _ self class ofSize: newSize.
	grownCollection replaceFrom: 1 to: self size with: self startingAt: 1.
	^grownCollection! !


!Text methodsFor: 'copying' stamp: 'jmv 5/31/2022 15:57:36'!
grownTo: newSize
	"Accommodate for the fact that string could hold a String or an ArrayOfCharactersAndCodePoints
	when being streamed to."

	| grownText |
	grownText _ Text fromString: (string class new: newSize).
	grownText replaceFrom: 1 to: self size with: self startingAt: 1.
	^grownText! !


!WriteStream methodsFor: 'private' stamp: 'jmv 5/31/2022 15:54:26'!
growTo: anInteger
	"Grow the collection by creating a new bigger collection and then
	copy over the contents from the old one. We grow by doubling the size.

	anInteger is the required minimal new size of the collection "

	| oldSize newSize |
	oldSize _ collection size.
	newSize _ anInteger + (oldSize max: 20).
	collection _ collection grownTo: newSize.
	writeLimit _ collection size! !

