'From Cuis 4.2 of 25 July 2013 [latest update: #2208] on 9 March 2015 at 9:42:47.886738 am'!

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'jmv 3/7/2015 22:23'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newArray index |
	newArray _ self new: aCollection size.
	index _ 0.
	aCollection do: [ :each | newArray at: (index _ index + 1) put: each].
	^ newArray

"
	Array newFrom: {1. 2. 3}
	{1. 2. 3} as: Array
	{1. 2. 3} as: ByteArray
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Text
"! !

