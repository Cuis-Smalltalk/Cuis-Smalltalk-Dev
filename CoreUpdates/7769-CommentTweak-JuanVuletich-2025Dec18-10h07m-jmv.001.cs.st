'From Cuis7.5 [latest update: #7768] on 18 December 2025 at 10:07:21 am'!

!UnicodeString methodsFor: 'converting' stamp: 'jmv 18/Dec/2025 09:46:17'!
thatCanBeModified
	"In UnicodeString #at:put: could be slow. Prefer ArrayOfCharactersAndCodePoints.
	(This comment used to say: Because UnicodeString instances don't support #at:put:)"

	^ArrayOfCharactersAndCodePoints withAll: self! !

