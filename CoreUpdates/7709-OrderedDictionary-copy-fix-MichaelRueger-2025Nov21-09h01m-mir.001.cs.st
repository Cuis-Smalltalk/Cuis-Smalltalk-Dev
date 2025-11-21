'From Cuis7.5 [latest update: #7708] on 21 November 2025 at 9:01:41 am'!

!OrderedDictionary methodsFor: 'private' stamp: 'mir 11/21/2025 16:16:37'!
postCopy
	"Must copy the orderedKeys as well"

	super postCopy.
	orderedKeys := orderedKeys copy! !

