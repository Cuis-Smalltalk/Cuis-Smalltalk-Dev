'From Cuis 4.0 of 21 April 2012 [latest update: #1286] on 15 May 2012 at 10:23:27 pm'!

!SystemDictionary methodsFor: 'code authors' stamp: 'jmv 5/15/2012 18:00'!
unknownContributors
	"Answer a collection of authorInitials for whom there is code in the system 
	(either in core or in loaded packages), but we don't knwo their full name.
	Smalltalk unknownContributors
	"

	| all ok |
	all _ Smalltalk allContributors asSet.
	ok _ (Smalltalk contributorInitialsAndNames collect: [ :pair | pair first ]) asSet.
	^(all difference: ok) asArray sort! !

