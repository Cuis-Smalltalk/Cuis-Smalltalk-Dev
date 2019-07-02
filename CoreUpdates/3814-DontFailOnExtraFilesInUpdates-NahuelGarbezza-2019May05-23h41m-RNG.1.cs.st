'From Cuis 5.0 of 7 November 2016 [latest update: #3725] on 5 May 2019 at 11:45:31 pm'!
"Change Set:		3726-CuisCore-AuthorName-2019May05-23h41m
Date:				5 May 2019
Author:			Nahuel Garbezza

When installing new updates, handle (ignore) files that are not updates"!


!ChangeSet class methodsFor: 'services' stamp: 'RNG 5/5/2019 23:44:16'!
isNewUpdate: aFile

	^ aFile name first isDigit and: [ aFile name asNumber > SystemVersion current highestUpdate ]! !


!ChangeSet class methodsFor: 'services' stamp: 'RNG 5/5/2019 23:41:26'!
newUpdates: updatesFileDirectory

	^ (updatesFileDirectory files select: [ :each | self isNewUpdate: each ])
		asSortedCollection: [ :a :b | a name < b name ]! !

