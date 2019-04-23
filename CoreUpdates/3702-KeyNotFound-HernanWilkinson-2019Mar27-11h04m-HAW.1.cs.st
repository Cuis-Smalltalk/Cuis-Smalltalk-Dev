'From Cuis 5.0 of 7 November 2016 [latest update: #3688] on 17 April 2019 at 9:21:57 pm'!

!Dictionary class methodsFor: 'error descriptions' stamp: 'HAW 4/4/2019 08:14:52'!
keyNotFoundErrorDescription

	^'key not found'! !


!Dictionary methodsFor: 'private' stamp: 'HAW 4/4/2019 08:15:21'!
errorKeyNotFound

	self error: self class keyNotFoundErrorDescription ! !


!Trie methodsFor: 'private' stamp: 'HAW 4/4/2019 08:15:38'!
errorKeyNotFound

	self error: Dictionary keyNotFoundErrorDescription ! !


!Dictionary class reorganize!
('instance creation' newFrom: newFromPairs:)
('error descriptions' keyNotFoundErrorDescription)
!

