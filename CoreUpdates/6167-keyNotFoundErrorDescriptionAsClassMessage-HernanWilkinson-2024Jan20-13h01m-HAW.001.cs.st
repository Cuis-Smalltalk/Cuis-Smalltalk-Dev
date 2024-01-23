'From Cuis6.3 [latest update: #6165] on 22 January 2024 at 9:02:00 pm'!

!Dictionary class methodsFor: 'error descriptions' stamp: 'HAW 1/22/2024 21:00:58'!
keyNotFoundErrorDescriptionFor: key

	^ String streamContents: [ :strm |
			strm nextPutAll: 	'key: '.
			key printOn: strm.
			strm nextPutAll: ' not found' ]! !


!Dictionary methodsFor: 'private' stamp: 'HAW 1/22/2024 20:58:02'!
errorKeyNotFound: key

	self error: (self class keyNotFoundErrorDescriptionFor: key)! !

!methodRemoval: Dictionary #keyNotFoundErrorDescriptionFor: stamp: 'HAW 1/22/2024 20:57:57'!
Dictionary removeSelector: #keyNotFoundErrorDescriptionFor:!

!Dictionary class reorganize!
('instance creation' newFrom: newFromPairs:)
('error descriptions' keyNotFoundErrorDescriptionFor:)
!

