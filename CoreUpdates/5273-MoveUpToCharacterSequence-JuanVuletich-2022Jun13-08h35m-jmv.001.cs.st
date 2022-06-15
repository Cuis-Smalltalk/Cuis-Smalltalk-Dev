'From Cuis 6.0 [latest update: #5271] on 13 June 2022 at 8:38:06 am'!

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/13/2022 08:36:38'!
copyReplacing: rangesToNewStrings

	^self class streamContents: [ :replacementStream | self copyReplacing: rangesToNewStrings into: replacementStream ].! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/13/2022 08:36:15'!
copyReplacing: rangesToNewStrings into: replacementStream

	| lastFrom |

	lastFrom := rangesToNewStrings inject: 1 into: [ :from :aRangeToNewString |
		self
			newFromAfterAppending: aRangeToNewString value
			into: replacementStream
			keepingFrom: from
			skipping: aRangeToNewString key ].

	replacementStream nextPutAll: (self copyFrom: lastFrom to: self size).
! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/13/2022 08:36:59'!
copyReplacing: ranges with: newString

	"Ranges must be in order, with first ranges first. If not, result is unexpected - Hernan"

	^ self class streamContents: [ :replacementStream | self copyReplacing: ranges with: newString into: replacementStream ]
		! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/13/2022 08:36:30'!
copyReplacing: ranges with: newString into: replacementStream

	| lastFrom |

	lastFrom := ranges
		inject: 1
		into: [ :from :aRange |
			self newFromAfterAppending: newString into: replacementStream keepingFrom: from skipping: aRange ].

	replacementStream nextPutAll: (self copyFrom: lastFrom to: self size).! !

!CharacterSequence methodsFor: 'copying' stamp: 'jmv 6/13/2022 08:35:29'!
newFromAfterAppending: aNewString into: replacementStream keepingFrom: from skipping: aRange

	replacementStream
		nextPutAll: (self copyFrom: from to: aRange first - 1);
		nextPutAll: aNewString.

	^ aRange last + 1! !

!methodRemoval: String #copyReplacing:with: stamp: 'jmv 6/13/2022 08:36:59'!
String removeSelector: #copyReplacing:with:!
!methodRemoval: String #copyReplacing: stamp: 'jmv 6/13/2022 08:36:38'!
String removeSelector: #copyReplacing:!
!methodRemoval: String #newFromAfterAppending:into:keepingFrom:skipping: stamp: 'jmv 6/13/2022 08:35:29'!
String removeSelector: #newFromAfterAppending:into:keepingFrom:skipping:!
!methodRemoval: String #copyReplacing:into: stamp: 'jmv 6/13/2022 08:36:15'!
String removeSelector: #copyReplacing:into:!
!methodRemoval: String #copyReplacing:with:into: stamp: 'jmv 6/13/2022 08:36:30'!
String removeSelector: #copyReplacing:with:into:!
