'From Cuis7.3 [latest update: #7149] on 12 May 2025 at 5:14:00 pm'!

!ArrayedCollection class methodsFor: '[mk] instance creation' stamp: 'jmv 5/12/2025 12:09:36'!
withAll: aSequenceableCollection with: finalElement
	"Create a new collection containing all the elements from aSequenceableCollection."

	| answer |
	answer := (self new: aSequenceableCollection size + 1)
		replaceFrom: 1
		to: aSequenceableCollection size
		with: aSequenceableCollection.
	answer at: answer size put: finalElement.
	^answer! !

