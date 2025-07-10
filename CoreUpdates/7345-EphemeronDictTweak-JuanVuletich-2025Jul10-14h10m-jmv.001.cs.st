'From Cuis7.5 [latest update: #7343] on 10 July 2025 at 2:10:48 pm'!

!EphemeronIdentityDictionary methodsFor: 'access protected protocol' stamp: 'eem 12/18/2024 11:39:36'!
removeKey: key ifAbsent: aBlock
	"Public protocol: Protect against the EphemeronFinalizationProcess modifying us.
	Note: Usually removing from our instances is done automatically when the key is collected.
	This method usually doesn't need to be called by users of this class."

	^mutex critical: [super removeKey: key ifAbsent: aBlock]! !

