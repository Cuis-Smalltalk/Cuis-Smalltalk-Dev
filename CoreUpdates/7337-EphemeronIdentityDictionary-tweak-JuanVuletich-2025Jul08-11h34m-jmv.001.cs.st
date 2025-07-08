'From Cuis7.5 [latest update: #7336] on 8 July 2025 at 11:34:54 am'!

!EphemeronIdentityDictionary methodsFor: 'finalization' stamp: 'jmv 7/8/2025 09:49:17'!
finalizeEphemeron: anEphemeron

	"Remove the ephemeron from myself.  Note that no finalization of the ephemeron's key is done.
	 Here we are using ephemerons to clean up property tables."

	self removeKey: anEphemeron key ifAbsent: nil! !

