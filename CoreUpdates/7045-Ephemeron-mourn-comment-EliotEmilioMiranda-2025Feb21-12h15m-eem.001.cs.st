'From Squeak6.1alpha of 20 February 2025 [latest update: #23209] on 21 February 2025 at 12:11:14 pm'!

!Ephemeron methodsFor: 'mourning' stamp: 'eem 2/21/2025 12:11'!
mourn
	"Triggered ephemerons get sent mourn by the #finalizationProcess (via #mournLoopWith:)
	 when the only reference(s) to an ephemeron's key is through one or more ephemerons.
	 i.e. the key is not reachable from the roots of the system except through ephemerons.
	 So ephemerons get sent mourn when their key would otherwise have been garbage
	 collected.  Respond to the information that the key would have been garbage collected
	 other than through ephemeron references by sending finalize to the key.

	 N.B. read #finalizeEphemeron: to understand why #finalize is typically not sent here."
	container
		ifNil: [key finalize]
		ifNotNil: [container finalizeEphemeron: self]! !
