'From Cuis7.3 [latest update: #6917] on 12 December 2024 at 4:26:01 pm'!

!Object methodsFor: 'finalization' stamp: 'JMM 11/20/2024 17:29:59'!
mourn
	"Sent by the new Finalization mechanism.
	All weak objects (instances of weak classes) and Ephemerons may be mourned if they point to an object that needs to be collected.

	This default implementation does nothing. To override in specific subclasses."! !

