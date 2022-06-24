'From Cuis 6.0 [latest update: #5316] on 24 June 2022 at 5:37:58 pm'!

!Utf8Symbol methodsFor: 'system primitives' stamp: 'di 1/2/1999 17:00'!
flushCache
	"Tell the interpreter to remove all entries with this symbol as a selector from its method lookup cache, if it has one.  This primitive must be called whenever a method is defined or removed.
	NOTE:  Only one of the two selective flush methods needs to be used.
	Squeak 2.3 and later uses 116 (See CompiledMethod flushCache)."

	<primitive: 119>
! !

