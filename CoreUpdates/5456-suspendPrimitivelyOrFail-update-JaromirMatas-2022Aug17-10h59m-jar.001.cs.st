'From Cuis 6.0 [latest update: #5454] on 17 August 2022 at 11:08:18 am'!

!Process methodsFor: 'test support' stamp: 'jar 8/17/2022 10:59:42'!
suspendPrimitivelyOrFail
	"Test support. Execute primitive 578, or fail.
	(We used to call primitive 88, the old suspend primitive, replaced by 578 in 2022)"

	<primitive: 578 error: ec>
	^self primitiveFailed! !

