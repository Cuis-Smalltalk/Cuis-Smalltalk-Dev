'From Cuis 5.0 [latest update: #4360] on 5 October 2020 at 10:52:58 am'!

!SystemDictionary methodsFor: 'system attributes' stamp: 'jmv 10/5/2020 10:52:42'!
maxIdentityHash
	"Answer the maximum identityHash value supported by the VM.
	Usually the VM is able to answer. It seems that SqueakJS (as of October 5, 2020) isn't.
	On primitive failure, still give a correct answer."

	<primitive: 176>	
	^self isSpur
		ifTrue: [ 16r3FFFFF ]							"22 bits in Spur"
		ifFalse: [ 16rFFF ]							"12 bits in V3 images"! !

