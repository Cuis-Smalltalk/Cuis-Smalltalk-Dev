'From Cuis6.3 [latest update: #6347] on 3 May 2024 at 11:00:58 am'!

!LiteralNode methodsFor: 'code generation (new scheme)' stamp: 'jmv 5/3/2024 10:55:28'!
sizeCodeForEffect: encoder

	"Include im method literals even if unused"
	self reserve: encoder.
	^0! !

