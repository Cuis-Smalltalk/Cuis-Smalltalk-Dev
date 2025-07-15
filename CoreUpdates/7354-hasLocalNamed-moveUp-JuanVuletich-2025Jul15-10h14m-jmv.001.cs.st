'From Cuis7.5 [latest update: #7353] on 15 July 2025 at 10:14:27 am'!

!BytecodeEncoder methodsFor: 'testing' stamp: 'jmv 7/15/2025 10:10:03'!
hasLocalNamed: aName

	^ scopeTable includesKey: aName ! !

!methodRemoval: EncoderForV3PlusClosures #hasLocalNamed: stamp: 'jmv 7/15/2025 10:14:11'!
EncoderForV3PlusClosures removeSelector: #hasLocalNamed:!
