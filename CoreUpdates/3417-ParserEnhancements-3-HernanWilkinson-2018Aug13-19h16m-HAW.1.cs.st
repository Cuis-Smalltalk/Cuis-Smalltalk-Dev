'From Cuis 5.0 of 7 November 2016 [latest update: #3411] on 13 August 2018 at 7:45:27 pm'!

!Encoder methodsFor: 'temps' stamp: 'HAW 8/13/2018 19:44:54'!
bindBlockArg: name within: aBlockNode range: range
	
	^self addMultiRange: range for: (self bindBlockArg: name within: aBlockNode)
	
! !

!Encoder methodsFor: 'temps' stamp: 'HAW 8/13/2018 19:45:13'!
bindBlockTemp: name within: aBlockNode range: range

	^self addMultiRange: range for: (self bindBlockTemp: name within: aBlockNode)
	
! !

