'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 7 June 2016 at 6:10:17.752655 pm'!

!String methodsFor: 'text conversion helpers' stamp: 'len 6/6/2016 02:14'!
struck
	^self asText struck! !


!Text methodsFor: 'adding emphasis' stamp: 'len 6/6/2016 02:15'!
struck
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis struckThrough from: 1 to: string size! !

