'From Cuis7.5 [latest update: #7669] on 22 October 2025 at 3:17:28 pm'!

!CharacterSequence methodsFor: 'text conversion color' stamp: 'jmv 10/22/2025 15:15:34'!
color: aColor
	"Stuff like
	'Hello world' color: (Color fromHexString: '#941100') :: edit
	'Hello world' color: (Color fromHexString: '#5A9F2D') :: edit
	"
	^self asText color: aColor! !


!Text methodsFor: 'adding color' stamp: 'jmv 10/22/2025 15:15:52'!
color: aColor
	"Stuff like
	'Hello world' color: (Color fromHexString: '#941100') :: edit
	'Hello world' color: (Color fromHexString: '#5A9F2D') :: edit
	"
	self addAttribute: (TextColor color: aColor)! !

