'From Cuis 6.0 [latest update: #5071] on 22 February 2022 at 10:06:47 am'!

!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 2/22/2022 10:06:21'!
keystrokeAction: aBlock
	"Sets the action to perform when user presses any key"

	self setProperty: #keyStroke: toValue: aBlock.! !


!TextModelMorph methodsFor: 'initialization' stamp: 'jmv 2/22/2022 09:44:48'!
keystrokeAction: aBlock
	
	scroller keystrokeAction: aBlock! !

