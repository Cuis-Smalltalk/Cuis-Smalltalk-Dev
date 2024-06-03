'From Cuis7.1 [latest update: #6429] on 3 June 2024 at 12:06:31 pm'!

!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 6/3/2024 12:05:55'!
keystrokeAction: aBlock
	"Sets the action to perform when user presses any key, after regular processing."

	self setProperty: #keyStroke: toValue: aBlock.! !


!TextModelMorph methodsFor: 'initialization' stamp: 'jmv 6/3/2024 12:06:01'!
keystrokeAction: aBlock
	"Sets the action to perform when user presses any key, after regular processing."
	
	scroller keystrokeAction: aBlock! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 6/3/2024 12:02:53'!
processKeystroke: aKeyboardEvent
	"System level event handling."

	self handlesKeyboard ifTrue: [
		aKeyboardEvent wasHandled ifFalse: [
			self keyStroke: aKeyboardEvent.
			aKeyboardEvent wasHandled: true.
			self 
				valueOfProperty: #'keyStroke:'
				ifPresentDo: [ :handler | handler value: aKeyboardEvent ]]].! !

