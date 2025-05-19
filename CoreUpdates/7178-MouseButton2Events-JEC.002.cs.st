'From Cuis7.3 [latest update: #7158] on 15 May 2025 at 12:09:56 pm'!

!Morph methodsFor: 'events' stamp: 'JEC 5/15/2025 10:09:39'!
mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 2 down event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"

	self
		valueOfProperty: #'mouseButton2Down:localPosition:'
		ifPresentDo: [ :handler |
			handler value: aMouseButtonEvent value: localEventPosition.
			^self ].

	self mouseButton2Activity.! !

!Morph methodsFor: 'events' stamp: 'JEC 5/15/2025 10:09:53'!
mouseButton2Up: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse button 2 up event.
	This message will only be sent to Morphs that answer true to #handlesMouseDown:"

	self
		valueOfProperty: #'mouseButton2Up:localPosition:'
		ifPresentDo: [ :handler | handler value: aMouseButtonEvent value: localEventPosition ]! !

