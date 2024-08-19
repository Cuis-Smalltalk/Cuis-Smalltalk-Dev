'From Cuis7.1 [latest update: #6607] on 19 August 2024 at 3:36:10 pm'!

!OrderedDictionary methodsFor: 'accessing' stamp: 'jmv 8/19/2024 15:29:54'!
atLast: key put: anObject
	"Similar to #at:put:, but the key will be at the end, even if it was already there."
	self at: key put: anObject.
	orderedKeys last = key ifFalse: [
		orderedKeys remove: key; addLast: key ].
	^anObject! !


!SystemWindow class methodsFor: 'menu and shortcuts' stamp: 'jmv 8/19/2024 15:35:21'!
at: aWidgetKey registerMenuAndShortcutsSpecProvider: aMessageSend
	menuNshortcutsProviders ifNil: [ menuNshortcutsProviders := Dictionary new ].
	(menuNshortcutsProviders at: aWidgetKey ifAbsentPut: [ OrderedDictionary new ])
		atLast: aMessageSend put: aMessageSend.! !

