'From Cuis 4.2 of 25 July 2013 [latest update: #2766] on 18 May 2016 at 10:43:22.763457 am'!

!Object methodsFor: 'user interface' stamp: 'jmv 5/18/2016 10:40'!
notify: aString 
	"Create and schedule a Notifier with the argument as the message in 
	order to request confirmation before a process can proceed."

	Warning signal: aString

	"
	nil notify: 'confirmation message'
	"! !


!InputSensor methodsFor: 'modifier keys' stamp: 'jmv 5/18/2016 10:39'!
macOptionKeyPressed
	"Answer whether the option key on the Macintosh keyboard is being held down. Macintosh specific."

	self notify: 'Portability note:
InputSensor>>macOptionKeyPressed is not portable.
Please use InputSensor>>mouseButton2Pressed instead!!'.

	^ self rawMacOptionKeyPressed! !


!UserInputEvent methodsFor: 'modifier state' stamp: 'jmv 5/18/2016 10:39'!
macOptionKeyPressed
	"Answer whether the option key on the Macintosh keyboard was being held down when this event occurred. Macintosh specific."

	self notify: 'Portability note:
MorphicEvent>>macOptionKeyPressed is not portable.
Please use MorphicEvent>>mouseButton2Pressed instead!!'.

	^ self rawMacOptionKeyPressed! !

!methodRemoval: Object #notifyWithLabel:!
Object removeSelector: #notifyWithLabel:!
