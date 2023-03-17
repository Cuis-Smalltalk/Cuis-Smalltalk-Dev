'From Cuis 6.0 [latest update: #5699] on 17 March 2023 at 6:17:52 pm'!

!EventSensor methodsFor: 'accessing' stamp: 'jmv 3/16/2023 13:22:18'!
shiftPressed
	"Answer whether the shift key on the keyboard is being held down."

	^ mouseButtons anyMask: InputSensor shiftKey! !

