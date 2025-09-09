'From Cuis7.5 [latest update: #7531] on 9 September 2025 at 11:40:39 am'!

!MouseEnterEvent methodsFor: 'dispatching' stamp: 'jmv 9/9/2025 10:11:57'!
dispatchIn: neverMind
	"We are sent directly to recipient via #sendEventTo:"

	self shouldNotHappen! !


!MouseLeaveEvent methodsFor: 'dispatching' stamp: 'jmv 9/9/2025 10:12:02'!
dispatchIn: neverMind
	"We are sent directly to recipient via #sendEventTo:"

	self shouldNotHappen! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/9/2025 09:54:39'!
startMouseDispatch: aMouseEvent
	"For MouseEvents with default dispatch behavior"

	self hasSubmorphs
		ifFalse: [
			aMouseEvent dispatchIn: owner ].! !

