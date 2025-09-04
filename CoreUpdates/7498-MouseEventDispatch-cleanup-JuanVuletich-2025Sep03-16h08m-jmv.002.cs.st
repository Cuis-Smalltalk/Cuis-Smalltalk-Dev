'From Cuis7.5 [latest update: #7495] on 3 September 2025 at 4:15:34 pm'!

!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 16:14:25'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph."

	type == #mouseDown ifTrue: [
		aMorph processMouseDown: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].
	type == #mouseUp ifTrue: [
		aMorph processMouseUp: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].

	"Because any MouseEvent can be of the kinds handled by super."
	super sendEventTo: aMorph.! !


!MouseEnterEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 16:14:19'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph."

	aMorph processMouseEnter: self localPosition: (aMorph internalizeFromWorld: position).! !


!MouseLeaveEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 16:13:45'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph."

	aMorph processMouseLeave: self localPosition: (aMorph internalizeFromWorld: position).! !


!MouseMoveEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 16:14:13'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph."

	aMorph processMouseMove: self localPosition: (aMorph internalizeFromWorld: position).! !


!MouseOverEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 16:10:35'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph."

	aMorph processMouseOver: self localPosition: (aMorph internalizeFromWorld: position).! !


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 16:14:04'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph."

	aMorph processMouseScroll: self localPosition: (aMorph internalizeFromWorld: position).! !

!methodRemoval: MouseEvent #sendEventTo: stamp: 'jmv 9/3/2025 16:14:40'!
MouseEvent removeSelector: #sendEventTo:!
