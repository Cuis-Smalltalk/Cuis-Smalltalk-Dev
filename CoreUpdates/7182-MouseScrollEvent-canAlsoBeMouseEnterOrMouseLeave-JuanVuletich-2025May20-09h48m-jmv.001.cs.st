'From Cuis7.3 [latest update: #7181] on 20 May 2025 at 9:50:50 am'!

!MouseEvent methodsFor: 'dispatching' stamp: 'jmv 5/20/2025 09:50:22'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph.
	See #asMouseEnter, #asMouseLeave, #asMouseMove, #asMouseOver."

	type == #mouseOver ifTrue: [
		^aMorph processMouseOver: self localPosition: (aMorph internalizeFromWorld: position) ].
	type == #mouseEnter ifTrue: [
		^ aMorph processMouseEnter: self localPosition: (aMorph internalizeFromWorld: position) ].
	type == #mouseLeave ifTrue: [
		^aMorph processMouseLeave: self localPosition: (aMorph internalizeFromWorld: position) ].

	"Backstop. Just in case."
	^ super sendEventTo: aMorph! !


!MouseButtonEvent methodsFor: 'dispatching' stamp: 'jmv 5/20/2025 09:49:47'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"

	type == #mouseDown ifTrue: [
		^ aMorph processMouseDown: self localPosition: (aMorph internalizeFromWorld: position) ].
	type == #mouseUp ifTrue: [
		^ aMorph processMouseUp: self localPosition: (aMorph internalizeFromWorld: position) ].

	"Because any MouseEvent can be of the kinds handled by super."
	^super sendEventTo: aMorph! !


!MouseMoveEvent methodsFor: 'dispatching' stamp: 'jmv 5/20/2025 09:49:32'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"

	type == #mouseMove ifTrue: [
		^ aMorph processMouseMove: self localPosition: (aMorph internalizeFromWorld: position) ].

	"Because any MouseEvent can be of the kinds handled by super."
	^ super sendEventTo: aMorph! !


!MouseScrollEvent methodsFor: 'dispatching' stamp: 'jmv 5/20/2025 09:49:24'!
sendEventTo: aMorph
	"Dispatch the receiver into anObject"

	type == #mouseScroll ifTrue: [
		^ aMorph processMouseScroll: self localPosition: (aMorph internalizeFromWorld: position) ].

	"Because any MouseEvent can be of the kinds handled by super."
	^ super sendEventTo: aMorph! !

