Class {
	#name : 'HoverableButtonMorph',
	#superclass : 'PluggableButtonMorph',
	#instVars : [
		'mouseEnterSelector',
		'mouseLeaveSelector'
	],
	#category : 'Morphic-Widgets',
	#package : 'Morphic',
	#tag : 'Widgets'
}

{ #category : 'new-morph participation' }
HoverableButtonMorph class >> includeInNewMorphMenu [
	"Not to be instantiated from the menu"
	^ false
]

{ #category : 'instance creation' }
HoverableButtonMorph class >> model: anObject stateGetter: getStateSel action: actionSel onMouseEnterSend: aMouseEnterSelector onMouseLeaveSend: aMouseLeaveSelector [

	^ self new
		model: anObject
		stateGetter: getStateSel
		action: actionSel
		onMouseEnterSend: aMouseEnterSelector
		onMouseLeaveSend: aMouseLeaveSelector
]

{ #category : 'initialization' }
HoverableButtonMorph >> model: anObject stateGetter: getStateSel action: actionSel onMouseEnterSend: aMouseEnterSelector onMouseLeaveSend: aMouseLeaveSelector [

	self model: anObject stateGetter: getStateSel action: actionSel label: nil.
	mouseEnterSelector := aMouseEnterSelector.
	mouseLeaveSelector := aMouseLeaveSelector.
]

{ #category : 'events' }
HoverableButtonMorph >> mouseEnter: aMouseEnterEvent [
	
	mouseEnterSelector ifNotNil: [ model perform: mouseEnterSelector ].
	super mouseEnter: aMouseEnterEvent
]

{ #category : 'events' }
HoverableButtonMorph >> mouseLeave: aMouseLeaveEvent [
	
	mouseLeaveSelector ifNotNil: [
		model perform: mouseLeaveSelector ].
	super mouseLeave: aMouseLeaveEvent.
]
