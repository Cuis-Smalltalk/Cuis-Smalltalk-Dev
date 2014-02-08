'From Cuis 4.2 of 25 July 2013 [latest update: #1957] on 3 February 2014 at 11:14:03.114024 pm'!

!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 2/3/2014 23:07'!
stepAt: millisecondClockValue in: aWindow

	"If there's anything in my monitor list, see if the strings have changed."
	| string changes |
	changes _ false.
	self monitorList keysAndValuesDo: [ :k :v |
		k ifNotNil: [
			k refresh.
			(string _ k asString) ~= v ifTrue: [
				self monitorList at: k put: string. changes _ true ]]].
	changes ifTrue: [ | sel |
		sel _ currentSelection.
		self changed: #getList.
		self noteNewSelection: sel ]! !


!ProcessBrowser methodsFor: 'stepping' stamp: 'jmv 2/3/2014 23:08'!
stepAt: millisecondClockValue in: aWindow

	self updateProcessList! !


!AutoCompleterMorph methodsFor: 'stepping' stamp: 'jmv 2/3/2014 22:53'!
stepAt: millisecondClockValue

	self timeOfLastActivity > self timeout
		ifTrue: [ self delete. completer menuClosed ]
		ifFalse: [self updateColor]! !


!HandleMorph methodsFor: 'stepping and presenter' stamp: 'jmv 2/3/2014 22:53'!
stepAt: millisecondClockValue

	pointBlock value: self morphBoundsInWorld center! !


!MagnifierMorph methodsFor: 'stepping' stamp: 'jmv 2/3/2014 22:53'!
stepAt: millisecondClockValue

	self redrawNeeded! !


!HaloMorph methodsFor: 'stepping' stamp: 'jmv 2/3/2014 22:54'!
stepAt: millisecondClockValue

	self updatePositionAndExtent! !


!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 2/3/2014 22:53'!
stepAt: millisecondClockValue
	"got the #mouseLeave: message"

	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal show ].
		^self stopStepping ].

	"hasn't got the #mouseLeave: message (yet)"
	p _ hand morphPosition.
	hand lastMouseEvent mouseButton1Pressed
		ifTrue: [
			self adjustOwnerAt: p.
			Preferences cheapWindowReframe ifTrue: [
				owner morphBoundsInWorld newRectFrom: [ :f |
					self adjustOwnerAt: Sensor mousePoint.
					owner morphBoundsInWorld ]].
			]
		ifFalse: [
			owner redrawNeeded.
			self stopStepping.
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor normal show ]]! !


!UpdatingMenuItemMorph methodsFor: 'stepping' stamp: 'jmv 2/3/2014 22:53'!
stepAt: millisecondClockValue

	super stepAt: millisecondClockValue.
	self updateContents! !


!UpdatingStringMorph methodsFor: 'stepping' stamp: 'jmv 2/3/2014 22:53'!
stepAt: millisecondClockValue

	self contents: (target perform: getSelector)! !


!Object methodsFor: 'stepping' stamp: 'jmv 2/3/2014 23:10'!
stepAt: millisecondClockValue in: aWindow
	"Ideally, a model should never know about the existance of a window on it..."! !


!Morph methodsFor: 'stepping' stamp: 'jmv 2/3/2014 22:57'!
stepAt: millisecondClockValue
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. The time between steps is specified by this morph's answer to the stepTime message.
	The millisecondClockValue parameter gives the value of the millisecond clock at the moment of dispatch."! !


!ObjectExplorerWindow methodsFor: 'stepping' stamp: 'jmv 2/3/2014 23:07'!
stepAt: millisecondClockValue

	super stepAt: millisecondClockValue.	
	model monitorList isEmpty ifTrue: [
		self stopStepping ]! !

!methodRemoval: UpdatingStringMorph #step!
UpdatingStringMorph removeSelector: #step!
!methodRemoval: UpdatingMenuItemMorph #step!
UpdatingMenuItemMorph removeSelector: #step!
!methodRemoval: LayoutAdjustingMorph #step!
LayoutAdjustingMorph removeSelector: #step!
!methodRemoval: HaloMorph #step!
HaloMorph removeSelector: #step!
!methodRemoval: MagnifierMorph #step!
MagnifierMorph removeSelector: #step!
!methodRemoval: HandleMorph #step!
HandleMorph removeSelector: #step!
!methodRemoval: AutoCompleterMorph #step!
AutoCompleterMorph removeSelector: #step!
!methodRemoval: Morph #step!
Morph removeSelector: #step!
!methodRemoval: ContextPart #zstep!
ContextPart removeSelector: #zstep!
!methodRemoval: ProcessBrowser #step!
ProcessBrowser removeSelector: #step!
!methodRemoval: ProcessBrowser #zstep!
ProcessBrowser removeSelector: #zstep!
!methodRemoval: ObjectExplorer #step!
ObjectExplorer removeSelector: #step!
!methodRemoval: ObjectExplorer #zstep!
ObjectExplorer removeSelector: #zstep!
