'From Cuis 4.2 of 25 July 2013 [latest update: #2695] on 27 February 2016 at 8:11:18.206656 pm'!
!classDefinition: #Inspector category: #'Tools-Inspector'!
TextProvider subclass: #Inspector
	instanceVariableNames: 'acceptedContentsCache object selectionIndex timeOfLastListUpdate context '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Inspector'!

!Object methodsFor: 'stepping' stamp: 'jmv 2/27/2016 20:07'!
stepAt: millisecondSinceLast in: aWindow
	"Ideally, a model should never know about the existance of a window on it..."! !


!ObjectExplorer methodsFor: 'monitoring' stamp: 'jmv 2/27/2016 20:07'!
stepAt: millisecondSinceLast in: aWindow

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


!ProcessBrowser methodsFor: 'stepping' stamp: 'jmv 2/27/2016 20:07'!
stepAt: millisecondSinceLast in: aWindow

	self updateProcessList! !


!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 2/27/2016 20:07'!
stepAt: millisecondSinceLast in: aSystemWindow

	self updateListsAndCodeIn: aSystemWindow! !


!ChangeSorter methodsFor: 'self-updating' stamp: 'jmv 2/27/2016 20:07'!
stepAt: millisecondSinceLast in: aSystemWindow

	self updateIfNecessary! !


!Inspector methodsFor: 'stepping' stamp: 'jmv 2/27/2016 20:09'!
stepAt: millisecondSinceLast in: aSystemWindow
	| newText |

	newText := self contentsIsString
		ifTrue: [self selection]
		ifFalse: ["keep it short to reduce time to compute it"
			self selectionPrintString ].
	newText = acceptedContentsCache ifFalse: [
		acceptedContentsCache _ newText.
		self acceptedContentsChanged ]! !


!Morph methodsFor: 'stepping' stamp: 'jmv 2/27/2016 20:06'!
stepAt: millisecondSinceLast
	"Do some periodic activity. Use startStepping/stopStepping to start and stop getting sent this message. 
	The desired time between steps is specified by this morph's answer to the stepTime message.
	The millisecondSinceLast parameter gives the time elapsed since the previous step."
	"
	m _ RectangleLikeMorph new.
	m color: Color random.
	m openInWorld.
	m morphPosition: 10@10.
	t _ 0.
	m when: #morphicStep evaluate: [ :delta |
		t _ t + delta.
		t < 10000
			ifTrue: [
				(m owner is: #HandMorph) ifFalse: [
					m morphPosition: 3@2 * t // 100 ]]
			ifFalse: [ m stopStepping ]].
	m startSteppingStepTime: 20.
	"
	self step.
	self triggerEvent: #morphicStep with: millisecondSinceLast! !


!AutoCompleterMorph methodsFor: 'stepping' stamp: 'jmv 2/27/2016 19:42'!
stepAt: millisecondSinceLast

	self timeOfLastActivity > self timeout
		ifTrue: [ self delete. completer menuClosed ]
		ifFalse: [self updateColor]! !


!HandleMorph methodsFor: 'stepping and presenter' stamp: 'jmv 2/27/2016 19:42'!
stepAt: millisecondSinceLast

	pointBlock value: self morphBoundsInWorld center! !


!SystemWindow methodsFor: 'stepping' stamp: 'jmv 2/27/2016 20:07'!
stepAt: millisecondSinceLast
	"If the receiver is not collapsed, step it, after first stepping the model."

	(self isCollapsed not or: [ self wantsStepsWhenCollapsed ]) ifTrue: [
		model ifNotNil: [ model stepAt: millisecondSinceLast in: self ].
		super stepAt: millisecondSinceLast ]! !


!ObjectExplorerWindow methodsFor: 'stepping' stamp: 'jmv 2/27/2016 20:07'!
stepAt: millisecondSinceLast

	super stepAt: millisecondSinceLast.	
	model monitorList isEmpty ifTrue: [
		self stopStepping ]! !


!LayoutAdjustingMorph methodsFor: 'stepping' stamp: 'jmv 2/27/2016 19:42'!
stepAt: millisecondSinceLast
	"got the #mouseLeave: message"

	| p |
	hand ifNil: [
		Cursor currentCursor == self cursor ifTrue: [
			Cursor normal activateCursor ].
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
			self stopStepping.
			"If the button was unpressed outside the morph (can happen if you try to go outside container),
			we might not get the #mouseLeave: message"
			(self morphContainsPoint: (self internalizeFromWorld: p)) ifFalse: [
				hand _ nil.
				Cursor normal activateCursor ]]! !


!UpdatingMenuItemMorph methodsFor: 'stepping' stamp: 'jmv 2/27/2016 20:07'!
stepAt: millisecondSinceLast

	super stepAt: millisecondSinceLast.
	self updateContents! !


!WorldState methodsFor: 'stepping' stamp: 'len 2/26/2016 03:01'!
runLocalStepMethodsIn: aWorld 
	"Run morph 'step' methods (LOCAL TO THIS WORLD) whose time has come. Purge any morphs that are no longer in this world."

	| now morphToStep scheduledTime |
	now _ lastCycleTime.
	self triggerAlarmsBefore: now.
	stepList isEmpty 
		ifTrue: [ ^self].
	[ stepList isEmpty not and: [ stepList first scheduledTime <= now ]] 
		whileTrue: [
			lastStepMessage _ stepList removeFirst.
			morphToStep _ lastStepMessage receiver.
			(morphToStep shouldGetStepsFrom: aWorld) 
				ifTrue: [
					scheduledTime _ lastStepMessage scheduledTime.
					lastStepMessage valueAtTime: now - scheduledTime + lastStepMessage stepTime.
					lastStepMessage ifNotNil: [
							lastStepMessage scheduledTime: (scheduledTime + lastStepMessage stepTime max: now + 1).
							stepList add: lastStepMessage ]].
			lastStepMessage _ nil ]! !

!methodRemoval: Inspector #convertToCurrentVersion:refStream:!
Inspector removeSelector: #convertToCurrentVersion:refStream:!
!methodRemoval: Inspector #timeOfLastListUpdate!
Inspector removeSelector: #timeOfLastListUpdate!
!classDefinition: #Inspector category: #'Tools-Inspector'!
TextProvider subclass: #Inspector
	instanceVariableNames: 'acceptedContentsCache object selectionIndex context'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Inspector'!

!Inspector reorganize!
('accessing' baseFieldList context: fieldList i1 i2 object object: selectedClass selectedClassOrMetaClass update wantsSteps)
('selecting' accept: contentsIsString replaceSelectionValue: selectedSlotName selection selectionIndex selectionPrintString selectionUnmodifiable toggleIndex:)
('code' doItContext doItReceiver)
('updating' updateListsAndCodeIn:)
('private' printStringErrorText)
('stepping' stepAt:in:)
('contents' acceptedStringOrText)
('initialization' initialize inspect:)
('user interface support' autoCompleterClassFor: editorClassFor:)
!

