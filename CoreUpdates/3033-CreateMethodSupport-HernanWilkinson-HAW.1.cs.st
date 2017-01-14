'From Cuis 5.0 of 7 November 2016 [latest update: #3025] on 12 January 2017 at 5:14:29 pm'!

!Debugger methodsFor: 'as yet unclassified' stamp: 'HAW 1/12/2017 17:05:16'!
askForSuperclassOf: aClass toImplement: aSelector ifCancel: cancelBlock

	| classes chosenClassIndex |

	classes _ aClass withAllSuperclasses.
	chosenClassIndex _ PopUpMenu
		withCaption: 'Define #', aSelector, ' in which class?'
		chooseFrom: (classes collect: [:c | c name]).
	chosenClassIndex = 0 ifTrue: [^ cancelBlock value].

	^ classes at: chosenClassIndex! !

!Debugger methodsFor: 'as yet unclassified' stamp: 'HAW 1/12/2017 17:09:46'!
create

	self shouldBeAbleToCreateMethod 
		ifTrue: [ self createMethod ]
		ifFalse: [ self inform: 'Only available for doesNotUndertand:' ]! !

!Debugger methodsFor: 'as yet unclassified' stamp: 'HAW 1/12/2017 17:04:29'!
createMethod

	| message chosenClass interruptedContext |
	
	interruptedContext _ self interruptedContext.
	message _ interruptedContext tempAt: 1.
	chosenClass _ self
		askForSuperclassOf: interruptedContext receiver class
		toImplement: message selector
		ifCancel: [^self].
	self implement: message inClass: chosenClass.
! !

!Debugger methodsFor: 'as yet unclassified' stamp: 'HAW 1/12/2017 17:01:22'!
shouldBeAbleToCreateMethod

	^self interruptedContext selector == #doesNotUnderstand:! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'HAW 1/12/2017 17:04:50'!
customButtonSpecs
	"Answer an array of elements of the form wording, selector, help-message, that characterize the custom button row of a debugger."

	^#(
		('Restart'		restart				'reset this context to its start.')
		('Into'			send					'step Into message sends')
		('Over'			doStep				'step Over message sends')
		('Through'		stepIntoBlock		'step into a block')
		('Full Stack'	fullStack			'show full stack')
		('Where'		where				'select current pc range')
		('Create'		create 				'create method'))! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'HAW 1/12/2017 17:06:02'!
buttonRowForPreDebugWindow: aDebugger
	| row aButton triads buttons |
	buttons _ OrderedCollection new.
	triads _ OrderedCollection withAll: self preDebugButtonSpec.
	(aDebugger shouldBeAbleToCreateMethod) ifTrue: [
		triads add: { 'Create'. #createMethod. 'create the missing method' }
	].
	triads do: [ :triad |
		aButton _ PluggableButtonMorph new model: self.
		aButton label: triad first.
		aButton action: triad second.
		aButton setBalloonText: triad third.
		buttons add: aButton].
	
	row _ LayoutMorph newRow.
	row doAdoptWidgetsColor.
	row color: Color transparent.
	row separation: 1.
	row addMorphs: buttons.
	^row! !

!PreDebugWindow methodsFor: 'button actions' stamp: 'HAW 1/12/2017 17:06:43'!
createMethod
	"Should only be called when this Debugger was created in response to a
	MessageNotUnderstood exception. Create a stub for the method that was
	missing and proceed into it."
	
	model createMethod.
	self debug
! !

!methodRemoval: PreDebugWindow #askForSuperclassOf:toImplement:ifCancel:!
PreDebugWindow removeSelector: #askForSuperclassOf:toImplement:ifCancel:!
