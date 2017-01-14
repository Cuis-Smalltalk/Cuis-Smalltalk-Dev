'From Cuis 5.0 of 7 November 2016 [latest update: #3025] on 12 January 2017 at 6:54:23 pm'!

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'HAW 1/12/2017 18:29:00'!
allSuperclassesUpTo: aSuperclass
	
	| superclasses |
	
	^ superclass = aSuperclass
		ifTrue: [ OrderedCollection with: aSuperclass]
		ifFalse: [superclasses _ superclass allSuperclassesUpTo: aSuperclass.
			superclasses addFirst: superclass.
			superclasses]! !

!Behavior methodsFor: 'accessing class hierarchy' stamp: 'HAW 1/12/2017 18:30:53'!
withAllSuperclassesUpTo: aSuperclass

	| classes |
	
	classes _ self allSuperclassesUpTo: aSuperclass.
	classes addFirst: self.
	
	^ classes! !


!ContextPart methodsFor: 'debugger access' stamp: 'HAW 1/12/2017 17:48:35'!
arguments

	| arguments |

	arguments _ Array new: self selector numArgs.
	1 to: arguments size do: [ :index | arguments at: index put: (self tempAt: index)].

	^arguments.
	
	! !

!ContextPart methodsFor: 'debugger access' stamp: 'HAW 1/12/2017 17:47:30'!
messageForYourself

	^Message selector: self selector arguments: self arguments.
	! !


!Debugger methodsFor: 'method creation' stamp: 'HAW 1/12/2017 18:31:14'!
askForSuperclassOf: aClass upTo: aSuperclass toImplement: aSelector ifCancel: cancelBlock

	| classes chosenClassIndex |

	classes _ aClass withAllSuperclassesUpTo: aSuperclass. 
	chosenClassIndex _ PopUpMenu
		withCaption: 'Define #', aSelector, ' in which class?'
		chooseFrom: (classes collect: [:c | c name]).
	chosenClassIndex = 0 ifTrue: [^ cancelBlock value].

	^ classes at: chosenClassIndex! !

!Debugger methodsFor: 'method creation' stamp: 'HAW 1/12/2017 18:45:03'!
createMethodOnSubclassResponsibility

	| message chosenClass subclassResponsibilityContext |
	
	subclassResponsibilityContext _ self interruptedContext sender sender.
	message _ subclassResponsibilityContext messageForYourself.
	
	chosenClass _ self
		askForSuperclassOf: subclassResponsibilityContext receiver class
		upTo: subclassResponsibilityContext method methodClass
		toImplement: message selector
		ifCancel: [^self].
		
	self implement: message inClass: chosenClass context: subclassResponsibilityContext 

! !

!Debugger methodsFor: 'method creation' stamp: 'HAW 1/12/2017 18:43:11'!
createMethodWhenDoesNotUndertand

	| message chosenClass interruptedContext |
	
	interruptedContext _ self interruptedContext.
	message _ interruptedContext tempAt: 1.
	
	chosenClass _ self
		askForSuperclassOf: interruptedContext receiver class
		toImplement: message selector
		ifCancel: [^self].
	
	self implement: message inClass: chosenClass context: self selectedContext

! !

!Debugger methodsFor: 'method creation' stamp: 'HAW 1/12/2017 18:43:25'!
implement: aMessage inClass: aClass context: aContext 
	
	aClass
		compile: aMessage createStubMethod
		classified: (self askForCategoryIn: aClass default: 'as yet unclassified').
		
	aContext privRefreshWith: (aClass lookupSelector: aMessage selector).
	aMessage arguments withIndexDo: [ :arg :index | aContext tempAt: index put: arg ].
	self resetContext: aContext! !

!Debugger methodsFor: 'method creation' stamp: 'HAW 1/12/2017 17:22:28'!
wasInterrupedOnDoesNotUnderstand

	^self interruptedContext selector == #doesNotUnderstand:! !

!Debugger methodsFor: 'method creation' stamp: 'HAW 1/12/2017 17:25:23'!
wasInterruptedOnSubclassResponsibility

	^self interruptedContext sender ifNil: [ false ] ifNotNil: [ :senderContext | senderContext selector == #subclassResponsibility ]! !


!Debugger methodsFor: 'method creation' stamp: 'HAW 1/12/2017 18:31:48'!
askForSuperclassOf: aClass toImplement: aSelector ifCancel: cancelBlock

	^self askForSuperclassOf: aClass upTo: ProtoObject toImplement: aSelector ifCancel: cancelBlock
! !

!Debugger methodsFor: 'method creation' stamp: 'HAW 1/12/2017 18:46:00'!
createMethod

	self wasInterrupedOnDoesNotUnderstand ifTrue: [ ^self createMethodWhenDoesNotUndertand ].
	self wasInterruptedOnSubclassResponsibility ifTrue: [ ^self createMethodOnSubclassResponsibility ].
	
	self inform: 'Only available for #doesNotUndertand: and #subclassResponsibility' ! !

!Debugger methodsFor: 'method creation' stamp: 'HAW 1/12/2017 17:22:47'!
shouldBeAbleToCreateMethod

	^self wasInterrupedOnDoesNotUnderstand or: [ self wasInterruptedOnSubclassResponsibility]! !


!DebuggerWindow methodsFor: 'GUI building' stamp: 'HAW 1/12/2017 18:46:32'!
customButtonSpecs
	"Answer an array of elements of the form wording, selector, help-message, that characterize the custom button row of a debugger."

	^#(
		('Restart'		restart				'reset this context to its start.')
		('Into'			send					'step Into message sends')
		('Over'			doStep				'step Over message sends')
		('Through'		stepIntoBlock		'step into a block')
		('Full Stack'	fullStack			'show full stack')
		('Where'		where				'select current pc range')
		('Create'		createMethod		'create method'))! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'HAW 1/12/2017 18:47:35'!
buttonRowForPreDebugWindow: aDebugger
	| row aButton triads buttons |
	buttons _ OrderedCollection new.
	triads _ OrderedCollection withAll: self preDebugButtonSpec.
	aDebugger shouldBeAbleToCreateMethod ifTrue: [ triads add: { 'Create'. #createMethod. 'create the missing method' }].
	
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

!methodRemoval: Debugger #create!
Debugger removeSelector: #create!
!methodRemoval: Debugger #implement:inClass:!
Debugger removeSelector: #implement:inClass:!

!Debugger reorganize!
('accessing' acceptedStringOrText contents:notifying: contextVariablesInspector debuggerMap interruptedContext interruptedProcess interruptedProcessUI: isNotifier labelString labelString: proceedValue proceedValue: receiver receiverInspector receiverInspectorObject:context:)
('notifier menu' storeLog)
('context stack (message list)' contextStackIndex contextStackList expandStack fullyExpandStack messageListIndex selectedMessage selectedMessageName toggleContextStackIndex:)
('context stack menu' askForCategoryIn:default: doStep down fullStack peelToFirst proceed restart returnValue: selectPC send stepIntoBlock up where)
('code pane' contentsSelection doItContext doItReceiver pc pcRange)
('code pane menu' runToSelection:)
('message category list')
('class list' selectedClass)
('dependents access' updateInspectors wantsSteps)
('private' checkContextSelection contextStackIndex:oldContextWas: externalInterrupt: lowSpaceChoices newStack: process:context: resetContext: resumeProcess selectedContext)
('breakpoints' toggleBreakOnEntry)
('shout styling' bindingNamesDo: hasBindingOf: shoutAboutToStyle:)
('initialization' initialize openFullAt: openFullMorphicLabel: openFullNoSuspendLabel: openNotifierContents:label: sendProceeds windowIsClosing)
('method creation' askForSuperclassOf:toImplement:ifCancel: askForSuperclassOf:upTo:toImplement:ifCancel: createMethod createMethodOnSubclassResponsibility createMethodWhenDoesNotUndertand implement:inClass:context: shouldBeAbleToCreateMethod wasInterrupedOnDoesNotUnderstand wasInterruptedOnSubclassResponsibility)
!

