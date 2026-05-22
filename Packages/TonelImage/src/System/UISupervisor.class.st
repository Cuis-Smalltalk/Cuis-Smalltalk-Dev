"
UISupervisor is an interface to User Interface services not tied to any specific GUI. There could even be no GUI.
All state and behavior is on the class side
"
Class {
	#name : 'UISupervisor',
	#superclass : 'Object',
	#classVars : [
		'UI',
		'UIProcess'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'accessing' }
UISupervisor class >> animatedUIOf: aProcess [
	"
	UISupervisor animatedUIOf: UIProcess
	[ (UISupervisor animatedUIOf: Processor activeProcess) print ] fork
	"
	^aProcess == UIProcess ifTrue: [ UI ]
]

{ #category : 'accessing' }
UISupervisor class >> isUIProcess: aProcess [
	^UIProcess == aProcess
]

{ #category : 'services' }
UISupervisor class >> isUIProcessRunning [
	"Is the currently running process the UI Process?"

	^UIProcess notNil and: [ UIProcess isRunning ].
]

{ #category : 'gui process' }
UISupervisor class >> newUIProcess [
	"Start a new UI process for the current UI.
	The current UI process (if any) will exit at the end of the current UI cycle."
	"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."

	self spawnNewMorphicProcessFor: UI.
]

{ #category : 'accessing' }
UISupervisor class >> runningWorld [
	"
	UISupervisor runningWorld
	[ UISupervisor runningWorld print ] fork
	"
	^self animatedUIOf: Processor activeProcess
]

{ #category : 'gui process' }
UISupervisor class >> spawnNewMorphicProcessFor: guiRootObject [
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	"If possible, reuse VectorEngine, avoiding the allocation of new large arrays for it."
	self spawnNewMorphicProcessFor: guiRootObject createNewCanvas: true.
]

{ #category : 'gui process' }
UISupervisor class >> spawnNewMorphicProcessFor: guiRootObject createNewCanvas: createNewCanvas [
	"
	aBoolean ifTrue terminate current process. Use with care.
	Maybe prefer the safer #spawnNewMorphicProcessFor:
	"
	| doYield |
	doYield := false.
	"If the old one gets resumed, don't step on each other feet"
	UIProcess ifNotNil: [
		UIProcess
			name: 'Formerly: ', UIProcess name;
			priority: UIProcess priority-1.
		UIProcess isRunning ifTrue: [ doYield := true ]].
	UI := guiRootObject.
	UISupervisor uiProcess: (guiRootObject runProcess: createNewCanvas).
	doYield ifTrue: [ Processor yield ].
]

{ #category : 'gui process' }
UISupervisor class >> stopUIProcess [
	UIProcess ifNotNil: [
		UIProcess terminate ].
	UI := nil.
	UIProcess := nil.
]

{ #category : 'gui process' }
UISupervisor class >> ui [
	^UI
]

{ #category : 'gui process' }
UISupervisor class >> uiProcess: aProcess [
	UIProcess := aProcess.
	UIProcess
		name: 'Morphic UI';
		priority: Processor userSchedulingPriority;
		resume.
]

{ #category : 'gui process' }
UISupervisor class >> uiProcessIfCantRun [
	"If the system needs a UIProcess (we know because UIProcess is not nil),
	then ensure that the UIProcess is ready to run, in order to hava a responsive UI.
	If we needed to create a new UI process, answer the old one, as it is most likely the
	process the user is interested in debugging.
	If there is an UIProcess, and it can run, answer nil.
	If there's no UIProcess at all, also answer nil.
	See senders."

	UIProcess ifNotNil: [ :oldUIProcess |
		oldUIProcess isBlocked | oldUIProcess isSuspended | oldUIProcess isTerminated ifTrue: [
			^oldUIProcess ].
		UI millisecondsSinceLastCycle > 500 ifTrue: [
			^oldUIProcess ].
		].
	^nil
]

{ #category : 'services' }
UISupervisor class >> whenUIinSafeState: evaluableObject [
	"If there is an UI present, evaluate argument when such UI is in a safe state.
	If not, just evaluate the argument right now."
	self ui
		ifNotNil: [ :guiRootObject | guiRootObject whenUIinSafeState: evaluableObject ]
		ifNil: evaluableObject
]
