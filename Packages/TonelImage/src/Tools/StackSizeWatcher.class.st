Class {
	#name : 'StackSizeWatcher',
	#superclass : 'Object',
	#instVars : [
		'watcher',
		'stackSizeThreashold',
		'delayBetweenChecks'
	],
	#classInstVars : [
		'current'
	],
	#category : 'Tools-Profiling',
	#package : 'Tools',
	#tag : 'Profiling'
}

{ #category : 'current' }
StackSizeWatcher class >> current [

	current isNil ifTrue: [ current := self new ].
	^current
]

{ #category : 'defaults' }
StackSizeWatcher class >> defaultStackSizeThreshold [

	^2000
]

{ #category : 'defaults' }
StackSizeWatcher class >> defaultTimeBetweenChecks  [

	"Time in milliseconds - Hernan"
	^10
]

{ #category : 'start/stop' }
StackSizeWatcher class >> isWatching [

	^ current notNil and: [ current isWatching ]
]

{ #category : 'current' }
StackSizeWatcher class >> resetCurrent [

	current := nil
]

{ #category : 'start/stop' }
StackSizeWatcher class >> startWatchingAt: aTimeBetweenChecks informingWhenStackSizeBiggerThan: aThreshold [

	^self current startWatchingAt: aTimeBetweenChecks informingWhenStackSizeBiggerThan: aThreshold

]

{ #category : 'start/stop' }
StackSizeWatcher class >> startWatchingWithDefaults [

	^self startWatchingAt: self defaultTimeBetweenChecks informingWhenStackSizeBiggerThan: self defaultStackSizeThreshold
]

{ #category : 'start/stop' }
StackSizeWatcher class >> stopWatching [

	self current stopWatching.
	self resetCurrent	
]

{ #category : 'assertions' }
StackSizeWatcher >> assertIsNotWatching [
	
	self isNotWatching ifFalse: [ self error: 'Already watching' ].
]

{ #category : 'assertions' }
StackSizeWatcher >> assertIsWatching [
	
	self isWatching ifFalse: [ self error: 'It is not watching' ]
]

{ #category : 'testing' }
StackSizeWatcher >> canDebug: aProcess [

	^(ProcessBrowser rulesFor: aProcess) second
	

]

{ #category : 'configuration changing' }
StackSizeWatcher >> changeStackSizeThresholdTo: aThreshold [
	
	stackSizeThreashold := aThreshold 
]

{ #category : 'configuration changing' }
StackSizeWatcher >> changeTimeBetweenChecksTo: aTimeBetweenChecks [

	"time in milliseconds - Hernan"

	delayBetweenChecks := Delay forMilliseconds: aTimeBetweenChecks.

]

{ #category : 'private' }
StackSizeWatcher >> debug: aProcess [

	aProcess debugWithTitle: 'Interrupted - Stack too deep'.

]

{ #category : 'testing' }
StackSizeWatcher >> isNotWatching [

	^self isWatching not
]

{ #category : 'testing' }
StackSizeWatcher >> isStackTooDeepAt: aProcess [

	"aProcess suspendedContext should never be nil under this circunstances but checking that just in case - Hernan"
	^aProcess suspendedContext 
		ifNil: [ false ] 
		ifNotNil: [ :topContext | topContext depthBelow > stackSizeThreashold ]
		
]

{ #category : 'testing' }
StackSizeWatcher >> isWatching [

	^watcher notNil 
]

{ #category : 'testing' }
StackSizeWatcher >> shouldStopAndDebug: aProcess [

	"Verify the process can be debugged before #isStackTooDeepAt: to avoid
	loosing time in #isStackDeeperThan: that is more expensive - Hernan"

	^(self canDebug: aProcess) and: [self isStackTooDeepAt: aProcess]


]

{ #category : 'start/stop' }
StackSizeWatcher >> startWatching [

	self assertIsNotWatching.
	
	watcher := [ [self watch] repeat ] newProcess.
	watcher priority: Processor lowIOPriority.
	watcher name: 'StackSizeWatcher monitor'.
	watcher resume.
	Processor yield
]

{ #category : 'start/stop' }
StackSizeWatcher >> startWatchingAt: aTimeBetweenChecks informingWhenStackSizeBiggerThan: aThreshold [

	self assertIsNotWatching.
	
	self changeTimeBetweenChecksTo: aTimeBetweenChecks.
	self changeStackSizeThresholdTo: aThreshold.
	self startWatching 
]

{ #category : 'start/stop' }
StackSizeWatcher >> stopWatching [

	self assertIsWatching.
	
	watcher terminate.
	watcher := nil
]

{ #category : 'private' }
StackSizeWatcher >> watch [

	| processToWatch |

	"Could happen if process is halted and resumed."
	delayBetweenChecks beingWaitedOn ifTrue: [
		delayBetweenChecks := Delay forMilliseconds: delayBetweenChecks delayDuration ].
	delayBetweenChecks wait.
	processToWatch := Processor nextReadyProcess.
	(self shouldStopAndDebug: processToWatch) ifTrue: [ self debug: processToWatch ]

]
