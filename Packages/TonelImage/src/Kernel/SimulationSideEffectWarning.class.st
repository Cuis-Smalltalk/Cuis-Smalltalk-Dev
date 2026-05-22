"
I am signaled to notify the client of the simulator (i.e., a sender of Context>>step) about potential side effects of the next instruction to be executed that would escape the control of the simulator. For example, I am signaled before the simulated code starts another process. See Context>>doPrimitive:method:receiver:args:, my messageText, and Parser>>simulationGuard for more information.
"
Class {
	#name : 'SimulationSideEffectWarning',
	#superclass : 'Warning',
	#instVars : [
		'primitiveIndex',
		'context',
		'method',
		'receiver',
		'arguments',
		'suppressed'
	],
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'instance creation' }
SimulationSideEffectWarning class >> forPrimitive: primitiveIndex [

	^ self new primitive: primitiveIndex
]

{ #category : 'accessing' }
SimulationSideEffectWarning >> arguments [

	^ arguments
]

{ #category : 'accessing' }
SimulationSideEffectWarning >> context [

	^ context
]

{ #category : 'initialize-release' }
SimulationSideEffectWarning >> context: aContext method: aCompiledMethod receiver: rcvr arguments: args [

	context := aContext.
	method := aCompiledMethod.
	receiver := rcvr.
	arguments := args.
]

{ #category : 'priv handling' }
SimulationSideEffectWarning >> defaultAction [

	self suppressed ifFalse: [super defaultAction].
	self flag: #forLater. "When we support explicit exception handler invocation (e.g., #resume, #retry) from the debugger, this exception should publish a #resume handler rather than relying on the weakly defined proceed semantics of the debugger."
	^ self defaultResumeValue
]

{ #category : 'defaults' }
SimulationSideEffectWarning >> defaultResumeValue [

	^ true
]

{ #category : 'testing' }
SimulationSideEffectWarning >> isControlPrimitive [
	"See StackInterpreter class>>#initializePrimitiveTable."

	^ self primitive between: 80 and: 89
]

{ #category : 'testing' }
SimulationSideEffectWarning >> isSimulationGuard [
	"See Parser >> #simulationGuard."

	^ self primitive = 19
]

{ #category : 'printing' }
SimulationSideEffectWarning >> messageText [

	^ messageText ifNil: [
		'The code being simulated is trying to control a process ({1}). {2}' translated format: {
			self context method asMethodReference.
			self isSimulationGuard
				ifTrue: ['If you proceed, your image may be locked. Continue at own risk, and better save your image before.' translated]
				ifFalse: ['Process controlling cannot be simulated. If you proceed, side effects may occur outside the observable area of the simulator.' translated]}]
]

{ #category : 'accessing' }
SimulationSideEffectWarning >> method [

	^ method
]

{ #category : 'accessing' }
SimulationSideEffectWarning >> primitive [

	^ primitiveIndex
]

{ #category : 'initialize-release' }
SimulationSideEffectWarning >> primitive: anInteger [

	primitiveIndex := anInteger.
]

{ #category : 'signaling' }
SimulationSideEffectWarning >> signalIfSkipped: skipBlock [

	^ self signal ifFalse: skipBlock
]

{ #category : 'handling' }
SimulationSideEffectWarning >> skipPrimitive [

	^ self resume: false
]

{ #category : 'accessing' }
SimulationSideEffectWarning >> suppress [

	suppressed := true.
]

{ #category : 'accessing' }
SimulationSideEffectWarning >> suppressed [

	^ suppressed ifNil: [self isSimulationGuard not]
]

{ #category : 'accessing' }
SimulationSideEffectWarning >> theReceiver [

	^ receiver
]

{ #category : 'accessing' }
SimulationSideEffectWarning >> unsuppress [

	suppressed := false.
]
