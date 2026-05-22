Class {
	#name : 'QuickReturnDebuggingTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Tools-Debugger',
	#package : 'BaseImageTests',
	#tag : 'Tools-Debugger'
}

{ #category : 'private' }
QuickReturnDebuggingTest >> debuggerOn: aBlock [
	| debugger |
	
	debugger := Debugger newDebugging: aBlock.
	
	debugger expandStack.
	
	[debugger interruptedContext method == aBlock outerContext method ]
		whileFalse: [debugger send].
		
	^ debugger
]

{ #category : 'private' }
QuickReturnDebuggingTest >> quickReturn [
	" Example:
		QuickReturnDebuggingTest new halt quickReturn
	"
	
	^ 2
]

{ #category : 'tests' }
QuickReturnDebuggingTest >> test01_debuggerCanStepIntoQuickReturnMethods [
	| aDebugger |
	aDebugger := self debuggerOn: [		self quickReturn 	].
	
	aDebugger send.
	
	self assert: aDebugger interruptedContext method equals: self class >> #quickReturn.
]

{ #category : 'tests' }
QuickReturnDebuggingTest >> test02_debuggerCanProceedFromAQuickReturnMethod [
	| aDebugger result |
	aDebugger := self debuggerOn: [		
		result := self quickReturn 
	].
	aDebugger send.
	
	aDebugger proceed.
	
	self assert: result equals: self quickReturn.
]

{ #category : 'tests' }
QuickReturnDebuggingTest >> test03_debuggerCanStepThroughFromAQuickReturnMethod [
	| aDebugger result |
	aDebugger := self debuggerOn: [		
		result := self quickReturn 
	].
	aDebugger send.
	
	aDebugger stepIntoBlock.
	
	self assert: aDebugger interruptedContext method equals: thisContext method.
	aDebugger proceed.
	self assert: result equals: self quickReturn.
]

{ #category : 'tests' }
QuickReturnDebuggingTest >> test04_debuggerCanStepFromAQuickReturnMethod [
	| aDebugger result |
	aDebugger := self debuggerOn: [		
		result := self quickReturn 
	].
	aDebugger send.
	
	aDebugger doStep.
	
	self assert: aDebugger interruptedContext method equals: thisContext method.
	aDebugger proceed.
	self assert: result equals: self quickReturn.
]

{ #category : 'tests' }
QuickReturnDebuggingTest >> test05_debuggerCanSendFromAQuickReturnMethod [
	| aDebugger result |
	aDebugger := self debuggerOn: [		
		result := self quickReturn 
	].
	aDebugger send.
	
	aDebugger send.
	
	self assert: aDebugger interruptedContext method equals: thisContext method.
	aDebugger proceed.
	self assert: result equals: self quickReturn.
]

{ #category : 'tests' }
QuickReturnDebuggingTest >> test06_debuggerCanRestartFromAQuickReturnMethod [
	| aDebugger result |
	aDebugger := self debuggerOn: [		
		result := self quickReturn 
	].
	aDebugger send.
	
	aDebugger restart.
	
	self assert: aDebugger interruptedContext method equals: self class >> #quickReturn.
	aDebugger proceed.
	self assert: result equals: self quickReturn.
]
