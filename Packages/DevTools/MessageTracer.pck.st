'From Cuis 6.0 [latest update: #5399] on 29 July 2022 at 1:42:26 pm'!
'Description Utility library for tracing the sending of messages.'!
!provides: 'MessageTracer' 1 22!
SystemOrganization addCategory: 'MessageTracer-Tests'!
SystemOrganization addCategory: 'MessageTracer'!


!classDefinition: #MessageTracerTestSubject category: 'MessageTracer-Tests'!
Object subclass: #MessageTracerTestSubject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageTracer-Tests'!
!classDefinition: 'MessageTracerTestSubject class' category: 'MessageTracer-Tests'!
MessageTracerTestSubject class
	instanceVariableNames: ''!

!classDefinition: #MessageTracer category: 'MessageTracer'!
Object subclass: #MessageTracer
	instanceVariableNames: 'outputStream tracing'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageTracer'!
!classDefinition: 'MessageTracer class' category: 'MessageTracer'!
MessageTracer class
	instanceVariableNames: 'singleton'!

!classDefinition: #TracedMethodWrapper category: 'MessageTracer'!
Object subclass: #TracedMethodWrapper
	instanceVariableNames: 'compiledMethod methodClass enabled when'
	classVariableNames: 'InvocationLevel'
	poolDictionaries: ''
	category: 'MessageTracer'!
!classDefinition: 'TracedMethodWrapper class' category: 'MessageTracer'!
TracedMethodWrapper class
	instanceVariableNames: ''!


!MessageTracer commentStamp: 'MM 7/29/2022 12:51:13' prior: 0!
I provide an api for tracing the sending of messages.

The message traces are sent to the Transcript.

For example, open a Transcript and evaluate:

subject _ MessageTracerTestSubject new.

subject foo.

MessageTracer trace: MessageTracerTestSubject selector: #foo.

subject foo.

MessageTracer untrace: MessageTracerTestSubject selector: #foo.

MessageTracerTestSubject trace: #add:to:.

subject add: 4 to: 5.

MessageTracerTestSubject trace: #fact:.

subject fact: 5.

MessageTracerTestSubject trace: #fib:.

subject fib: 10.

MessageTracerTestSubject untrace: #fib:.!

!MessageTracerTestSubject methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 13:33:48'!
add: x to: y

	^ x + y! !

!MessageTracerTestSubject methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 12:51:53'!
bar

	^ 'bar'! !

!MessageTracerTestSubject methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 13:36:43'!
fact: x

	^ x = 1 ifTrue: [x]
		ifFalse: [x * (self fact: x - 1)]! !

!MessageTracerTestSubject methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 13:39:28'!
fib: x

	^ x <= 1 ifTrue: [x]
		ifFalse: [(self fib: x - 1) + (self fib: x - 2)]! !

!MessageTracerTestSubject methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 12:51:47'!
foo
	^ 'foo'! !

!MessageTracer methodsFor: 'accessing' stamp: 'MM 6/17/2022 12:35:45'!
outputStream
	"Answer the value of outputStream"

	^ outputStream! !

!MessageTracer methodsFor: 'accessing' stamp: 'MM 6/17/2022 12:35:45'!
outputStream: anObject
	"Set the value of outputStream"

	outputStream := anObject! !

!MessageTracer methodsFor: 'api' stamp: 'MM 6/17/2022 13:01:41'!
trace: aClass selector: selector

	|methodWrapper compiledMethod |
	
	compiledMethod _ aClass compiledMethodAt: selector.
	
	"If breakpoint already installed, setup and enable."
	compiledMethod class isCompiledMethodClass ifFalse: [
		(compiledMethod isKindOf: TracedMethodWrapper)
			ifTrue: [
				compiledMethod when: #around; enable.
				^ self]].
		
	"Install a new trace wrapper"
	methodWrapper _ TracedMethodWrapper on: aClass selector: selector.
	methodWrapper		install.
	tracing add: methodWrapper! !

!MessageTracer methodsFor: 'api' stamp: 'MM 6/17/2022 13:03:39'!
untrace: aClass selector: selector

	|compiledMethod |
	
	compiledMethod _ aClass compiledMethodAt: selector.
	
	compiledMethod class isCompiledMethodClass ifFalse: [
		(compiledMethod isKindOf: TracedMethodWrapper)
			ifTrue: [
				compiledMethod uninstall.
				tracing remove: compiledMethod.
				^ self]]! !

!MessageTracer methodsFor: 'api' stamp: 'MM 6/17/2022 13:45:08'!
untraceAll
	tracing do: [:tracedMethod |
		tracedMethod uninstall].
	tracing _ OrderedCollection new! !

!MessageTracer methodsFor: 'initialization' stamp: 'MM 6/17/2022 12:54:23'!
initialize
	tracing _ OrderedCollection new.
	outputStream _ Transcript! !

!MessageTracer class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 12:35:28'!
outputStream
	^ self singleton outputStream! !

!MessageTracer class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 12:35:13'!
singleton
	^ singleton ifNil: [singleton _ self new]! !

!MessageTracer class methodsFor: 'api' stamp: 'MM 6/17/2022 12:53:20'!
trace: aClass selector: aSelector

	self singleton trace: aClass selector: aSelector! !

!MessageTracer class methodsFor: 'api' stamp: 'MM 6/17/2022 13:02:37'!
untrace: aClass selector: aSelector

	self singleton untrace: aClass selector: aSelector! !

!MessageTracer class methodsFor: 'api' stamp: 'MM 6/17/2022 13:44:35'!
untraceAll
	self singleton untraceAll! !

!TracedMethodWrapper methodsFor: 'initialization' stamp: 'MM 6/17/2022 12:30:37'!
initialize: aClass selector: aSelector

	methodClass _ aClass.
	compiledMethod _ aClass compiledMethodAt: aSelector.
	
	enabled _ true.
	when _ #around
	! !

!TracedMethodWrapper methodsFor: 'installation' stamp: 'MM 6/17/2022 12:59:34'!
enable
	enabled _ true! !

!TracedMethodWrapper methodsFor: 'installation' stamp: 'MM 6/17/2022 12:58:58'!
install
	
	methodClass
		addSelectorSilently: compiledMethod selector
		withMethod: self! !

!TracedMethodWrapper methodsFor: 'installation' stamp: 'MM 6/17/2022 12:59:25'!
toggle
	enabled _ enabled not! !

!TracedMethodWrapper methodsFor: 'installation' stamp: 'MM 6/17/2022 12:59:15'!
uninstall
	
	methodClass
		addSelectorSilently: compiledMethod selector
		withMethod: compiledMethod! !

!TracedMethodWrapper methodsFor: 'testing' stamp: 'MM 6/17/2022 12:59:42'!
enabled
	^ enabled! !

!TracedMethodWrapper methodsFor: 'implementation' stamp: 'MM 7/29/2022 12:57:46'!
decreaseInvocationLevel
	InvocationLevel _ InvocationLevel - 1.! !

!TracedMethodWrapper methodsFor: 'implementation' stamp: 'MM 6/17/2022 12:03:30'!
doesNotUnderstand: aMessage
		
	^ aMessage sendTo: compiledMethod! !

!TracedMethodWrapper methodsFor: 'implementation' stamp: 'MM 7/29/2022 13:11:57'!
increaseInvocationLevel
	InvocationLevel _ self invocationLevel + 1.! !

!TracedMethodWrapper methodsFor: 'implementation' stamp: 'MM 7/29/2022 13:06:06'!
invocationLevel
	^ InvocationLevel ifNil: [1]! !

!TracedMethodWrapper methodsFor: 'implementation' stamp: 'MM 7/29/2022 13:01:01'!
outputMethodEntry: aReceiver arguments: arguments
	
	|stream|
	
	stream _ MessageTracer outputStream.
	self printIndentation: stream.
	stream nextPutAll: 'Calling ';
			nextPutAll: aReceiver printString;
			nextPutAll: '>>';
			nextPutAll: compiledMethod selector printString;
			newLine.
	self printIndentation: stream.
	stream nextPutAll: '	      arguments: ';			nextPutAll: arguments printString;
			newLine.
		
			! !

!TracedMethodWrapper methodsFor: 'implementation' stamp: 'MM 7/29/2022 13:03:02'!
outputMethodExit: aReceiver result: result

	|stream|
	
	stream _ MessageTracer outputStream.
	self printIndentation: stream.
	stream nextPutAll: 'Returning from ';
			nextPutAll: aReceiver printString;
			nextPutAll: '>>';
			nextPutAll: compiledMethod selector;
			newLine.
	self printIndentation: stream.
	stream    	nextPutAll: '        returned: ';
			nextPutAll: result printString;
			newLine
			! !

!TracedMethodWrapper methodsFor: 'implementation' stamp: 'MM 7/29/2022 13:15:36'!
printIndentation: aStream
	(self invocationLevel - 1) timesRepeat: [
		aStream nextPutAll: '  |'].! !

!TracedMethodWrapper methodsFor: 'implementation' stamp: 'MM 7/29/2022 13:41:35'!
run: aSelector with: arguments in: aReceiver 

	"Main entry point"
	
	| result |
	
	enabled ifFalse: [^ compiledMethod valueWithReceiver: aReceiver arguments: arguments].
	
	(#(onEntry around) includes: when) ifTrue: [
		self outputMethodEntry: aReceiver arguments: arguments].
	
	self increaseInvocationLevel .
	
	[result _ compiledMethod			valueWithReceiver: aReceiver				arguments: arguments] ensure:
		[self decreaseInvocationLevel].
	
	(#(onExit around) includes: when) ifTrue: [
		self outputMethodExit: aReceiver result: result].
	
	^ result! !

!TracedMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 12:59:50'!
disable
	enabled _ false! !

!TracedMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 13:02:08'!
when: aSymbol

	when _ aSymbol! !

!TracedMethodWrapper class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 12:03:49'!
on: aClass selector: selector
	^ self new initialize: aClass selector: selector! !

!Behavior methodsFor: '*MessageTracer' stamp: 'MM 6/17/2022 13:35:02'!
trace: aSelector

	MessageTracer trace: self selector: aSelector! !

!Behavior methodsFor: '*MessageTracer' stamp: 'MM 6/17/2022 13:40:56'!
untrace: aSelector

	MessageTracer untrace: self selector: aSelector! !
