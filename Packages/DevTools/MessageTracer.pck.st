'From Cuis 6.0 [latest update: #5552] on 5 November 2022 at 8:43:24 pm'!
'Description Utility library for tracing the sending of messages.'!
!provides: 'MessageTracer' 1 35!
SystemOrganization addCategory: 'MessageTracer-Tests'!
SystemOrganization addCategory: 'MessageTracer'!


!classDefinition: #MessageTracerWindow category: 'MessageTracer'!
SystemWindow subclass: #MessageTracerWindow
	instanceVariableNames: 'messageListIndex tracedMessage'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MessageTracer'!
!classDefinition: 'MessageTracerWindow class' category: 'MessageTracer'!
MessageTracerWindow class
	instanceVariableNames: ''!

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


!MessageTracerWindow commentStamp: '<historical>' prior: 0!
A morphic tool for managing traced messages.!

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

!MessageTracerWindow methodsFor: 'actions' stamp: 'MM 11/5/2022 20:20:08'!
browseSelectedMessage

	tracedMessage ifNotNil: [
	.	BrowserWindow fullOnClass: tracedMessage compiledMethod methodClass 
						selector: tracedMessage compiledMethod selector]! !

!MessageTracerWindow methodsFor: 'actions' stamp: 'MM 11/5/2022 20:25:25'!
untraceAll
	model untraceAll.
	self changed: #messageList! !

!MessageTracerWindow methodsFor: 'actions' stamp: 'MM 11/5/2022 20:27:32'!
untraceSelectedMessage

	model untrace: 	tracedMessage.
	self changed: #messageList! !

!MessageTracerWindow methodsFor: 'initialization' stamp: 'MM 11/5/2022 18:51:06'!
initialize
	super initialize.
	
	messageListIndex _ 0.! !

!MessageTracerWindow methodsFor: 'GUI building' stamp: 'MM 11/5/2022 20:43:11'!
buildMorphicWindow

	| messageList actionsRow |
	
	messageList _ PluggableListMorph
					model: self
					listGetter: #messageList
					indexGetter: #messageListIndex
					indexSetter: #messageListIndex:.
					
	layoutMorph addMorphUseAll: messageList.
	
	actionsRow _ LayoutMorph newRow.
	
	actionsRow addMorph: (PluggableButtonMorph model: self action: #browseSelectedMessage label: 'Browse').
	actionsRow addMorph: (PluggableButtonMorph model: self action: #untraceSelectedMessage label: 'Untrace').
	actionsRow addMorph: (PluggableButtonMorph model: self action: #untraceAll label: 'Untrace all').
	layoutMorph addMorph: actionsRow fixedHeight: 30.! !

!MessageTracerWindow methodsFor: 'GUI building' stamp: 'MM 11/5/2022 18:59:20'!
initialExtent

	^ 400@300! !

!MessageTracerWindow methodsFor: 'GUI building' stamp: 'MM 11/5/2022 20:41:23'!
update: anObject

	self changed: #messageList! !

!MessageTracerWindow methodsFor: 'accessing' stamp: 'MM 11/5/2022 19:03:32'!
messageList
	^ model tracing collect: [:traceWrapper | 
		traceWrapper compiledMethod methodClass name asString , '>>', traceWrapper selector asString]! !

!MessageTracerWindow methodsFor: 'accessing' stamp: 'MM 11/5/2022 18:51:14'!
messageListIndex
	"Answer the value of messageListIndex"

	^ messageListIndex! !

!MessageTracerWindow methodsFor: 'accessing' stamp: 'MM 11/5/2022 20:16:34'!
messageListIndex: anObject
	"Set the value of messageListIndex"

	messageListIndex := anObject.
	tracedMessage _ nil.
	
	anObject ifNotNil: [
		anObject isZero ifFalse: [ 
			tracedMessage _ model tracing at: anObject]]
			! !

!MessageTracerWindow class methodsFor: 'as yet unclassified' stamp: 'MM 11/5/2022 18:56:30'!
open
	^ self open: MessageTracer singleton label: 'Tracing'! !

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

!MessageTracer methodsFor: 'api' stamp: 'MM 11/5/2022 20:32:36'!
toggleTrace: aCompiledMethod

	(aCompiledMethod isKindOf: TracedMethodWrapper)
		ifTrue: [self untrace: aCompiledMethod]
		ifFalse: [self trace: aCompiledMethod]! !

!MessageTracer methodsFor: 'api' stamp: 'MM 11/5/2022 20:40:43'!
trace: aCompiledMethod

	|methodWrapper  |
	
	"If breakpoint already installed, setup and enable."
	aCompiledMethod class isCompiledMethodClass ifFalse: [
		(aCompiledMethod isKindOf: TracedMethodWrapper)
			ifTrue: [
				aCompiledMethod when: #around; enable.
				^ self]].
		
	"Install a new trace wrapper"
	methodWrapper _ TracedMethodWrapper on: aCompiledMethod methodClass selector: aCompiledMethod selector.
	methodWrapper		install.
	tracing add: methodWrapper.
	self changed.! !

!MessageTracer methodsFor: 'api' stamp: 'MM 11/5/2022 18:12:34'!
trace: aClass selector: selector

	|compiledMethod |
	
	compiledMethod _ aClass compiledMethodAt: selector.
	
	^ self trace: compiledMethod! !

!MessageTracer methodsFor: 'api' stamp: 'MM 11/5/2022 20:40:54'!
untrace: aCompiledMethod

	aCompiledMethod class isCompiledMethodClass ifFalse: [
		(aCompiledMethod isKindOf: TracedMethodWrapper)
			ifTrue: [
				aCompiledMethod uninstall.
				tracing remove: aCompiledMethod.
				self changed.
				^ self]]! !

!MessageTracer methodsFor: 'api' stamp: 'MM 11/5/2022 20:21:48'!
untrace: aClass selector: selector

	|compiledMethod |
	
	compiledMethod _ aClass compiledMethodAt: selector.
	
	^ self untrace: compiledMethod! !

!MessageTracer methodsFor: 'api' stamp: 'MM 11/5/2022 20:42:06'!
untraceAll
	tracing do: [:tracedMethod |
		tracedMethod uninstall].
	tracing _ OrderedCollection new.
	self changed.! !

!MessageTracer methodsFor: 'initialization' stamp: 'MM 6/17/2022 12:54:23'!
initialize
	tracing _ OrderedCollection new.
	outputStream _ Transcript! !

!MessageTracer methodsFor: 'as yet unclassified' stamp: 'MM 11/5/2022 18:21:00'!
tracing
	^ tracing! !

!MessageTracer class methodsFor: 'as yet unclassified' stamp: 'MM 11/5/2022 18:41:23'!
messageListMenuOptions
	
	^`{
			{
				#itemGroup 			-> 		25.
				#itemOrder 			-> 		30.
				#label 			-> 		'toggle tracing'.
				#object -> #model.
				#selector 			-> 		#toggleMessageTracing.
				#icon 			-> 		#debugIcon
			} asDictionary.
			
			{
				#itemGroup -> 25.
				#itemOrder -> 30.
				#label -> 'show traced'.
				#object -> #model.
				#selector -> #showTracedMessages.
				#icon -> #displayIcon.
			} asDictionary.
			
			{
				#itemGroup -> 25.
				#itemOrder -> 30.
				#object -> MessageTracer.
				#label -> 'untrace all'.
				#selector -> #untraceAll.
				#icon -> #debugIcon.
			} asDictionary.
		}`! !

!MessageTracer class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 12:35:28'!
outputStream
	^ self singleton outputStream! !

!MessageTracer class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 12:35:13'!
singleton
	^ singleton ifNil: [singleton _ self new]! !

!MessageTracer class methodsFor: 'api' stamp: 'MM 11/5/2022 20:33:11'!
toggleTrace: aCompiledMethod

	self singleton toggleTrace: aCompiledMethod! !

!MessageTracer class methodsFor: 'api' stamp: 'MM 11/5/2022 18:25:04'!
trace: aCompiledMethod

	self singleton trace: aCompiledMethod! !

!MessageTracer class methodsFor: 'api' stamp: 'MM 6/17/2022 12:53:20'!
trace: aClass selector: aSelector

	self singleton trace: aClass selector: aSelector! !

!MessageTracer class methodsFor: 'api' stamp: 'MM 11/5/2022 18:21:23'!
tracing

	^ self singleton tracing! !

!MessageTracer class methodsFor: 'api' stamp: 'MM 11/5/2022 20:22:24'!
untrace: aCompiledMethod

	self singleton untrace: aCompiledMethod! !

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

!TracedMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 11/5/2022 19:02:39'!
compiledMethod

	^ compiledMethod! !

!TracedMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 12:59:50'!
disable
	enabled _ false! !

!TracedMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 13:02:08'!
when: aSymbol

	when _ aSymbol! !

!TracedMethodWrapper class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 12:03:49'!
on: aClass selector: selector
	^ self new initialize: aClass selector: selector! !

!Browser methodsFor: '*MessageTracer' stamp: 'MM 11/5/2022 20:14:34'!
showTracedMessages

	MessageTracerWindow open! !

!Browser methodsFor: '*MessageTracer' stamp: 'MM 11/5/2022 20:32:53'!
toggleMessageTracing

	"Start or stop tracing current selected method"

	| selectedMethod |
	
	self selectedClassOrMetaClass ifNil: [ ^self].
	
	selectedMethod := self selectedClassOrMetaClass >> self selectedMessageName.
	MessageTracer toggleTrace: selectedMethod! !

!Behavior methodsFor: '*MessageTracer' stamp: 'MM 6/17/2022 13:35:02'!
trace: aSelector

	MessageTracer trace: self selector: aSelector! !

!Behavior methodsFor: '*MessageTracer' stamp: 'MM 6/17/2022 13:40:56'!
untrace: aSelector

	MessageTracer untrace: self selector: aSelector! !
