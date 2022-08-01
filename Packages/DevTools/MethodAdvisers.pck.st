'From Cuis 6.0 [latest update: #5399] on 25 July 2022 at 11:16:33 pm'!
'Description '!
!provides: 'MethodAdvisers' 1 26!
SystemOrganization addCategory: 'MethodAdvisers-Tests'!
SystemOrganization addCategory: 'MethodAdvisers'!


!classDefinition: #MethodAdvisersTests category: 'MethodAdvisers-Tests'!
TestCase subclass: #MethodAdvisersTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MethodAdvisers-Tests'!
!classDefinition: 'MethodAdvisersTests class' category: 'MethodAdvisers-Tests'!
MethodAdvisersTests class
	instanceVariableNames: ''!

!classDefinition: #MethodAdviserTestSubject category: 'MethodAdvisers-Tests'!
Object subclass: #MethodAdviserTestSubject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MethodAdvisers-Tests'!
!classDefinition: 'MethodAdviserTestSubject class' category: 'MethodAdvisers-Tests'!
MethodAdviserTestSubject class
	instanceVariableNames: ''!

!classDefinition: #AdvisedMethod category: 'MethodAdvisers'!
Object subclass: #AdvisedMethod
	instanceVariableNames: 'before after around compiledMethod methodClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'MethodAdvisers'!
!classDefinition: 'AdvisedMethod class' category: 'MethodAdvisers'!
AdvisedMethod class
	instanceVariableNames: ''!


!AdvisedMethod commentStamp: '<historical>' prior: 0!
I use the Objects as Method Wrappers facility provided by the VM to wrap compiled methods.
My entry point is my #run:with:in: method, which will be executed by the VM when I install myself in the target class's method dictionary.

Examples:

MethodAdviserTestSubject after: #test1 do: [:receiver | Transcript show: 'After test1'].

MethodAdviserTestSubject new test1.

MethodAdviserTestSubject removeAllAdvice .

MethodAdviserTestSubject new test1.

MethodAdviserTestSubject after: #test1 do: [:receiver | Transcript show: 'After test1'].
MethodAdviserTestSubject after: #test1 do: [:receiver | Transcript show: 'After test1 again'].
MethodAdviserTestSubject new test1.

MethodAdviserTestSubject removeAllAdvice .

MethodAdviserTestSubject before: #test1 do: [:receiver | Transcript show: 'Before test1'].
MethodAdviserTestSubject after: #test1 do: [:receiver | Transcript show: 'After test1'].
MethodAdviserTestSubject new test1.

MethodAdviserTestSubject removeAllAdvice .
MethodAdviserTestSubject around: #test1 do: [:receiver :nextMethod |
	Transcript show: 'Start around'.
	nextMethod value.
	Transcript show: 'End around'.
	'wrapped test1'].
MethodAdviserTestSubject new test1!

!MethodAdvisersTests methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 23:09:23'!
tearDown

	MethodAdviserTestSubject removeAdviceFrom: #test1:.  ! !

!MethodAdvisersTests methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 23:09:58'!
testAfter

	| output result |
	
	output _ String streamContents: [:s |
		result _ MethodAdviserTestSubject new test1: s].
	
	self assert: output = 'test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject after:  #test1: do: [:receiver :aStream |
			aStream nextPutAll: 'after.'.
			#after].
		
	output  _ String streamContents: [:s |		 result _ MethodAdviserTestSubject new test1: s].
	 
	self assert: output = 'test1.after.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject removeAdviceFrom: #test1:.  ! !

!MethodAdvisersTests methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 23:13:12'!
testAfterTwice

	| output result |
	
	output _ String streamContents: [:s |
		result _ MethodAdviserTestSubject new test1: s].
	
	self assert: output = 'test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject after:  #test1: do: [:receiver :aStream |
			aStream nextPutAll: 'after1.'.
			#after1].
		
	MethodAdviserTestSubject after:  #test1: do: [:receiver :aStream |
			aStream nextPutAll: 'after2.'.
			#after2].
		
	output  _ String streamContents: [:s |		 result _ MethodAdviserTestSubject new test1: s].
	 
	self assert: output = 'test1.after1.after2.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject removeAdviceFrom: #test1:.  ! !

!MethodAdvisersTests methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 23:03:56'!
testAll

	| output result |
	
	output _ String streamContents: [:s |
		result _ MethodAdviserTestSubject new test1: s].
	
	self assert: output = 'test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject around:  #test1: do: [:receiver :nextMethod :aStream |
			aStream nextPutAll: 'before around.'.
			nextMethod value.
			aStream nextPutAll: 'after around.'.
			#testAround].	
		
	MethodAdviserTestSubject before: #test1: do: [:receiver :aStream |
		aStream nextPutAll: 'before.'.
		#before].
	
	MethodAdviserTestSubject after: #test1: do: [:receiver :aStream |
		aStream nextPutAll: 'after.'.
		#after].

	output  _ String streamContents: [:s |		
		result _ MethodAdviserTestSubject new test1: s].
	 
	self assert: output = 'before.before around.test1.after around.after.'.
	self assert: result = #testAround.
	
	MethodAdviserTestSubject removeAdviceFrom: #test1:.! !

!MethodAdvisersTests methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 23:16:23'!
testAllMany

	| output result |
	
	output _ String streamContents: [:s |
		result _ MethodAdviserTestSubject new test1: s].
	
	self assert: output = 'test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject around:  #test1: do: [:receiver :nextMethod :aStream |
			aStream nextPutAll: 'before around1.'.
			nextMethod value.
			aStream nextPutAll: 'after around1.'.
			#testAround1].	
		
	MethodAdviserTestSubject around:  #test1: do: [:receiver :nextMethod :aStream |
			aStream nextPutAll: 'before around2.'.
			nextMethod value.
			aStream nextPutAll: 'after around2.'.
			#testAround2].	
		
	MethodAdviserTestSubject before: #test1: do: [:receiver :aStream |
		aStream nextPutAll: 'before1.'.
		#before1].
	
	MethodAdviserTestSubject before: #test1: do: [:receiver :aStream |
		aStream nextPutAll: 'before2.'.
		#before2].
	
	MethodAdviserTestSubject after: #test1: do: [:receiver :aStream |
		aStream nextPutAll: 'after1.'.
		#after1].
	
	MethodAdviserTestSubject after: #test1: do: [:receiver :aStream |
		aStream nextPutAll: 'after2.'.
		#after2].

	output  _ String streamContents: [:s |		
		result _ MethodAdviserTestSubject new test1: s].
	 
	self assert: output = 'before2.before1.before around2.before around1.test1.after around1.after around2.after1.after2.'.
	self assert: result = #testAround2.
	
	MethodAdviserTestSubject removeAdviceFrom: #test1:.! !

!MethodAdvisersTests methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 23:04:14'!
testAround

	| output result |
	
	output _ String streamContents: [:s |
		result _ MethodAdviserTestSubject new test1: s].
	
	self assert: output = 'test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject around:  #test1: do: [:receiver :nextMethod :aStream |
			aStream nextPutAll: 'before around.'.
			nextMethod value.
			aStream nextPutAll: 'after around.'.
			#testAround].	
		
	output  _ String streamContents: [:s |		
	result _ MethodAdviserTestSubject new test1: s].
	 
	self assert: output = 'before around.test1.after around.'.
	self assert: result = #testAround.
	
	MethodAdviserTestSubject removeAdviceFrom: #test1:.! !

!MethodAdvisersTests methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 23:10:42'!
testAroundTwice

	| output result |
	
	output _ String streamContents: [:s |
		result _ MethodAdviserTestSubject new test1: s].
	
	self assert: output = 'test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject around:  #test1: do: [:receiver :nextMethod :aStream |
			aStream nextPutAll: 'before around.'.
			nextMethod value.
			aStream nextPutAll: 'after around.'.
			#testAround].	
		
	MethodAdviserTestSubject around:  #test1: do: [:receiver :nextMethod :aStream |
			aStream nextPutAll: 'before around around.'.
			nextMethod value.
			aStream nextPutAll: 'after around around.'.
			#testAroundAround].	
		
	output  _ String streamContents: [:s |		
	result _ MethodAdviserTestSubject new test1: s].
	 
	self assert: output = 'before around around.before around.test1.after around.after around around.'.
	self assert: result = #testAroundAround.
	
	MethodAdviserTestSubject removeAdviceFrom: #test1:.! !

!MethodAdvisersTests methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 23:04:44'!
testBefore

	| output result |
	
	output _ String streamContents: [:s |
		result _ MethodAdviserTestSubject new test1: s].
	
	self assert: output = 'test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject before:  #test1: do: [:receiver :aStream |
			aStream nextPutAll: 'before.'.
			#before].
		
	output  _ String streamContents: [:s |		 result _ MethodAdviserTestSubject new test1: s].
	 
	self assert: output = 'before.test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject removeAdviceFrom: #test1:.! !

!MethodAdvisersTests methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 23:10:57'!
testBeforeAndAfter

	| output result |
	
	output _ String streamContents: [:s |
		result _ MethodAdviserTestSubject new test1: s].
	
	self assert: output = 'test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject before:  #test1: do: [:receiver :aStream |
			aStream nextPutAll: 'before.'.
			#after].
	
	MethodAdviserTestSubject after:  #test1: do: [:receiver :aStream |
			aStream nextPutAll: 'after.'.
			#after].
		
	output  _ String streamContents: [:s |		 result _ MethodAdviserTestSubject new test1: s].
	 
	self assert: output = 'before.test1.after.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject removeAdviceFrom: #test1:.! !

!MethodAdvisersTests methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 23:12:33'!
testBeforeTwice

	| output result |
	
	output _ String streamContents: [:s |
		result _ MethodAdviserTestSubject new test1: s].
	
	self assert: output = 'test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject before:  #test1: do: [:receiver :aStream |
			aStream nextPutAll: 'before1.'.
			#before1].
		
	MethodAdviserTestSubject before:  #test1: do: [:receiver :aStream |
			aStream nextPutAll: 'before2.'.
			#before2].
		
	output  _ String streamContents: [:s |		 result _ MethodAdviserTestSubject new test1: s].
	 
	self assert: output = 'before2.before1.test1.'.
	self assert: result = #test1.
	
	MethodAdviserTestSubject removeAdviceFrom: #test1:.! !

!MethodAdviserTestSubject methodsFor: 'as yet unclassified' stamp: 'MM 6/16/2022 21:13:04'!
test1
	Transcript show: 'test1'.
	^ 'test1'! !

!MethodAdviserTestSubject methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 22:42:36'!
test1: aStream
	aStream nextPutAll: 'test1.'.
	^ 'test1'! !

!AdvisedMethod methodsFor: 'as yet unclassified' stamp: 'MM 6/16/2022 20:33:32'!
afterDo: aBlock
	after addLast: aBlock! !

!AdvisedMethod methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 22:38:45'!
aroundDo: aBlock
	around addFirst: aBlock! !

!AdvisedMethod methodsFor: 'as yet unclassified' stamp: 'MM 6/16/2022 20:33:43'!
beforeDo: aBlock
	before addFirst: aBlock! !

!AdvisedMethod methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:15:09'!
doesNotUnderstand: aMessage

	"Any message I don't understand is forwarded to the compiledMethod."
		
	^ aMessage sendTo: compiledMethod! !

!AdvisedMethod methodsFor: 'as yet unclassified' stamp: 'MM 7/25/2022 10:10:36'!
evaluate: aroundList around: aBlock with: arguments in: aReceiver

	^ aroundList isEmpty 
		ifTrue: [	aBlock value]
		ifFalse: [	|args|
			args _ (Array with: aReceiver with: [self evaluate: aroundList allButFirst around: aBlock with: arguments in: aReceiver]), arguments.
			aroundList first valueWithArguments: args]! !

!AdvisedMethod methodsFor: 'as yet unclassified' stamp: 'MM 6/16/2022 20:31:46'!
initialize: aClass selector: aSelector

	methodClass _ aClass.
	compiledMethod _ aClass compiledMethodAt: aSelector.
	after _ OrderedCollection new.
	before _ OrderedCollection new.
	around _ OrderedCollection new.! !

!AdvisedMethod methodsFor: 'as yet unclassified' stamp: 'MM 6/16/2022 19:59:33'!
install
	
	methodClass
		addSelectorSilently: compiledMethod selector
		withMethod: self! !

!AdvisedMethod methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:19:08'!
run: aSelector with: arguments in: aReceiver 

	"Main entry point"
	
	| result |
	
	"Evaluate before blocks"
	
	before do: [:aBlock | aBlock valueWithArguments: ({aReceiver}, arguments)]. 
	
	result _ self evaluate: around 
				around: [	compiledMethod
							valueWithReceiver: aReceiver
							arguments: arguments]
				with: arguments
				in: aReceiver.
	
	after do: [:aBlock | aBlock valueWithArguments: ({aReceiver}, arguments)].
	
	^ result! !

!AdvisedMethod methodsFor: 'as yet unclassified' stamp: 'MM 6/16/2022 20:52:29'!
uninstall
	
	methodClass
		addSelectorSilently: compiledMethod selector
		withMethod: compiledMethod! !

!AdvisedMethod class methodsFor: 'as yet unclassified' stamp: 'MM 6/16/2022 20:26:48'!
ensureOn: aClass selector: aSelector

	| compiledMethod |
	
	compiledMethod _ aClass compiledMethodAt: aSelector.
	compiledMethod class isCompiledMethodClass
		ifTrue: ["The compiled method is unwrapped"
			^ (self on: aClass selector: aSelector)
				install;
				yourself]
		ifFalse: ["We assume this is an AdvicedMethod"
			^ compiledMethod]! !

!AdvisedMethod class methodsFor: 'as yet unclassified' stamp: 'MM 6/16/2022 19:52:21'!
on: aCompiledMethod

	self assert: aCompiledMethod class isCompiledMethodClass! !

!AdvisedMethod class methodsFor: 'as yet unclassified' stamp: 'MM 6/16/2022 20:00:46'!
on: aClass selector: aSelector

	^ self new initialize: aClass selector: aSelector! !

!AdvisedMethod class methodsFor: 'as yet unclassified' stamp: 'MM 6/16/2022 20:53:45'!
uninstallFrom: aClass selector: aSelector

	| compiledMethod |
	
	compiledMethod _ aClass compiledMethodAt: aSelector.
	compiledMethod class isCompiledMethodClass
		ifTrue: [self error: 'No AdvisedMethod installed.']
		ifFalse: [compiledMethod uninstall]! !

!Behavior methodsFor: '*MethodAdvisers' stamp: 'MM 7/25/2022 22:48:27'!
after: aSelector do: aBlock
	
	|methodAdviser|
	
	methodAdviser _ AdvisedMethod ensureOn: self selector: aSelector.
	
	methodAdviser afterDo: aBlock.
	
	^ methodAdviser! !

!Behavior methodsFor: '*MethodAdvisers' stamp: 'MM 7/25/2022 22:48:14'!
around: aSelector do: aBlock
	
	|methodAdviser|
	
	methodAdviser _ AdvisedMethod ensureOn: self selector: aSelector.
	
	methodAdviser aroundDo: aBlock.
	
	^ methodAdviser! !

!Behavior methodsFor: '*MethodAdvisers' stamp: 'MM 7/25/2022 22:48:36'!
before: aSelector do: aBlock
	
	|methodAdviser|
	
	methodAdviser _ AdvisedMethod ensureOn: self selector: aSelector.
	
	methodAdviser beforeDo: aBlock.
	
	^ methodAdviser! !

!Behavior methodsFor: '*MethodAdvisers' stamp: 'MM 6/16/2022 21:23:20'!
removeAdviceFrom: aSelector
	
	| compiledMethod |
	
	compiledMethod _ self compiledMethodAt: aSelector.
	(compiledMethod isKindOf: AdvisedMethod) 
		ifTrue: [compiledMethod uninstall]! !

!Behavior methodsFor: '*MethodAdvisers' stamp: 'MM 6/16/2022 21:04:48'!
removeAllAdvice

	methodDict values do: [:compiledMethod |
		compiledMethod class isCompiledMethodClass ifFalse: [
			(compiledMethod isKindOf: AdvisedMethod) ifTrue: [
				compiledMethod uninstall]]]! !
