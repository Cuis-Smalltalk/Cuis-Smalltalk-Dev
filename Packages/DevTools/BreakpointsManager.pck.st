'From Cuis 6.0 [latest update: #5399] on 30 July 2022 at 10:17:53 am'!
'Description Utility library for managing breakpoints via the installation of Method Wrappers.'!
!provides: 'BreakpointsManager' 1 19!
SystemOrganization addCategory: 'BreakpointsManager-Tests'!
SystemOrganization addCategory: 'BreakpointsManager'!


!classDefinition: #BreakpointsManagerTests category: 'BreakpointsManager-Tests'!
TestCase subclass: #BreakpointsManagerTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BreakpointsManager-Tests'!
!classDefinition: 'BreakpointsManagerTests class' category: 'BreakpointsManager-Tests'!
BreakpointsManagerTests class
	instanceVariableNames: ''!

!classDefinition: #BreakpointsManagerTestSubject category: 'BreakpointsManager-Tests'!
Object subclass: #BreakpointsManagerTestSubject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BreakpointsManager-Tests'!
!classDefinition: 'BreakpointsManagerTestSubject class' category: 'BreakpointsManager-Tests'!
BreakpointsManagerTestSubject class
	instanceVariableNames: ''!

!classDefinition: #BreakpointMethodWrapper category: 'BreakpointsManager'!
Object subclass: #BreakpointMethodWrapper
	instanceVariableNames: 'compiledMethod when enabled methodClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BreakpointsManager'!
!classDefinition: 'BreakpointMethodWrapper class' category: 'BreakpointsManager'!
BreakpointMethodWrapper class
	instanceVariableNames: ''!

!classDefinition: #BreakpointsManager category: 'BreakpointsManager'!
Object subclass: #BreakpointsManager
	instanceVariableNames: 'installedBreakpoints'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'BreakpointsManager'!
!classDefinition: 'BreakpointsManager class' category: 'BreakpointsManager'!
BreakpointsManager class
	instanceVariableNames: 'singleton'!


!BreakpointsManager commentStamp: '<historical>' prior: 0!
I provide an api for managing the installation of breakpoints.

Example:

|subject|

subject _ BreakpointsManagerTestSubject new.

subject foo.

BreakpointsManager setBreakOnEntry: BreakpointsManagerTestSubject selector: #foo. 

subject foo.

BreakpointsManagerTestSubject setBreakOnEntry: #foo.

BreakpointsManager disableAllBreakpoints.
BreakpointsManager enableAllBreakpoints.
BreakpointsManager toggleAllBreakpoints.

BreakpointsManager uninstallAll !

!BreakpointsManagerTests methodsFor: 'as yet unclassified' stamp: 'MM 7/30/2022 10:02:14'!
tearDown
	BreakpointsManager uninstallAll ! !

!BreakpointsManagerTests methodsFor: 'as yet unclassified' stamp: 'MM 7/30/2022 10:11:10'!
testBreakOnEntry

	|subject|
	
	subject _ BreakpointsManagerTestSubject new.
	
	subject foo.
	
	self assert: subject foo = 'foo'.
	
	BreakpointsManager setBreakOnEntry: BreakpointsManagerTestSubject selector: #foo.
	
	self  should: [subject foo] raise: Exception.
	
	BreakpointsManager disableBreakpointIn: BreakpointsManagerTestSubject selector: #foo.
	
	self assert: subject foo = 'foo'. 
	
	BreakpointsManager enableBreakpointIn: BreakpointsManagerTestSubject selector: #foo.
	
	self  should: [subject foo] raise: Exception.
	
	BreakpointsManager unsetBreakpoint: BreakpointsManagerTestSubject selector: #foo.
	
	self assert: subject foo = 'foo'.	 ! !

!BreakpointsManagerTests methodsFor: 'as yet unclassified' stamp: 'MM 7/30/2022 10:13:06'!
testBreakOnExit

	|subject|
	
	subject _ BreakpointsManagerTestSubject new.
	
	subject foo.
	
	self assert: subject foo = 'foo'.
	
	BreakpointsManager setBreakOnExit: BreakpointsManagerTestSubject selector: #foo.
	
	self  should: [subject foo] raise: Exception.
	
	BreakpointsManager disableBreakpointIn: BreakpointsManagerTestSubject selector: #foo.
	
	self assert: subject foo = 'foo'. 
	
	BreakpointsManager enableBreakpointIn: BreakpointsManagerTestSubject selector: #foo.
	
	self  should: [subject foo] raise: Exception.
	
	BreakpointsManager unsetBreakpoint: BreakpointsManagerTestSubject selector: #foo.
	
	self assert: subject foo = 'foo'.	 ! !

!BreakpointsManagerTests methodsFor: 'as yet unclassified' stamp: 'MM 7/30/2022 10:17:46'!
testSetManyBreaks

	|subject|
	
	subject _ BreakpointsManagerTestSubject new.
	
	BreakpointsManagerTestSubject setBreakOnEntry: #foo.
	BreakpointsManagerTestSubject setBreakOnEntry: #foo.
	BreakpointsManagerTestSubject setBreakOnEntry: #foo.
	
	self should: [subject foo] raise: Exception.
	
	BreakpointsManagerTestSubject unsetBreakAt: #foo.
	
	self assert: subject foo = 'foo'! !

!BreakpointsManagerTestSubject methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 10:09:29'!
bar: aString
	^ 'bar: ', aString! !

!BreakpointsManagerTestSubject methodsFor: 'as yet unclassified' stamp: 'MM 7/30/2022 10:04:10'!
foo
	Transcript show: 'foo'.
	^ 'foo'! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:29:08'!
breakOnEntry
	when _ #onEntry! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:30:52'!
breakOnExit
	when _ #onExit! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:31:12'!
disable
	enabled _ false! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:06:30'!
doesNotUnderstand: aMessage
		
	^ aMessage sendTo: compiledMethod! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:31:05'!
enable
	enabled _ true! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:31:20'!
enabled
	^ enabled! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:28:47'!
initialize: aClass selector: aSelector

	methodClass _ aClass.
	compiledMethod _ aClass compiledMethodAt: aSelector.
	
	enabled _ true.
	when _ #onEntry
	! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:47:28'!
install
	
	methodClass
		addSelectorSilently: compiledMethod selector
		withMethod: self! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 10:16:56'!
run: aSelector with: arguments in: aReceiver 

	"Main entry point"
	
	| result |
	
	enabled ifFalse: [^ compiledMethod valueWithReceiver: aReceiver arguments: arguments].
	
	when == #onEntry ifTrue: [self break].
	
	result _ compiledMethod			valueWithReceiver: aReceiver				arguments: arguments.
	
	when == #onExit ifTrue: [self break].
	
	^ result! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 10:06:35'!
toggle
	enabled _ enabled not! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:47:40'!
uninstall
	
	methodClass
		addSelectorSilently: compiledMethod selector
		withMethod: compiledMethod! !

!BreakpointMethodWrapper methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:31:27'!
when
	^ when! !

!BreakpointMethodWrapper class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:45:46'!
on: aClass selector: selector
	^ self new initialize: aClass selector: selector! !

!BreakpointsManager methodsFor: 'initialization' stamp: 'MM 6/17/2022 09:48:55'!
initialize
	installedBreakpoints _ OrderedCollection new! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 6/17/2022 10:05:59'!
disableAllBreakpoints

	installedBreakpoints do: [:bp | bp disable]! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 7/29/2022 13:55:30'!
disableBreakpointIn: aClass selector: selector

	|bp|
	
	bp _ self breakpointIn: aClass at: selector.
	bp disable! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 6/17/2022 10:06:05'!
enableAllBreakpoints

	installedBreakpoints do: [:bp | bp enable]! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 7/29/2022 13:55:46'!
enableBreakpointIn: aClass selector: selector

	|bp|
	
	bp _ self breakpointIn: aClass at: selector.
	bp enable! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 6/17/2022 09:54:11'!
installedBreakpoints

	^ installedBreakpoints! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 6/17/2022 09:51:04'!
setBreakOnEntry: aClass selector: selector

	| compiledMethod breakpointWrapper |
	
	compiledMethod _ aClass compiledMethodAt: selector.
	
	"If breakpoint already installed, setup and enable."
	compiledMethod class isCompiledMethodClass ifFalse: [
		(compiledMethod isKindOf: BreakpointMethodWrapper)
			ifTrue: [
				compiledMethod breakOnEntry; enable.
				^ self]].
		
	"Install a new breakpoint wrapper"
	breakpointWrapper _ BreakpointMethodWrapper on: aClass selector: selector.
	breakpointWrapper breakOnEntry;		install.
	installedBreakpoints add: breakpointWrapper
	
		
	 ! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 6/17/2022 09:53:27'!
setBreakOnExit: aClass selector: selector

	| compiledMethod breakpointWrapper |
	
	compiledMethod _ aClass compiledMethodAt: selector.
	
	"If breakpoint already installed, setup and enable."
	compiledMethod class isCompiledMethodClass ifFalse: [
		(compiledMethod isKindOf: BreakpointMethodWrapper)
			ifTrue: [
				compiledMethod breakOnExit; enable.
				^ self]].
		
	"Install a new breakpoint wrapper"
	breakpointWrapper _ BreakpointMethodWrapper on: aClass selector: selector.
	breakpointWrapper breakOnExit;		install.
	installedBreakpoints add: breakpointWrapper
	
		
	 ! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 6/17/2022 10:06:13'!
toggleAllBreakpoints

	installedBreakpoints do: [:bp | bp toggle]! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 7/29/2022 13:54:16'!
toggleBreakpointIn: aClass selector: selector

	|bp|
	
	bp _ self breakpointIn: aClass at: selector.
	bp toggle! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 6/17/2022 10:25:23'!
uninstallAll

	installedBreakpoints do: [:bp | bp uninstall].
	
	installedBreakpoints _ OrderedCollection new! !

!BreakpointsManager methodsFor: 'api' stamp: 'MM 7/29/2022 13:58:03'!
unsetBreakpoint: aClass selector: selector

	|breakpointWrapper|
	
	breakpointWrapper _ self breakpointIn: aClass at: selector.
	breakpointWrapper uninstall. 
	installedBreakpoints remove: breakpointWrapper.! !

!BreakpointsManager methodsFor: 'as yet unclassified' stamp: 'MM 7/29/2022 14:01:58'!
breakpointIn: aClass at: aSelector

	|breakpointMethod|
	
	breakpointMethod _ aClass methodDict at: aSelector.
	self assert: (breakpointMethod isKindOf: BreakpointMethodWrapper) description: 'No breakpoint installed'.
	
	^ breakpointMethod! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 10:05:13'!
disableAllBreakpoints
	self singleton disableAllBreakpoints! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 10:13:46'!
disableBreakpointIn: aClass selector: aSelector
	self singleton disableBreakpointIn: aClass selector: aSelector! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 10:05:28'!
enableAllBreakpoints
	self singleton enableAllBreakpoints! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 7/30/2022 10:09:16'!
enableBreakpointIn: aClass selector: aSelector
	self singleton enableBreakpointIn: aClass selector: aSelector! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:54:37'!
installedBreakpoints

	^ self singleton installedBreakpoints! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:38:41'!
setBreakOnEntry: aClass selector: selector

	self singleton setBreakOnEntry: aClass selector: selector! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:38:56'!
setBreakOnExit: aClass selector: selector

	self singleton setBreakOnExit: aClass selector: selector! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 09:04:55'!
singleton
	^ singleton ifNil: [singleton _ self new]! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 10:05:37'!
toggleAllBreakpoints
	self singleton toggleAllBreakpoints! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 6/17/2022 10:24:55'!
uninstallAll
	self singleton uninstallAll! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 7/29/2022 14:01:00'!
uninstallBreakpoint: aClass selector: selector

	self singleton unsetBreakpoint:  aClass selector: selector! !

!BreakpointsManager class methodsFor: 'as yet unclassified' stamp: 'MM 7/29/2022 14:01:21'!
unsetBreakpoint: aClass selector: selector

	self singleton unsetBreakpoint: aClass selector: selector! !

!Behavior methodsFor: '*BreakpointsManager' stamp: 'MM 6/17/2022 09:37:40'!
setBreakOnEntry: selector

	BreakpointsManager setBreakOnEntry: self selector: selector! !

!Behavior methodsFor: '*BreakpointsManager' stamp: 'MM 6/17/2022 09:37:54'!
setBreakOnExit: selector

	BreakpointsManager setBreakOnExit: self selector: selector! !

!Behavior methodsFor: '*BreakpointsManager' stamp: 'MM 7/30/2022 10:17:26'!
unsetBreakAt: aSelector

	BreakpointsManager unsetBreakpoint: self selector: aSelector ! !
