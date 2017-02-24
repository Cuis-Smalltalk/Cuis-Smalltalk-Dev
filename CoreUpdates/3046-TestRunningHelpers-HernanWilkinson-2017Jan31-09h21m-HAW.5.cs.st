'From Cuis 5.0 of 7 November 2016 [latest update: #3042] on 10 February 2017 at 5:39:28 pm'!
!classDefinition: #ProgessiveTestRunner category: #'Tools-Testing'!
Object subclass: #ProgessiveTestRunner
	instanceVariableNames: 'testSuite testsStream progressBar testResult testRunIncrement'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!

!Browser methodsFor: 'tdd' stamp: 'HAW 2/10/2017 17:32:22'!
debugMethodTest
	
	currentCompiledMethod isTestMethod ifTrue: [
		currentCompiledMethod methodClass debugAsFailure: currentCompiledMethod selector ]
! !

!Browser methodsFor: 'tdd' stamp: 'HAW 2/4/2017 20:33:00'!
runClassTests
	
	self selectedClassName ifNotNil: [ :aClassName | | selectedClass |
		selectedClass _ Smalltalk classNamed: aClassName.
		(ProgessiveTestRunner for: (TestSuite forClass: selectedClass)) value ]! !

!Browser methodsFor: 'tdd' stamp: 'HAW 2/7/2017 10:47:50'!
runMessageCategoryTests
	
	selectedMessageCategory ifNotNil: [ | selectedClass suite |
		selectedClass _ Smalltalk classNamed: selectedClassName.
		suite _ TestSuite forMessageCategoryNamed: selectedMessageCategory of: selectedClass categorizedWith: classOrganizer.
		(ProgessiveTestRunner for: suite) value ]
	
	! !

!Browser methodsFor: 'tdd' stamp: 'HAW 2/4/2017 21:51:50'!
runMethodTest
	
	| suite |
	
	suite _ TestSuite forCompiledMethod: currentCompiledMethod.
	(ProgessiveTestRunner for: suite) value 
	! !

!Browser methodsFor: 'tdd' stamp: 'HAW 2/1/2017 19:23:54'!
runSystemCategoryTests
	
	selectedSystemCategory ifNotNil: [ | suite |
		suite _ TestSuite forSystemCategoryNamed: selectedSystemCategory using: systemOrganizer.
		(ProgessiveTestRunner for: suite) value ]
	
	! !


!Behavior methodsFor: 'tdd' stamp: 'HAW 2/7/2017 11:55:50'!
testCaseClass

	self subclassResponsibility ! !

!Behavior methodsFor: 'tdd' stamp: 'HAW 2/7/2017 11:55:40'!
withTestCaseClassDo: aFoundTestCaseClassBlock ifNone: aNoneBlock 
		
	| potentialTestCaseClass |
	
	potentialTestCaseClass _ self testCaseClass.
	
	^potentialTestCaseClass ifNil: aNoneBlock ifNotNil: aFoundTestCaseClassBlock 
	! !


!Class methodsFor: 'tdd' stamp: 'HAW 2/7/2017 10:42:40'!
testCaseClass
		
	| potentialTestCaseClass |
	
	potentialTestCaseClass _ Smalltalk classNamed: self name, 'Test'.
	
	^potentialTestCaseClass 
	
 ! !


!Metaclass methodsFor: 'tdd' stamp: 'HAW 2/7/2017 11:56:51'!
testCaseClass

	^self soleInstance testCaseClass ! !


!Categorizer methodsFor: 'tdd' stamp: 'HAW 2/7/2017 09:06:13'!
classesAt: aCategoryName

	^(self listAtCategoryNamed: aCategoryName) collect: [:aClassName | Smalltalk classNamed: aClassName ]
		! !

!Categorizer methodsFor: 'tdd' stamp: 'HAW 2/7/2017 09:06:34'!
testCaseClassesAt: aCategoryName

	^(self classesAt: aCategoryName) select: [ :aClass | aClass is: #TestCaseClass ]! !


!SmalltalkEditor methodsFor: 'editing keys' stamp: 'HAW 2/10/2017 16:03:46'!
acceptAndDebugTest: aKeyboardEvent 
	
	^self acceptAndDebugTest ! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/10/2017 17:34:45'!
acceptAndDebugTest
	
	^self acceptAndWithMethodDo: [ :aPotencialTestMethod | 
		aPotencialTestMethod isTestMethod ifTrue: [ aPotencialTestMethod methodClass debugAsFailure: aPotencialTestMethod selector ]]! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/10/2017 17:34:38'!
acceptAndWithMethodDo: aBlock
	
	| potencialTestMethod |
	
	self acceptContents ifFalse: [ ^false ].
	
	potencialTestMethod _ self codeProvider currentCompiledMethod.
	^potencialTestMethod 
		ifNil: [ false ]
		ifNotNil: [
			aBlock value: potencialTestMethod.
			true]! !


!ProgessiveTestRunner methodsFor: 'initialization' stamp: 'HAW 2/1/2017 19:20:06'!
initializeFor: aTestSuite

	testSuite _ aTestSuite.
	! !

!ProgessiveTestRunner methodsFor: 'evaluating' stamp: 'HAW 1/31/2017 11:50:37'!
value

	testsStream _ ReadStream on: testSuite tests.
	testsStream atEnd
		ifTrue: [ self informNoTestToRun ]
	 	ifFalse:[ self createProgressBarAndRun ]! !

!ProgessiveTestRunner methodsFor: 'progress bar - private' stamp: 'HAW 1/31/2017 10:25:22'!
calculateTestRunIncrement
	
	testRunIncrement _ 1/testsStream size! !

!ProgessiveTestRunner methodsFor: 'progress bar - private' stamp: 'HAW 1/31/2017 10:25:31'!
createProgressBar
		
	progressBar _ ProgressMorph label: testSuite name.
	self calculateTestRunIncrement.
	self updateProgressBarSubLabel.
	! !

!ProgessiveTestRunner methodsFor: 'progress bar - private' stamp: 'HAW 1/31/2017 10:25:43'!
updateDoneIncrement 

 	progressBar incrDone: testRunIncrement 
	! !

!ProgessiveTestRunner methodsFor: 'progress bar - private' stamp: 'HAW 1/31/2017 11:10:25'!
updateProgressBarSubLabel

	testsStream atEnd ifFalse: [
		progressBar subLabel: testsStream next printString, ' (', testsStream position printString, '/', testsStream size printString, ')' ].! !

!ProgessiveTestRunner methodsFor: 'show result - private' stamp: 'HAW 1/31/2017 20:05:25'!
informAllTestPassed 
	
	PopUpMenu inform: testResult printString.
	! !

!ProgessiveTestRunner methodsFor: 'show result - private' stamp: 'HAW 1/31/2017 10:05:21'!
informNoTestToRun
		
	 PopUpMenu inform: 'No test to run'! !

!ProgessiveTestRunner methodsFor: 'show result - private' stamp: 'HAW 2/1/2017 19:26:08'!
openTestResultForDebuggingAndInspection

	testResult forDebuggingAndInspection inspect ! !

!ProgessiveTestRunner methodsFor: 'show result - private' stamp: 'HAW 1/31/2017 10:10:53'!
showDeffects
	
	 | defects |
			
	defects _ 	testResult defects.
	defects size = 1
		ifTrue: [ defects anyOne debug ] 
		ifFalse: [ self openTestResultForDebuggingAndInspection]! !

!ProgessiveTestRunner methodsFor: 'evaluating - private' stamp: 'HAW 2/1/2017 19:14:08'!
createProgressBarAndRun

	self createProgressBar.
	[ self runSuiteShowingProgress ] fork! !

!ProgessiveTestRunner methodsFor: 'evaluating - private' stamp: 'HAW 2/1/2017 19:26:59'!
registerTestSuiteAction
		
	testSuite when: #changed: send: #testRun: to: self! !

!ProgessiveTestRunner methodsFor: 'evaluating - private' stamp: 'HAW 1/31/2017 10:09:41'!
runSuite
		
	testResult _ testSuite run.	
	testResult hasPassed 
		ifTrue: [ self informAllTestPassed ] 
		ifFalse: [self showDeffects ]
	! !

!ProgessiveTestRunner methodsFor: 'evaluating - private' stamp: 'HAW 1/31/2017 11:47:58'!
runSuiteShowingProgress

	[ self registerTestSuiteAction. 
	progressBar openInWorld.
	self runSuite ] ensure: [
		self unregisterTestSuiteAction.
		WorldState addDeferredUIMessage: [progressBar dismissMorph] ].
	! !

!ProgessiveTestRunner methodsFor: 'evaluating - private' stamp: 'HAW 2/1/2017 19:19:28'!
testRun: aTest

	self updateProgressBarSubLabel.
	self updateDoneIncrement 
	
		! !

!ProgessiveTestRunner methodsFor: 'evaluating - private' stamp: 'HAW 1/31/2017 10:04:32'!
unregisterTestSuiteAction
	
	testSuite releaseActionMap ! !


!ProgessiveTestRunner class methodsFor: 'instance creation' stamp: 'HAW 1/31/2017 09:37:34'!
for: aTestSuite

	^self new initializeFor: aTestSuite! !


!TestCase class methodsFor: 'Instance Creation' stamp: 'HAW 2/10/2017 16:01:40'!
debugAsFailure: aSymbol

	^(self selector: aSymbol) debugAsFailure
			! !


!TestSuite class methodsFor: 'Creation' stamp: 'HAW 2/4/2017 20:31:47'!
forClass: aClass

	^(aClass is: #TestCaseClass) 
		ifTrue: [ self forTestCaseClass: aClass ]
		ifFalse: [ self forNoTestCaseClass: aClass ]
! !

!TestSuite class methodsFor: 'Creation' stamp: 'HAW 2/4/2017 21:53:12'!
forCompiledMethod: aCompiledMethod

	^aCompiledMethod isTestMethod 
		ifTrue: [ self forTestMethod: aCompiledMethod ]
		ifFalse: [ self forNoTestMethod: aCompiledMethod ]
	! !

!TestSuite class methodsFor: 'Creation' stamp: 'HAW 2/1/2017 18:43:22'!
forMessageCategoryNamed: aMessageCategoryName of: aClass categorizedWith: aClassOrganizer

	| suite |
	
	suite _ self named: aClass name, ' tests categorized under ',aMessageCategoryName.
	(aClassOrganizer listAtCategoryNamed: aMessageCategoryName) do: [ :selector |
		(aClass compiledMethodAt: selector) isTestMethod ifTrue: [ suite addTest: (aClass selector: selector) ]].
	
	^suite
	! !

!TestSuite class methodsFor: 'Creation' stamp: 'HAW 2/7/2017 10:24:12'!
forSystemCategoryNamed: aCategoryName  using: aSystemOrganizer 

	| testCaseClasses |
	
	testCaseClasses _ aSystemOrganizer testCaseClassesAt: aCategoryName.
	
	^testCaseClasses isEmpty 
		ifTrue: [ self forClasses: (aSystemOrganizer classesAt: aCategoryName) named: aCategoryName, ' infered tests' ]
		ifFalse: [ self forTestCaseClasses: testCaseClasses named: aCategoryName, ' tests' ]
		
! !

!TestSuite class methodsFor: 'Creation - Private' stamp: 'HAW 2/7/2017 10:25:51'!
allTestCaseClassesReferencing: aClass

	^(aClass allCallsOn 
		collect: [ :aMethodReference | aMethodReference actualClass ]
		thenSelect: [ :aPotentialTestCaseClass | aPotentialTestCaseClass is: #TestCaseClass ]) asSet.! !

!TestSuite class methodsFor: 'Creation - Private' stamp: 'HAW 2/7/2017 10:27:27'!
allTestsSending: aSelector
	
	^(Smalltalk allCallsOn: aSelector) select: [:aMethodReference | 
		(aMethodReference actualClass is: #TestCaseClass) and: [aMethodReference compiledMethod isTestMethod ]].! !

!TestSuite class methodsFor: 'Creation - Private' stamp: 'HAW 2/7/2017 09:08:23'!
forClasses: classes named: name

	| suite |
	
	suite _ classes
		inject: (self named: name)
		into: [ :partialSuite :aClass | partialSuite addTests: (self forClass: aClass) tests ].
		
	^suite
		
! !

!TestSuite class methodsFor: 'Creation - Private' stamp: 'HAW 2/7/2017 10:32:17'!
forNoTestCaseClass: aClass 

	^aClass 
		withTestCaseClassDo: [ :aTestCaseClass | self forTestCaseClass: aTestCaseClass ]
		ifNone: [ self forReferencesToClass: aClass ]

! !

!TestSuite class methodsFor: 'Creation - Private' stamp: 'HAW 2/7/2017 11:28:55'!
forNoTestMethod: aCompiledMethod 
	
	| allTestSenders testCaseClassesReferencingClass reducedTestSenders suite |
	
	allTestSenders _ self allTestsSending: aCompiledMethod selector.
	testCaseClassesReferencingClass _ aCompiledMethod methodClass 
		withTestCaseClassDo:  [:aTestCaseClass | Array with: aTestCaseClass ]
		ifNone: [ self allTestCaseClassesReferencing: aCompiledMethod methodClass ].
		
	reducedTestSenders _ allTestSenders select: [ :aMethodReference | testCaseClassesReferencingClass includes: aMethodReference actualClass ].
	reducedTestSenders isEmpty 
		ifTrue: [ suite _ self forClass: aCompiledMethod methodClass ]
		ifFalse: [ 
			suite _ self named: 'Tests senders of ', aCompiledMethod selector.
			reducedTestSenders do: [ :aMethodReference | suite addTest: (aMethodReference actualClass selector: aMethodReference selector)]].
	
	^suite! !

!TestSuite class methodsFor: 'Creation - Private' stamp: 'HAW 2/7/2017 10:33:27'!
forReferencesToClass: aClass 

	| testCaseClasses |
	
	testCaseClasses _ self allTestCaseClassesReferencing: aClass.
		
	^testCaseClasses 
		inject: (self named: aClass name, ' all test references')
		into: [ :suite :aTestCaseClass | aTestCaseClass addToSuiteFromSelectors: suite ]
	

! !

!TestSuite class methodsFor: 'Creation - Private' stamp: 'HAW 2/7/2017 10:34:27'!
forTestCaseClass: aTestCaseClass 

	| suite |
	
	suite _ aTestCaseClass buildSuite.
	suite name: aTestCaseClass name, ' tests'.
	
	^suite! !

!TestSuite class methodsFor: 'Creation - Private' stamp: 'HAW 2/7/2017 09:04:48'!
forTestCaseClasses: testCaseClasses named: aName

	| suite |
	
	suite _ testCaseClasses
		inject: (self named: aName) 
		into: [:partialSuite :aTestCaseClass | aTestCaseClass addToSuiteFromSelectors: partialSuite ].
		
	^suite! !

!TestSuite class methodsFor: 'Creation - Private' stamp: 'HAW 2/4/2017 21:53:22'!
forTestMethod: aCompiledMethod
	
	| suite |
	
	suite _ self named: 'Test'.
	suite addTest: (aCompiledMethod methodClass selector: aCompiledMethod selector).
	
	^suite
		! !


!UndefinedObject methodsFor: 'class hierarchy' stamp: 'HAW 2/10/2017 17:37:23'!
allSuperclassesUpTo: aSuperclass 

	self error: (self superclassNotValidErrorDescriptionFor: aSuperclass)! !

!UndefinedObject methodsFor: 'class hierarchy' stamp: 'HAW 2/10/2017 17:37:30'!
superclassNotValidErrorDescriptionFor: aClass 

	^aClass name, ' not in superclasses chain'! !


!Behavior methodsFor: 'system-support' stamp: 'HAW 2/4/2017 20:51:10'!
allCallsOn
	"Answer a SortedCollection of all the methods that refer to me by name or 
	as part of an association in a global dict."
	"
	^ (Smalltalk
		allCallsOn: (Smalltalk associationAt: self theNonMetaClass name))
		, (Smalltalk allCallsOn: self theNonMetaClass name)
	"

	^ Smalltalk allCallsOn: self theNonMetaClass name! !


!SystemDictionary methodsFor: 'browsing' stamp: 'HAW 2/4/2017 20:49:09'!
browseAllCallsOnClass: aClass
	"Create and schedule a message browser on each method that refers to 
	aClass. For example, Smalltalk browseAllCallsOnClass: Object."
	self
		browseMessageList: aClass allCallsOn asArray sort
		name: 'Users of class ' , aClass theNonMetaClass name
		autoSelect: aClass theNonMetaClass name.! !


!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/1/2017 18:51:22'!
acceptAndTest
	
	self acceptThenTestMethodAndSuite: [ :aMethod | self testSuiteOf: aMethod methodClass ].
	^true! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/1/2017 18:50:41'!
acceptAndTestAll
	
	self acceptThenTestMethodAndSuite: [ :aMethod | self testSuiteForCategoryOf: aMethod methodClass ].
	^true
	
	! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/10/2017 17:34:52'!
acceptThenTestMethodAndSuite: aSuiteBuilder
	
	self acceptAndWithMethodDo: [ :aPotencialTestMethod |
		self runAndDebuggIfNecessary: aPotencialTestMethod.
		self runTestSuite: (aSuiteBuilder value: aPotencialTestMethod) ]! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/10/2017 16:08:47'!
runAndDebuggIfNecessary: aPotencialTestMethod

	aPotencialTestMethod isTestMethod ifTrue: [
		aPotencialTestMethod methodClass debug: aPotencialTestMethod selector ]! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/1/2017 19:19:02'!
runTestSuite: aTestSuite

	(ProgessiveTestRunner for: aTestSuite) value
	
	! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/5/2017 10:49:25'!
testSuiteForCategoryOf: aClass

	^TestSuite forSystemCategoryNamed: aClass category using: SystemOrganization 
! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HAW 2/5/2017 10:48:27'!
testSuiteOf: aClass

	^TestSuite forClass: aClass
! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 2/10/2017 15:58:27'!
cmdShortcutsSpec
	"
	Editor initialize
	"
	"arranged in QWERTY keyboard order"
	^#(
		#(		$r	#acceptAndDebugTest:		'Saves method and debugs it')
		#(		$t	#acceptAndTest:				'Saves method, runs it as test and then all tests in class. Opens debugger if error')
		#(		$y	#acceptAndTestAll:				'Saves method, runs it as test and then all tests in class category. Opens debugger if error')
		#(		$i	#inspectIt:							'Inspect it (selection is a valid expression, or selection is over an inspect-ilst)')
		#(		$p	#printIt:								'Print it (selection is a valid expression)')

		#(		$s	#save:								'Save (i.e. accept)')
		#(		$d	#doIt:								'Do it (selection is a valid expression)')
		#(		$l	#cancelEdits:						'Cancel')

		#(		$b	#browseIt:							'Browse it (selection is a class name or cursor is over a class-list or message-list)')
		#(		$n	#sendersOfIt:						'Senders of it (selection is a message selector or cursor is over a class-list or message-list)')
		#(		$m	#implementorsOfIt:				'Implementors of it (selection is a message selector or cursor is over a class-list or message-list)')

		#(		$E	#methodStringsContainingit:'Method strings containing it')
		#(		$T	#displayIfTrue:					'Insert #ifTrue:')
		#(		$I	#exploreIt:							'Inspect via Object Explorer')

		#(		$A	#argAdvance:						'Advance argument')
		#(		$D	#debugIt:							'Debut it')
		#(		$F	#displayIfFalse:					'Insert #ifFalse:')
		#(		$G	#fileItIn:							'File in selection')

		#(		$V	#pasteInitials:					'Paste author initials')
		#(		$N	#referencesToIt:					'References to it (selection is a class name, or cursor is over a class-list or message-list)')
	)! !

!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'HAW 2/10/2017 15:57:40'!
initializeMenu
	"Initialize the mouseButton2 (right button) pop-up menu and corresponding messages."
	"
	Editor initialize
	"

	menu _ SelectionMenu fromArray: {
		{'Help...'.										#openHelp}.
		#-.
		{'Do it (d)'.									#doIt}.
		{'Print it (p)'.									#printIt}.
		{'Inspect it (i)'.								#inspectIt}.
		{'Explore it (I)'.								#exploreIt}.
		{'Debug it (D)'.								#debugIt}.
		{'Profile it'.									#profileIt}.
		#-.
		{'Explain'.										#explain}.
		{'Browse it (b)'.								#browseIt}.
		{'Senders of it (n)'.							#sendersOfIt}.
		{'Implementors of it (m)'.					#implementorsOfIt}.
		{'References to it (N)'.						#referencesToIt}.
		#-.
		{'Method Strings with it (E)'.				#methodStringsContainingit}.
		{'Method Source with it'.					#methodSourceContainingIt}.
		{'Class Comments with it'.					#classCommentsContainingIt}.
		#-.
		{'Accept (s)'.									#acceptContents}.
		{'Cancel (l)'.									#cancelEdits}.
		{'Accept & Run Test in Class (t)'.			#acceptAndTest}.
		{'Accept & Run Test in Category (y)'.		#acceptAndTestAll}.
		{'Accept & Debug Test (r)'.					#acceptAndDebugTest}.
		#-.
		{'More...'.										#getMenu2}.
	}.
	menu2 _ SelectionMenu fromArray: {
		{'Find...(f)'.									#find}.
		{'Find Again (g)'.								#findAgain}.
		{'Use Selection for Find (j)'.				#setSearchString}.
		#-.
		{'Undo - multiple (z)'.						#undo}.
		{'Redo - multiple (Z)'.						#redo}.
		{'Undo / Redo history'.						#offerUndoHistory}.
		#-.
		{'Copy (c)'.									#copySelection}.
		{'Cut (x)'.										#cut}.
		{'Paste (v)'.									#paste}.
		{'Paste without Format'.						#pasteString}.
		{'Paste...'.									#pasteRecent}.
		#-.
		{'Toggle WordWrap'.						#wrapOnOff}.
		{'Set Font... (k)'.								#offerFontMenu}.
		{'Clear Font'.									#clearFont}.
		{'Set Default Font...'.						#offerDefaultFontMenu}.
		#-.
		{'More...'.										#getMenu}.
	}! !


!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'HAW 1/31/2017 11:56:31'!
classListKey: aChar from: view 
	"Respond to a Command key.  I am a model with a list of classes and a 
	code pane, and I also have a listView that has a list of methods.  The 
	view knows how to get the list and selection."

	aChar == $r ifTrue: [^ model recent].
	aChar == $h ifTrue: [^ self browseHierarchy].
	aChar == $x ifTrue: [^ model removeClass].
	aChar == $t ifTrue: [^ model runClassTests ].
	
	^ self messageListKey: aChar from: view! !

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'HAW 2/10/2017 17:33:25'!
messageListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also
	have a listView that has a list of methods.  The view knows how to get
	the list and selection."

	| sel class |
	aChar == $D ifTrue: [^ model toggleDiffing].

	sel _ model selectedMessageName.
	aChar == $m ifTrue: [  "These next two put up a type in if no message selected"
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllImplementorsOf: to: Smalltalk].
	aChar == $n ifTrue: [
		^ self useSelector: sel orGetSelectorAndSendQuery: #browseAllCallsOn: to: Smalltalk].

	"The following require a class selection"
	(class _ model selectedClassOrMetaClass) ifNil: [^ self ].
	aChar == $b ifTrue: [^ BrowserWindow fullOnClass: class selector: sel].
	aChar == $N ifTrue: [^ self browseClassRefs].
	aChar == $i ifTrue: [^ self methodHierarchy].
	aChar == $h ifTrue: [^ self browseHierarchy].
	aChar == $p ifTrue: [^ self browseFullProtocol].

	"The following require a method selection"
	sel ifNotNil: [
		aChar == $o ifTrue: [^ model fileOutMessage].
		aChar == $c ifTrue: [^ model copySelector].
		aChar == $v ifTrue: [^ self browseVersions].
		aChar == $O ifTrue: [^ self openSingleMessageBrowser].
		aChar == $x ifTrue: [^ model removeMessage].
		aChar == $t ifTrue: [^ model runMethodTest].
		aChar == $r ifTrue: [^ model debugMethodTest]]! !

!CodeWindow methodsFor: 'keyboard shortcuts' stamp: 'HAW 2/7/2017 10:49:07'!
systemCatListKey: aChar from: view
	"Respond to a Command key.  I am a model with a code pane, and I also have a listView that has a list of methods.  The view knows how to get the list and selection."

	aChar == $f ifTrue: [^ self findClass].
	aChar == $x ifTrue: [^ model removeSystemCategory].
	aChar == $t ifTrue: [ ^model runSystemCategoryTests ].
	
	^ self classListKey: aChar from: view! !


!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 1/31/2017 11:32:21'!
classListMenu
	"Set up the menu to apply to the receiver's class list, honoring the #shifted boolean"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addList: #(
		('browse full (b)'			browseMethodFull)
		('browse hierarchy (h)'		browseHierarchy)
		('browse protocol (p)'		browseFullProtocol)
		-
		('fileOut'					fileOutClass				''		model)
		-
		('show hierarchy'			hierarchy					''		model)
		('show definition'			editClass					''		model)
		('show comment'			editComment			''		model)
		-
		('inst var refs...'				browseInstVarRefs)
		('inst var defs...'			browseInstVarDefs)
		-
		('class var refs...'			browseClassVarRefs)
		('class vars'					browseClassVariables)
		('class refs (N)'				browseClassRefs)
		-
		('rename class ...'			renameClass				''		model)
		('copy class...'				copyClass				''		model)
		('remove class (x)'			removeClass				''		model)
		-
		('Run tests (t)'		runClassTests 	''	model)
		('more...'					offerShiftedClassListMenu)).
	^ aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 1/31/2017 19:57:02'!
messageCategoryMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: model.			"All the options are for the model."
	aMenu addList: #(
		('fileOut'							fileOutMessageCategories)
		-
		('reorganize'						editMessageCategories)
		('alphabetize'						alphabetizeMessageCategories)
		('remove empty categories'		removeEmptyCategories)
		('categorize all uncategorized'		categorizeAllUncategorizedMethods)
		('new category...'					addCategory)
		-
		('rename...'							renameCategory)
		('remove'							removeMessageCategory)
		-
		('Run tests'	runMessageCategoryTests)).
	^aMenu! !

!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 2/10/2017 17:29:43'!
messageListMenu
	"Answer the message-list menu"
	"Changed by emm to include menu-item for breakpoints"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	aMenu addList:#(
			('what to show...'				offerWhatToShowMenu)
                	('toggle break on entry'		toggleBreakOnEntry					''		model)
            	-
			('browse full (b)' 				browseMethodFull)
			('browse hierarchy (h)'			browseHierarchy)
			('browse method (O)'			openSingleMessageBrowser)
			('browse protocol (p)'			browseFullProtocol)
			-
			('fileOut'						fileOutMessage							''		model)
			-
			('senders of... (n)'				browseSendersOfMessages)
			('implementors of... (m)'			browseMessages)
			('inheritance (i)'					methodHierarchy)
			('versions (v)'					browseVersions)
			-
			('inst var refs...'					browseInstVarRefs)
			('inst var defs...'				browseInstVarDefs)
			('class var refs...'				browseClassVarRefs)
			('class variables'				browseClassVariables)
			('class refs (N)'					browseClassRefs)
			-
			('remove method (x)'			removeMessage							''		model)
			('Run test (t)'		runMethodTest 	'' 	model)
			('Debug test (r)'		debugMethodTest 	'' 	model)
			-
			('more...'						openShiftedMessageListMenu)).
	^ aMenu
! !

!BrowserWindow methodsFor: 'menu building' stamp: 'HAW 1/31/2017 11:52:17'!
systemCategoryMenu

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	self flag: #renameSystemCategory.	"temporarily disabled"
	aMenu addList: #(
		('find class... (f)'				findClass)
		('recent classes... (r)'		recent									''		model)
		-
		('browse all'				browseAllClasses)
		('browse'					openSystemCategoryBrowser)
		-
		('fileOut'					fileOutSystemCategory				''		model)
		-
		('reorganize'				editSystemCategories					''		model)
		('alphabetize'				alphabetizeSystemCategories		''		model)
		-
		('update'					updateSystemCategories				''		model)
		('add item...'				addSystemCategory					''		model)
"		('rename...'					renameSystemCategory				''		model)"
		('remove'					removeSystemCategory				''		model)
		-
		('move to top'				moveSystemCategoryTop				''		model)
		('move up'					moveSystemCategoryUp				''		model)
		('move down'				moveSystemCategoryDown			''		model)
		('move to bottom' 			moveSystemCategoryBottom			''		model)
		-
		('Run tests (t)'				runSystemCategoryTests  	''	model)).
	^aMenu! !


!Theme methodsFor: 'menus' stamp: 'HAW 2/10/2017 17:30:49'!
miscellaneousIcons

	"Everything else."

	^ {
		#('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon.
		#('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history' 'code file contents' 'package file contents') -> #changesIcon.
		#('Install New Updates' 'update' 'turn on auto-update (a)' 'update list (u)' 'install code' 'fileIn entire file' 'install package') -> #updateIcon.
		#('find again (g)' 'full stack (k)') -> #systemIcon.
		#('print it (p)' 'check change set for slips') -> #printIcon.
		#('accept (s)' 'make changes go to me (m)') -> #acceptIcon.
		#('cancel (l)' 'turn off auto-update (a)') -> #cancelIcon.
		#('debug...' 'Debug it (D)' 'toggle break on entry') -> #debugIcon.
		#('close' 'close all debuggers' 'close top window') -> #closeIcon.
		#('collapse' 'hide taskbar' 'collapse all windows') -> #collapseIcon.
		#('expand / contract' 'show taskbar' 'restore all windows') -> #expandIcon.
		#('menu') -> #windowMenuIcon.
		#('browse all' 'browser' 'browse it (b)' 'MessageTally UI and browse' 'browse recent submissions' 'browse full (b)' 'find changed browsers...' 'browse (b)' 'browse my changes') -> #editFindReplaceIcon.
		#('workspace' 'workspace with contents') -> #terminalIcon.
		#('styled text editor' 'text editor'  'basic text editor'  'unicode editor' 'edit this list' 'edit postscript...' 'add postscript...') -> #textEditorIcon.
		#('file list' 'find a fileList' 'compress file') -> #systemFileManagerIcon.
		#('transcript' 'find a transcript' 'Transcript clear.' 'log to transcript') -> #printerIcon.
		#('process browser' 'vm statistics' 'MessageTally all Processes') -> #systemMonitorIcon.
		#('emergency evaluator' 'conflicts with other change sets' 'check for slips' 'conflicts with change set opposite' 'conflicts with category opposite') -> #emblemImportantIcon.
		#('change sorter') -> #halfRefreshIcon.
		#('SUnit Test Runner' 'Accept & Run Test in Class (t)' 'Accept & Run Test in Category (y)' 'Run tests (t)' 'Run tests' 'Run test (t)' 'Accept & Debug Test (r)' 'Debug test (r)') -> #weatherFewCloudsIcon.
		#('Font Sizes...' 'system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon.
		#('full screen on') -> #viewFullscreenIcon.
		#('full screen off') -> #exitFullscreenIcon.
		#('set desktop color...') -> #wallpaperIcon.
		#('preferences...' 'All preferences...' 'what to show...' 'view as hex') -> #preferencesIcon.
		#('Editor keyboard shortcuts') -> #keyboardShortcutsIcon.
		#('world menu help') -> #globeIcon.		"currently unused, but a neat icon"
		#('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon.
		#('set code author...' 'check for other authors' 'check for any other authors') -> #usersIcon.
		#('space left') -> #removableMediaIcon.
		#('start drawing all again' 'window color...') -> #graphicsIcon.
		#('start stepping again') -> #mediaPlaybackStartIcon.
		#('file out current change set' 'fileOut' 'File out and remove (o)' 'File out and keep (k)') -> #fileOutIcon.
		#('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'Profile it') -> #clockIcon.
		#('senders of it (n)' 'senders of... (n)' 'local senders of...' 'senders (n)') -> #mailForwardIcon.
		#('implementors of it (m)' 'implementors of... (m)' 'implementors of sent messages') -> #developmentIcon.
		#('references to it (N)') -> #addressBookIcon.
		#('class var refs...' 'class refs (N)' 'class variables' 'class vars' 'local implementors of...' 'subclass template') -> #classIcon.
		#('inst var refs...' 'inst var defs...' 'sample instance') -> #instanceIcon.
		#('Use Selection for Find (j)' 'rename class ...' 'rename...' 'change title...') -> #saveAsIcon.
		#('smalltalk options' 'method source with it' 'browse method (O)' 'check for uncategorized methods') -> #scriptIcon.
		#('method strings with it (E)' 'Toggle WordWrap') -> #genericTextIcon.
		#('browse hierarchy (h)' 'move to top' 'promote to top of list') -> #goTopIcon.
		#('move up' 'make next-to-topmost') -> #goUpIcon.
		#('move to bottom' 'send to back' 'send top window to back') -> #goBottomIcon.
		#('inheritance (i)' 'move down') -> #goDownIcon.
		#('browse protocol (p)' 'spawn sub-protocol') -> #spreadsheetIcon.
		#('spawn full protocol') -> #speadsheetTemplateIcon.
		#('alphabetize') -> #fontXGenericIcon.
		#('Installed Packages' 'browse' 'show category (C)' 'categorize all uncategorized' 'select change set...' 'view affected class categories') -> #packageIcon.
		#('remove from current change set' 'remove empty categories' 'subtract other side (-)' 'remove from this browser') -> #listRemoveIcon.
		#('add to current change set' 'add all meths to current chgs' 'add preamble (p)' 'More...') -> #listAddIcon.
		#('toggle diffing (D)' 'toggle selections') -> #switchIcon.
		#('reorganize' 'create inst var accessors' 'ChangeSorter reorderChangeSets.' 'reorder all change sets' 'by name' 'by size' 'by date') -> #sendReceiveIcon.
		#('unsent methods' 'unreferenced class vars' 'unreferenced inst vars' 'Undeclared inspect.' 'Undeclared removeUnreferencedKeys; inspect.' 'ChangeSorter removeEmptyUnnamedChangeSets.' 'check for unsent messages') -> #junkIcon.
		#('find changed windows...') -> #newWindowIcon.
		#('make undraggable') -> #pushPinIcon.
		#('Utilities saveScreenshot.') -> #stillCameraIcon.
		#('add new directory') -> #newFolderIcon.
		#('select all' 'deselect all') -> #selectAllIcon.
		#('sort by date') -> #dateIcon.
		#('justified') -> #formatJustifyFillIcon.
		#('centered') -> #formatJustifyCenterIcon.
		#('set alignment...' 'leftFlush') -> #formatJustifyLeftIcon.
		#('rightFlush') -> #formatJustifyRightIcon.
		#('signal Semaphore (S)') -> #haloHelpIcon.
		#('Change Paragraph Style...' 'Change Character Style...' 'Remove Character Style' 'Replace all uses of Paragraph Style...' 'Replace all uses of Character Style...' 'Set Default Font...') -> #fontXGenericIcon.
		#('Clear Font') -> #newIcon.
		#('code file browser' 'package file browser') -> #findIcon.
	}! !


!TestCase methodsFor: 'Accessing' stamp: 'HAW 2/10/2017 17:37:41'!
should: aBlock raise: anExceptionalType withExceptionDo: assertionsBlock

	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalType withExceptionDo: assertionsBlock)
			! !

!TestCase methodsFor: 'Private' stamp: 'HAW 2/10/2017 17:38:10'!
executeShould: aBlock inScopeOf: anExceptionType withExceptionDo: assertionsBlock

	^[aBlock value.
 	false] 
		on: anExceptionType
		do: [:exception | 
			assertionsBlock value: exception.
			exception sunitExitWith: true]! !


!TestCase class methodsFor: 'Testing' stamp: 'HAW 2/10/2017 16:32:10'!
is: aSymbol

	^self ~= TestCase 
		and: [ aSymbol == #TestCaseClass or: [ super is: aSymbol ]]! !

!TestCase class methodsFor: 'Testing' stamp: 'HAW 2/1/2017 19:35:57'!
shouldInheritSelectors
	"I should inherit from an Abstract superclass but not from a concrete one by default, unless I have no testSelectors in which case I must be expecting to inherit them from my superclass.  If a test case with selectors wants to inherit selectors from a concrete superclass, override this to true in that subclass."

	^self ~= TestCase 
		and: [ self superclass isAbstract or: [self testSelectors isEmpty]]

"$QA Ignore:Sends system method(superclass)$"
			! !


!TestResultForDebuggingAndInspection methodsFor: 'printing' stamp: 'HAW 2/7/2017 10:51:56'!
print: aCollectionOfTestCases startingWith: header on: aStream
	
	aCollectionOfTestCases do: [ :aTestCase |
		aStream 
			nextPutAll: header;
			space;
			nextPutAll: aTestCase class name;
			nextPutAll: ' debug: #';
			nextPutAll: aTestCase selector;
			newLine ]
		
		
	! !

!methodRemoval: TestSuite class #allTestCasesReferencing:!
TestSuite class removeSelector: #allTestCasesReferencing:!
!methodRemoval: TestSuite class #allTestReferencesTo:!
TestSuite class removeSelector: #allTestReferencesTo:!
!methodRemoval: TestSuite class #allTestsReferencing:!
TestSuite class removeSelector: #allTestsReferencing:!
!methodRemoval: TestSuite class #from:using:!
TestSuite class removeSelector: #from:using:!
!methodRemoval: TestSuite class #fromClass:!
TestSuite class removeSelector: #fromClass:!
!methodRemoval: TestSuite class #fromSystemCategoryNamed:using:!
TestSuite class removeSelector: #fromSystemCategoryNamed:using:!
!methodRemoval: ProgessiveTestRunner #initializeFor:informingResultUsing:!
ProgessiveTestRunner removeSelector: #initializeFor:informingResultUsing:!
!methodRemoval: ProgessiveTestRunner #initializeFor:showingTestPassedWith:!
ProgessiveTestRunner removeSelector: #initializeFor:showingTestPassedWith:!
!methodRemoval: ProgessiveTestRunner #showProgressBarAndRunSuite!
ProgessiveTestRunner removeSelector: #showProgressBarAndRunSuite!
!methodRemoval: SmalltalkEditor #acceptAndWithTestMethodDo:!
SmalltalkEditor removeSelector: #acceptAndWithTestMethodDo:!
!methodRemoval: SmalltalkEditor #flashWith:!
SmalltalkEditor removeSelector: #flashWith:!
!methodRemoval: SmalltalkEditor #flashWithGreen!
SmalltalkEditor removeSelector: #flashWithGreen!
!methodRemoval: Categorizer #testCasesAt:!
Categorizer removeSelector: #testCasesAt:!
!methodRemoval: Class #withTestCaseClassDo:ifNone:!
Class removeSelector: #withTestCaseClassDo:ifNone:!
!methodRemoval: Behavior #withTestClassDo:ifNone:!
Behavior removeSelector: #withTestClassDo:ifNone:!

!Browser reorganize!
('accessing' acceptedStringOrText contents:notifying: contentsSelection doItReceiver editSelection editSelection: request:initialAnswer:)
('annotation' annotation)
('breakpoints' toggleBreakOnEntry)
('class comment pane' newClassComment:)
('class functions' classCommentText classDefinitionText copyClass createInstVarAccessors defineClass:notifying: editClass editComment explainSpecial: fileOutClass hierarchy makeNewSubclass plusButtonHit removeClass renameClass)
('class list' classList classListIndex classListIndex: recent selectClass: selectedClass selectedClassName)
('code pane' compileMessage:notifying:)
('message category functions' addCategory alphabetizeMessageCategories canShowMultipleMessageCategories categoryOfCurrentMethod changeMessageCategories: editMessageCategories fileOutMessageCategories removeEmptyCategories removeMessageCategory renameCategory showHomeCategory)
('message category list' categorizeAllUncategorizedMethods messageCategoryList messageCategoryListIndex messageCategoryListIndex: messageCategoryListSelection rawMessageCategoryList selectOriginalCategoryForCurrentMethod selectedMessageCategoryName setOriginalCategoryIndexForCurrentMethod)
('message functions' defineMessageFrom:notifying: removeMessage removeMessageFromBrowser)
('message list' messageList messageListIndex messageListIndex: reformulateList selectedMessage selectedMessageName selectedMessageName:)
('metaclass' classCommentIndicated classMessagesIndicated classOrMetaClassOrganizer indicateClassMessages indicateInstanceMessages instanceMessagesIndicated metaClassIndicated metaClassIndicated: selectedClassOrMetaClass setClassOrganizer)
('system category functions' addSystemCategory alphabetizeSystemCategories changeSystemCategories: editSystemCategories fileOutSystemCategory moveSystemCategoryBottom moveSystemCategoryDown moveSystemCategoryTop moveSystemCategoryUp potentialClassNames removeSystemCategory renameSystemCategory systemCategoryBrowser updateSystemCategories)
('system category list' indexIsOne indexIsOne: selectCategoryForClass: selectedSystemCategoryName systemCategoryList systemCategoryListIndex systemCategoryListIndex: systemCategorySingleton)
('shout styling' shoutAboutToStyle:)
('initialization' defaultBrowserTitle labelString methodCategoryChanged setClass:selector: setSelector: systemOrganizer:)
('tdd' debugMethodTest runClassTests runMessageCategoryTests runMethodTest runSystemCategoryTests)
!


!Behavior reorganize!
('accessing' classDepth compilerClass decompilerClass format methodDict name parserClass sourceCodeTemplate typeOfClass typeOfClassPreSpur typeOfClassSpur)
('testing' canZapMethodDictionary instSize instSpec isBehavior isBits isBytes isCompiledMethodClass isEphemeronClass isFixed isImmediateClass isMeta isObsolete isPointers isVariable isVariablePreSpur isVariableSpur isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('copying' copyOfMethodDictionary postCopy)
('printing' literalScannedAs:notifying: longPrintOn: printHierarchy printOn: printWithClosureAnalysisOn: storeLiteral:on:)
('compiling' binding compile: compile:notifying: compileAll compileAllFrom: decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges recompileNonResidentMethod:atSelector:from: variablesAndOffsetsDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new:)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: subclasses superclass superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesUpTo:)
('accessing method dictionary' >> addSelector:withMethod: addSelector:withMethod:notifying: addSelectorSilently:withMethod: allSelectors changeRecordsAt: compiledMethodAt: compiledMethodAt:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: lookupSelector: methodDict: methodDictionary methodDictionary: methodHeaderFor: methodsDo: precodeCommentOrInheritedCommentFor: removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: zapAllMethods)
('accessing instances and variables' allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSharedPools allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames elementSize inspectAllInstances inspectSubInstances instVarNames instanceCount sharedPools someInstance)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass kindOfSubclassPreSpur kindOfSubclassSpur)
('testing method dictionary' allUnsentMessages bindingOf: canUnderstand: classBindingOf: hasMethods includesSelector: thoroughWhichSelectorsReferTo:special:byte: whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferTo: whichSelectorsReferTo:special:byte: whichSelectorsStoreInto:)
('enumerating' allInstancesDo: allInstancesOrNil allSubInstancesDo: allSubclassesDo: allSubclassesDoGently: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDoGently: withAllSuperclassesDo:)
('user interface' allLocalCallsOn: allUnreferencedInstanceVariables crossReference lastUnderscoreMeansSubscript unreferencedInstanceVariables withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicRemoveSelector: becomeCompact becomeCompactSimplyAt: becomeUncompact flushCache handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: indexIfCompact setFormat: spaceUsed)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses removeAllObsoleteSubclasses removeObsoleteSubclass:)
('methods' closuresInfoAt: stampAt:)
('system-support' allCallsOn)
('initialization' superclass:methodDictionary:format:)
('release' obsolete)
('*Squeak Compatibility')
('comparing' identityHash)
('tdd' testCaseClass withTestCaseClassDo:ifNone:)
!


!Class reorganize!
('accessing' classPool classPool: classPoolFrom: name)
('testing' hasMethods isObsolete)
('copying' copy)
('class name' rename:)
('instance variables' addInstVarName: removeInstVarName:)
('class variables' addClassVarName: allClassVarNames classVarNames ensureClassPool removeClassVarName:)
('pool variables' addSharedPool: allSharedPools removeSharedPool: sharedPools)
('compiling' binding bindingOf: compileAllFrom: localBindingOf: possibleVariablesFor:continuedFrom:)
('subclass creation' immediateSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category: weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:)
('fileIn/Out' fileOut fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: fileOutPool:onFileStream: fileOutSharedPoolsOn: objectForDataStream: reformatAll shouldFileOutPool: shouldFileOutPools storeDataOn:)
('accessing class hierarchy' addSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('private' setName: spaceUsed)
('initialization' declare: sharing: superclass:methodDict:format:name:organization:instVarNames:classPool:sharedPools: superclass:methodDictionary:format:)
('release' obsolete removeFromSystem removeFromSystem: removeFromSystemUnlogged unload)
('tdd' testCaseClass)
!


!Metaclass reorganize!
('accessing' allInstances category isMeta name soleInstance theMetaClass theNonMetaClass)
('copying' postCopy)
('instance creation' new)
('instance variables' addInstVarName: removeInstVarName:)
('pool variables' classPool)
('class hierarchy' addObsoleteSubclass: addSubclass: obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('compiling' acceptsLoggingOfCompilation bindingOf: possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isObsolete)
('enumerating' allInstancesDo:)
('private' replaceObsoleteInstanceWith:)
('initialization' adoptInstance:from: instanceVariableNames:)
('tdd' testCaseClass)
!


!Categorizer reorganize!
('accessing' addCategory: addCategory:before: allMethodSelectors categories categories: categoryOfElement: changeFromCategorySpecs: changeFromString: classify:under: classify:under:suppressIfDefault: classifyAll:under: elementCategoryDict isEmptyCategoryNumber: listAtCategoryNamed: listAtCategoryNumber: moveCategoryBottom: moveCategoryDown: moveCategoryTop: moveCategoryUp: numberOfCategoryOfElement: removeCategory: removeElement: removeEmptyCategories renameCategory:toBe: sortCategories)
('printing' printOn:)
('fileIn/Out' scanFrom:)
('private' elementArray firstIndexOfCategoryNumber: lastIndexOfCategoryNumber: setDefaultList:)
('copying' postCopy)
('testing' hasAnyCategoriesSuchThat:)
('tdd' classesAt: testCaseClassesAt:)
!


!SmalltalkEditor reorganize!
('editing keys' acceptAndDebugTest: acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt printIt profileIt)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn getMenu2 implementorsOfIt methodSourceContainingIt methodStringsContainingit referencesToIt selectedSelector selectedSymbol sendersOfIt)
('typing/selecting keys' argAdvance: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier selectWord)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndDebugTest acceptAndTest acceptAndTestAll acceptAndWithMethodDo: acceptThenTestMethodAndSuite: runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
!


!BrowserWindow reorganize!
('GUI building' buildLowerPanes buildMorphicClassColumn buildMorphicClassList buildMorphicCommentPane buildMorphicMessageCatList buildMorphicMessageList buildMorphicSwitches buildMorphicSystemCatList buildMorphicWindow buildNoSysCatMorphicWindow windowColor)
('menu building' addExtraShiftedItemsTo: classListMenu messageCategoryMenu messageListMenu shiftedClassListMenu shiftedMessageListMenu systemCatSingletonMenu systemCategoryMenu)
('menu commands' browseAllClasses openSystemCategoryBrowser)
('commands' findClass)
('keyboard shortcuts' systemCatSingletonKey:from:)
!


!ProgessiveTestRunner reorganize!
('initialization' initializeFor:)
('evaluating' value)
('progress bar - private' calculateTestRunIncrement createProgressBar updateDoneIncrement updateProgressBarSubLabel)
('show result - private' informAllTestPassed informNoTestToRun openTestResultForDebuggingAndInspection showDeffects)
('evaluating - private' createProgressBarAndRun registerTestSuiteAction runSuite runSuiteShowingProgress testRun: unregisterTestSuiteAction)
!


!ProgessiveTestRunner class reorganize!
('instance creation' for:)
!


!TestSuite class reorganize!
('Creation' forClass: forCompiledMethod: forMessageCategoryNamed:of:categorizedWith: forSystemCategoryNamed:using: named:)
('Creation - Private' allTestCaseClassesReferencing: allTestsSending: forClasses:named: forNoTestCaseClass: forNoTestMethod: forReferencesToClass: forTestCaseClass: forTestCaseClasses:named: forTestMethod:)
!

