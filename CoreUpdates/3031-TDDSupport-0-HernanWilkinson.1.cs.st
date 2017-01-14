'From Cuis 5.0 of 7 November 2016 [latest update: #3025] on 10 January 2017 at 8:09:15 pm'!
!classDefinition: #TestResultForDebuggingAndInspection category: #'Tools-Testing'!
Object subclass: #TestResultForDebuggingAndInspection
	instanceVariableNames: 'testResult'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!

!CompiledMethod methodsFor: 'testing' stamp: 'HernanWilkinson 1/10/2017 18:22:10'!
isTestMethod

    ^ (self methodClass is: #TestCaseClass) 
		and: [ ((self selector beginsWith: 'test') or: [ (self selector beginsWith: 'should')]) 
		and: [ self numArgs isZero ] ]! !


!SmalltalkEditor methodsFor: 'tdd' stamp: 'HernanWilkinson 1/10/2017 19:35:14'!
acceptAndTest
	
	^self acceptThenTestMethodAndSuite: [ :aMethod | self testSuiteOf: aMethod methodClass ]! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HernanWilkinson 1/10/2017 19:35:34'!
acceptAndTestAll
	
	^self acceptThenTestMethodAndSuite: [ :aMethod | self testSuiteForCategoryOf: aMethod methodClass ]
	
	! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HernanWilkinson 1/10/2017 19:04:06'!
acceptThenTestMethodAndSuite: aSuiteBuilder
	
	| potencialTestMethod |
	
	self acceptContents ifFalse: [ ^false ].
	
	potencialTestMethod _ model textProvider currentCompiledMethod.
	self runAndDebuggIfNecessary: potencialTestMethod.
	^(self runTestSuite: (aSuiteBuilder value: potencialTestMethod)) hasPassed
	! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HernanWilkinson 1/10/2017 18:50:03'!
flashWith: aColor

	^morph flashWith: aColor! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HernanWilkinson 1/10/2017 18:50:21'!
flashWithGreen

	^self flashWith: Color green

	
	
	! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HernanWilkinson 1/10/2017 19:03:07'!
runAndDebuggIfNecessary: aPotentialTestMethod

	aPotentialTestMethod isTestMethod ifTrue: [
		aPotentialTestMethod methodClass debug: aPotentialTestMethod selector ]! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HernanWilkinson 1/10/2017 19:04:42'!
runTestSuite: aTestSuite

	| suiteRunResult |
	 
	suiteRunResult _ aTestSuite run.
	suiteRunResult hasPassed 
		ifTrue: [self flashWithGreen ]
		ifFalse: [ suiteRunResult forDebuggingAndInspection inspect ].
		
	^suiteRunResult 

	
	
	! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HernanWilkinson 1/10/2017 19:35:52'!
testSuiteForCategoryOf: aClass

	| testCaseClasses |
	
	testCaseClasses _ (SystemOrganization listAtCategoryNamed: aClass category)
		collect: [ :aClassName | Smalltalk classNamed: aClassName ]
		thenSelect: [ :aClassInCategory | aClassInCategory is: #TestCaseClass ].
		
	
	^testCaseClasses
		inject: (TestSuite named: 'Test of Category ', aClass category)
		into: [ :suite :testCaseClass | testCaseClass addToSuiteFromSelectors: suite ]! !

!SmalltalkEditor methodsFor: 'tdd' stamp: 'HernanWilkinson 1/10/2017 19:34:58'!
testSuiteOf: aPotentialTestCaseClass

	^(aPotentialTestCaseClass is: #TestCaseClass)
		ifTrue: [ aPotentialTestCaseClass buildSuite ]
		ifFalse: [ TestSuite named: 'Tests of ', aPotentialTestCaseClass name ]! !


!DisplayScreen methodsFor: 'displaying' stamp: 'HernanWilkinson 1/10/2017 18:45:48'!
flash: aRectangle with: aColor

	self fill: aRectangle fillColor: aColor.
	self forceToScreen: aRectangle.
	(Delay forMilliseconds: 100) wait.
	self fill: aRectangle fillColor: aColor.
	self forceToScreen: aRectangle! !


!Morph methodsFor: 'macpal' stamp: 'HernanWilkinson 1/10/2017 18:49:44'!
flashWith: aColor

	self morphBoundsInWorld ifNotNil: [ :r | Display flash: r with: aColor ]! !


!InnerTextMorph methodsFor: 'editing' stamp: 'HernanWilkinson 1/10/2017 15:56:51'!
confirmAcceptAnyway

	^ self confirm: 
'Caution!! Contents were saved
elsewhere since you started
editing them here.  Accept anyway?'! !


!TestCase class methodsFor: 'Testing' stamp: 'HernanWilkinson 1/10/2017 16:29:48'!
is: aSymbol

	^aSymbol == #TestCaseClass or: [ super is: aSymbol ]! !


!TestResult methodsFor: 'Inspecting' stamp: 'HernanWilkinson 1/10/2017 16:33:03'!
forDebuggingAndInspection

	^TestResultForDebuggingAndInspection on: self! !


!TestResultForDebuggingAndInspection methodsFor: 'initialization' stamp: 'HernanWilkinson 1/10/2017 16:34:56'!
initializeOn: aTestResult

	testResult _ aTestResult! !

!TestResultForDebuggingAndInspection methodsFor: 'printing' stamp: 'HernanWilkinson 1/10/2017 17:49:59'!
print: aCollectionOfTestCases startingWith: header on: aStream
	
	aCollectionOfTestCases
		do: [ :aTestCase |
			aStream 
				nextPutAll: header;
				space;
				nextPutAll: aTestCase class name;
				nextPutAll: ' debug: #';
				nextPutAll: aTestCase selector ]
		separatedBy: [ aStream newLine ].
		
	! !

!TestResultForDebuggingAndInspection methodsFor: 'printing' stamp: 'HernanWilkinson 1/10/2017 17:49:05'!
printOn: aStream

	aStream print: testResult.
	aStream newLine.
	
	self print: testResult errors startingWith: '"E"' on: aStream.
	self print: testResult failures startingWith: '"F"' on: aStream.
	! !

!TestResultForDebuggingAndInspection methodsFor: 'running' stamp: 'HernanWilkinson 1/10/2017 18:14:09'!
reRun

	| suite |
	 
	suite _ TestSuite new.
	suite addTests: testResult tests.
	
	testResult _ suite run.! !


!TestResultForDebuggingAndInspection class methodsFor: 'instance creation' stamp: 'HernanWilkinson 1/10/2017 16:34:28'!
on: aTestResult

	^self new initializeOn: aTestResult! !


!TextEditor methodsFor: 'menu messages' stamp: 'HernanWilkinson 1/10/2017 16:00:24'!
acceptContents
	"Save the current text of the text being edited as the current acceptable version for purposes of canceling.  Allow my morph to take appropriate action"
	^morph acceptContents! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'HernanWilkinson 1/10/2017 19:58:01'!
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


!InspectorWindow methodsFor: 'GUI building' stamp: 'HernanWilkinson 1/10/2017 17:47:44'!
initialExtent

	^600@325! !


!InnerTextMorph methodsFor: 'editing' stamp: 'HernanWilkinson 1/10/2017 19:46:39'!
acceptContents
	"The message is sent when the user hits return or Cmd-S.
	Accept the current contents and end editing."
	"Inform the model of text to be accepted, and return true if OK."

	| accepted prevSelection prevScrollValue |
	
	prevSelection _ self editor selectionInterval copy.
	prevScrollValue _ owner verticalScrollBar value.
	
	(self canDiscardEdits and: [(self hasProperty: #alwaysAccept) not]) ifTrue: [self flash. ^true].
	(self hasEditingConflicts and: [ self confirmAcceptAnyway not ]) ifTrue: [self flash. ^false].
	
	accepted _ model acceptContentsFrom: owner.
	"During the step for the browser, updatePaneIfNeeded is called, and 
		invariably resets the contents of the code-holding PluggableTextMorph
		at that time, resetting the cursor position and scroller in the process.
		The following line forces that update without waiting for the step,
 		then restores the cursor and scrollbar"
	
	"some implementors of acceptContentsFrom: answer self :("
	^accepted == true 
		ifTrue: [
			model refetch.
			self editor selectFrom: prevSelection first to: prevSelection last.
			WorldState addDeferredUIMessage: [
				self world ifNotNil: [ :w | w activeHand newKeyboardFocus: self ].
				owner verticalScrollBar setValue: prevScrollValue].
			true]
		ifFalse: [ false ]! !


!SmalltalkEditor reorganize!
('editing keys' acceptAndTest: acceptAndTestAll: browseIt: debugIt: doIt: exploreIt: fileItIn: implementorsOfIt: inspectIt: methodStringsContainingit: pasteInitials: printIt: referencesToIt: save: sendersOfIt:)
('do-its' compileSelectionFor:in: debug:receiver:in: debugIt doIt evaluateSelectionAndDo:ifFail:profiled: exploreIt inspectIt printIt profileIt)
('menu messages' browseClassFromIt browseIt classCommentsContainingIt explain fileItIn getMenu2 implementorsOfIt methodSourceContainingIt methodStringsContainingit referencesToIt selectedSelector selectedSymbol sendersOfIt)
('typing/selecting keys' argAdvance: displayIfFalse: displayIfTrue: newLine:)
('explain' explainAnySel: explainChar: explainClass: explainCtxt: explainDelimitor: explainGlobal: explainInst: explainMySel: explainNumber: explainPartSel: explainTemp:)
('new selection' nextTokenFrom:direction: notify:at:in: selectPrecedingIdentifier selectWord)
('private' codeProvider)
('accessing-selection' selection)
('events' clickAndHalf)
('tdd' acceptAndTest acceptAndTestAll acceptThenTestMethodAndSuite: flashWith: flashWithGreen runAndDebuggIfNecessary: runTestSuite: testSuiteForCategoryOf: testSuiteOf:)
!


!TestResult reorganize!
('Accessing' correctCount defects errorCount errors failureCount failures passed passedCount runCount tests)
('Testing' hasErrors hasFailures hasPassed isError: isFailure: isPassed:)
('Printing' printOn:)
('Running' runCase:)
('Init / Release')
('Inspecting' forDebuggingAndInspection)
!


!TestResultForDebuggingAndInspection reorganize!
('initialization' initializeOn:)
('printing' print:startingWith:on: printOn:)
('running' reRun)
!

