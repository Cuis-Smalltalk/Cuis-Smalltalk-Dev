'From Cuis 5.0 [latest update: #3877] on 16 September 2019 at 7:37:07 pm'!
!classDefinition: #TestResultConsolePrinter category: #'System-Support'!
Object subclass: #TestResultConsolePrinter
	instanceVariableNames: 'outputStream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!
!classDefinition: 'TestResultConsolePrinter class' category: #'System-Support'!
TestResultConsolePrinter class
	instanceVariableNames: 'stdOutCommandLine'!

!TestResultConsolePrinter methodsFor: 'initialization' stamp: 'FJG 9/15/2019 03:44:31'!
initializeWith: aWriteStream

	outputStream _ aWriteStream
	! !

!TestResultConsolePrinter methodsFor: 'printing' stamp: 'FJG 9/15/2019 03:48:28'!
print: aString

	outputStream nextPutAll: aString; newLine! !

!TestResultConsolePrinter methodsFor: 'printing' stamp: 'GC 9/16/2019 19:07:31'!
printError: aTestCase because: anExceptionSignal
	| exception |
	
	exception _ anExceptionSignal exception.
	
	self printInRed: ('&#16r274C; {1} has errored because: {2}' format: {aTestCase asString . exception asString })! !

!TestResultConsolePrinter methodsFor: 'printing' stamp: 'GC 9/16/2019 19:07:16'!
printFailed: aTestCase because: aCause

	self printInRed: ('&#16r274C; {1} has failed because: {2}' format: {aTestCase asString . aCause asString })! !

!TestResultConsolePrinter methodsFor: 'printing' stamp: 'GC 9/16/2019 19:14:03'!
printInGreen: aString

	outputStream nextPutAll: Character escape asString, '[32m'.
	self print: aString.	
	outputStream nextPutAll: Character escape asString, '[0m'.
	outputStream flush! !

!TestResultConsolePrinter methodsFor: 'printing' stamp: 'GC 9/16/2019 19:13:34'!
printInRed: aString

	outputStream nextPutAll: Character escape asString, '[31m'.
	self print: aString.	
	outputStream nextPutAll: Character escape asString, '[0m'.
	outputStream flush! !

!TestResultConsolePrinter methodsFor: 'printing' stamp: 'GC 9/16/2019 19:12:25'!
printPassed: aTestCase

	self printInGreen: '&#16r2713; ', aTestCase asString! !

!TestResultConsolePrinter methodsFor: 'printing' stamp: 'GC 9/16/2019 19:07:59'!
printReport: aTestResult
	| failuresCount |
	
	self print: aTestResult asString.
	
	failuresCount _ (aTestResult errors, aTestResult failures) size.
	failuresCount isZero ifTrue: [
		self
			printInGreen: '--------';
		      printInGreen: 'SUCCESS';
			printInGreen: '--------'.
	] ifFalse: [
		self printInRed: '--------';
			 printInRed: failuresCount asString, ' FAILURE(S)';
			 printInRed: '--------'.
		aTestResult errors do: [:aTestCase |
			self printError: aTestCase because: aTestCase raisedError
		].
		aTestResult failures do: [:aTestCase |
			self printFailed: aTestCase because: aTestCase failureString
		]
	]! !


!TestResultConsolePrinter class methodsFor: 'instance creation' stamp: 'FJG 9/15/2019 01:41:17'!
stdOutCommandLine

	^ stdOutCommandLine ifNil: [
		stdOutCommandLine _ self new initializeWith: StdIOWriteStream stdout; yourself
	]! !

!TestResultConsolePrinter class methodsFor: 'stdout printing' stamp: 'FJG 9/15/2019 03:24:22'!
printError: aTestCase because: anExceptionSignal

	self stdOutCommandLine printError: aTestCase because: anExceptionSignal! !

!TestResultConsolePrinter class methodsFor: 'stdout printing' stamp: 'FJG 9/15/2019 03:24:42'!
printFailed: aTestCase because: anException

	self stdOutCommandLine printFailed: aTestCase because: anException! !

!TestResultConsolePrinter class methodsFor: 'stdout printing' stamp: 'FJG 9/15/2019 03:23:05'!
printPassed: aTestCase

	self stdOutCommandLine printPassed: aTestCase! !

!TestResultConsolePrinter class methodsFor: 'stdout printing' stamp: 'FJG 9/15/2019 03:22:14'!
printReport: aTestResult

	self stdOutCommandLine printReport: aTestResult! !


!TestResult methodsFor: 'running' stamp: 'FJG 9/15/2019 03:22:19'!
printReport

	TestResultConsolePrinter printReport: self! !


!TestResult methodsFor: 'running' stamp: 'FJG 9/15/2019 03:26:17'!
reportError: aTestCase because: anExceptionSignal
	
	TestResultConsolePrinter printError: aTestCase because: anExceptionSignal! !

!TestResult methodsFor: 'running' stamp: 'FJG 9/15/2019 03:25:11'!
reportFailed: aTestCase because: anException
	
	TestResultConsolePrinter printFailed: aTestCase because: anException! !

!TestResult methodsFor: 'running' stamp: 'FJG 9/15/2019 03:23:35'!
reportPassed: aTestCase
	
	TestResultConsolePrinter printPassed: aTestCase! !


!TestResult reorganize!
('accessing' correctCount defects errorCount errors failureCount failures passed passedCount removeFromDefectsAndAddToPassed: runCount tests)
('testing' hasErrors hasFailures hasPassed isError: isFailure: isPassed:)
('printing' printOn:)
('running' printReport reportError:because: reportFailed:because: reportPassed: runCase:)
('logging' reportAboutToRun:)
!

