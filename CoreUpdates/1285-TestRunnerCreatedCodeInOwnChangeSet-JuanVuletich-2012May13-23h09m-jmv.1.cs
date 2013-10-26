'From Cuis 4.0 of 21 April 2012 [latest update: #1284] on 13 May 2012 at 11:12:09 pm'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 5/13/2012 23:01'!
notInstallOrTestRun

	^Installing isNil or: [
		(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name ~= 'TestRunner' ]]
	
	"
	(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name = 'TestRunner' ]			-> Test
	(Installing beginsWith: Install-')																		-> Install
	Installing isNil 																						-> Normal
	(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name ~= 'TestRunner' ]			-> Normal
	"! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 5/13/2012 22:46'!
runningTest: aTestName do: aBlock

	Installing _ 'RunningTest-', aTestName.
	aBlock ensure: [ Installing _ nil ]! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 5/13/2012 23:10'!
changeSetForBaseSystem

	| csName numberToUse |
	self notInstallOrTestRun
		ifTrue: [
			numberToUse _ self currentBaseCSNumber.
			ChangeSorter allChangeSets
				detect: [ :any | any name initialIntegerOrNil = numberToUse ]
				ifFound: [ :existing | ^existing ]
				ifNone: [
					csName _ (self baseSystemNameFor: numberToUse),
						(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]).
					^ChangeSorter existingOrNewChangeSetNamed: csName forBaseSystem: true ]]

		ifFalse: [
			csName _ 'Affects-BaseSystem--', Installing.
			^ChangeSorter existingOrNewChangeSetNamed: csName forBaseSystem: false
			"Changes are for the base system, but are not user's own changes..." ]! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 5/13/2012 23:11'!
changeSetForPackage: aCodePackage

	| csName |
	aCodePackage ifNil: [
		^self changeSetForBaseSystem ].
	csName _ 	self notInstallOrTestRun
		ifTrue: [ 'UnsavedChangesTo-', aCodePackage name ]
		ifFalse: [
			Installing = ('Install-', aCodePackage packageName)
				ifTrue: [ Installing ]
				ifFalse: [ 'Affects-', aCodePackage name, '--', Installing ]].
	^ChangeSorter existingOrNewChangeSetNamed: csName forBaseSystem: false! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 5/13/2012 22:42'!
installing: aCodePackageOrChangeSetName do: aBlock

	| currentCS currentCSNumber newHighestUpdate |
	Installing _ 'Install-', aCodePackageOrChangeSetName.
	aBlock ensure: [ Installing _ nil ].

	SystemVersion current registerUpdate: aCodePackageOrChangeSetName.
	newHighestUpdate _  SystemVersion current highestUpdate.

	currentCSNumber _ self currentBaseCSNumber.
	currentCS _ self changeSetForBaseSystem.
	currentCS isEmpty ifTrue: [
		ChangeSorter removeChangeSet: currentCS.
		currentCSNumber > newHighestUpdate ifFalse: [
			CurrentBaseCSNumber _ newHighestUpdate + 1 ]]! !


!TestCase methodsFor: 'Running' stamp: 'jmv 5/13/2012 23:05'!
run: aResult
	ChangeSet
		runningTest: self printString
		do: [ aResult runCase: self ]
			! !


!TestRunner methodsFor: 'processing' stamp: 'jmv 5/13/2012 22:51'!
runSuite: suite
	running ifNotNil: [ ^self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	runSemaphore initSignals.
	running _ [
            [ result _ suite run ]
	            ensure: [
		            running _ nil.
				suite removeDependent: self.
				runSemaphore signal.
				WorldState addDeferredUIMessage: [
					self updateWindow: result.
			      	self changed: #runTests.
			      	self changed: #runOneTest.
				].
	            ].
     ] newProcess.
	self runWindow.
      self changed: #runTests.
      self changed: #runOneTest.
      running
		name: 'TestRunner';
	      priority: Processor userBackgroundPriority;
	      resume.
! !

!TestRunner methodsFor: 'processing' stamp: 'jmv 5/13/2012 22:51'!
runSuiteProfiled: suite
	running ifNotNil: [ ^self inform: 'already running' ].
	suite addDependent: self.
	totalTests _ suite tests size.
	completedTests _ 0.
	runSemaphore initSignals.
	running _ [
            [ result _ MessageTally spyOn: [suite run] ]
	            ensure: [
		            running _ nil.
				suite removeDependent: self.
				runSemaphore signal.
				WorldState addDeferredUIMessage: [
					self updateWindow: result.
			      	self changed: #runTests.
			      	self changed: #runOneTest.
				].
	            ].
     ] newProcess.
	self runWindow.
      self changed: #runTests.
      self changed: #runOneTest.
      running
		name: 'TestRunner';
	      priority: Processor userBackgroundPriority;
	      resume.
! !

