'From Cuis 4.2 of 25 July 2013 [latest update: #2753] on 3 May 2016 at 11:58:13.658764 am'!
!classDefinition: #CodePackageList category: #'Tools-Packages'!
ActiveModel subclass: #CodePackageList
	instanceVariableNames: 'packages selection'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Packages'!
!classDefinition: #PackageRequirementsList category: #'Tools-Packages'!
ActiveModel subclass: #PackageRequirementsList
	instanceVariableNames: 'codePackageList requirements selection'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Packages'!
!classDefinition: #TestRunner category: #'Tools-Testing'!
ActiveModel subclass: #TestRunner
	instanceVariableNames: 'result details passFail failures errors tests lastPass selectedFailureTest selectedErrorTest selectedSuite filter selectedSuites running runSemaphore completedTests totalTests progressLabel'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #SinglePackageBrowser category: #'Tools-Packages'!
Browser subclass: #SinglePackageBrowser
	instanceVariableNames: 'package'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Packages'!
!classDefinition: #TestCaseDebugger category: #'Tools-Testing'!
Debugger subclass: #TestCaseDebugger
	instanceVariableNames: 'doneSemaphore'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #AutoCompleter category: #'Tools-Autocompletion'!
Object subclass: #AutoCompleter
	instanceVariableNames: 'model textMorph menuMorph position entries prefix'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
!classDefinition: #SmalltalkCompleter category: #'Tools-Autocompletion'!
AutoCompleter subclass: #SmalltalkCompleter
	instanceVariableNames: 'parser'
	classVariableNames: 'AccessLock Selectors'
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
!classDefinition: #CodePackageFile category: #'System-Package Support'!
CodeFile subclass: #CodePackageFile
	instanceVariableNames: 'packageName description classesToRemove methodsToRemove featureSpec'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #CodePackage category: #'System-Package Support'!
Object subclass: #CodePackage
	instanceVariableNames: 'packageName methodCategoryPrefix fullFileName hasUnsavedChanges description sourceSystem featureSpec'
	classVariableNames: 'InstalledPackages'
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #DifferenceFinder category: #'Tools-Code Differ'!
Object subclass: #DifferenceFinder
	instanceVariableNames: 'base case x y map tally matrix differences'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Code Differ'!
!classDefinition: #FeatureRequirementUnsatisfied category: #'System-Package Support'!
Error subclass: #FeatureRequirementUnsatisfied
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #TestFailure category: #'Tools-Testing'!
Exception subclass: #TestFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #ResumableTestFailure category: #'Tools-Testing'!
TestFailure subclass: #ResumableTestFailure
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #Feature category: #'System-Package Support'!
Object subclass: #Feature
	instanceVariableNames: 'name version revision'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minVersion minRevision maxVersion pathName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FeatureSpec category: #'System-Package Support'!
Object subclass: #FeatureSpec
	instanceVariableNames: 'provides requires'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
!classDefinition: #FmEntry category: #'System-FileMan-Core'!
Object subclass: #FmEntry
	instanceVariableNames: 'drive pathComponents parent name creationTime modificationTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-FileMan-Core'!
!classDefinition: #FmDirectoryEntry category: #'System-FileMan-Core'!
FmEntry subclass: #FmDirectoryEntry
	instanceVariableNames: 'children'
	classVariableNames: 'CurrentDirectory ImageDirectory VMDirectory'
	poolDictionaries: ''
	category: 'System-FileMan-Core'!
!classDefinition: #FmFileEntry category: #'System-FileMan-Core'!
FmEntry subclass: #FmFileEntry
	instanceVariableNames: 'fileSize'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-FileMan-Core'!
!classDefinition: #FmFileIOAccessor category: #'System-FileMan-Core'!
Object subclass: #FmFileIOAccessor
	instanceVariableNames: 'slash drives'
	classVariableNames: 'Default'
	poolDictionaries: ''
	category: 'System-FileMan-Core'!
!classDefinition: #AutoCompleterMorph category: #'Tools-Autocompletion'!
BorderedRectMorph subclass: #AutoCompleterMorph
	instanceVariableNames: 'completer selected firstVisible itemHeight lastActivity'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Autocompletion'!
!classDefinition: #CodePackageListWindow category: #'Morphic-Tools'!
SystemWindow subclass: #CodePackageListWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tools'!
!classDefinition: #SHParserST80 category: #'Tools-Syntax Highlighting'!
Object subclass: #SHParserST80
	instanceVariableNames: 'classOrMetaClass source workspace arguments sourcePosition currentToken currentTokenFirst temporaries instanceVariables errorBlock currentTokenSourcePosition blockDepth bracketDepth ranges'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
!classDefinition: #SHRange category: #'Tools-Syntax Highlighting'!
Object subclass: #SHRange
	instanceVariableNames: 'start end type'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
!classDefinition: #SHTextStyler category: #'Tools-Syntax Highlighting'!
Object subclass: #SHTextStyler
	instanceVariableNames: 'backgroundProcess formattedText textModel mutex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
!classDefinition: #SHTextStylerST80 category: #'Tools-Syntax Highlighting'!
SHTextStyler subclass: #SHTextStylerST80
	instanceVariableNames: 'classOrMetaClass workspace parser disableFormatAndConvert'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Syntax Highlighting'!
!classDefinition: #SUnitNameResolver category: #'Tools-Testing'!
Object subclass: #SUnitNameResolver
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #SequenceDifference category: #'Tools-Code Differ'!
Object subclass: #SequenceDifference
	instanceVariableNames: 'x y lcs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Code Differ'!
!classDefinition: #TestCase category: #'Tools-Testing'!
Object subclass: #TestCase
	instanceVariableNames: 'testSelector failureString raisedError'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #TestResource category: #'Tools-Testing'!
Object subclass: #TestResource
	instanceVariableNames: 'name description'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #TestResult category: #'Tools-Testing'!
Object subclass: #TestResult
	instanceVariableNames: 'failures errors passed'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #TestSuite category: #'Tools-Testing'!
Object subclass: #TestSuite
	instanceVariableNames: 'tests resources name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
