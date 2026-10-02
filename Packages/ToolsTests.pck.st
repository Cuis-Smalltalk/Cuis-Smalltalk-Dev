'From Cuis7.9 [latest update: #8262] on 2 October 2026 at 11:52:25 am'!
'Description '!
!provides: 'ToolsTests' 1 3!
!requires: 'DynamicallyCodeCreatingTestSupport' 1 0 nil!
SystemOrganization addCategory: #'ToolsTests-Browser'!
SystemOrganization addCategory: #'ToolsTests-Changes'!
SystemOrganization addCategory: #'ToolsTests-Syntax Highlighting'!
SystemOrganization addCategory: #'ToolsTests-Debugger'!
SystemOrganization addCategory: #'ToolsTests-Autocompletion'!
SystemOrganization addCategory: #'ToolsTests-Taskbar'!
SystemOrganization addCategory: #'ToolsTests-Testing'!
SystemOrganization addCategory: #'ToolsTests-Code Differ'!


!classDefinition: #ChangesTest category: #'ToolsTests-Changes'!
DynamicallyCodeCreatingTest subclass: #ChangesTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ToolsTests-Changes'!
!classDefinition: 'ChangesTest class' category: #'ToolsTests-Changes'!
ChangesTest class
	instanceVariableNames: ''!

!classDefinition: #SmalltalkCompleterTest category: #'ToolsTests-Autocompletion'!
TestCase subclass: #SmalltalkCompleterTest
	instanceVariableNames: 'instanceVariable1 classToBrowse selectorToBrowse'
	classVariableNames: 'ClassVariableForTesting'
	poolDictionaries: 'poolDictionaryForTesting'
	category: 'ToolsTests-Autocompletion'!
!classDefinition: 'SmalltalkCompleterTest class' category: #'ToolsTests-Autocompletion'!
SmalltalkCompleterTest class
	instanceVariableNames: ''!

!classDefinition: #DynamicTypingSmalltalkCompleterTest category: #'ToolsTests-Autocompletion'!
SmalltalkCompleterTest subclass: #DynamicTypingSmalltalkCompleterTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ToolsTests-Autocompletion'!
!classDefinition: 'DynamicTypingSmalltalkCompleterTest class' category: #'ToolsTests-Autocompletion'!
DynamicTypingSmalltalkCompleterTest class
	instanceVariableNames: ''!

!classDefinition: #MethodReferenceTest category: #'ToolsTests-Browser'!
TestCase subclass: #MethodReferenceTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ToolsTests-Browser'!
!classDefinition: 'MethodReferenceTest class' category: #'ToolsTests-Browser'!
MethodReferenceTest class
	instanceVariableNames: ''!

!classDefinition: #DifferenceFinderTest category: #'ToolsTests-Code Differ'!
TestCase subclass: #DifferenceFinderTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ToolsTests-Code Differ'!
!classDefinition: 'DifferenceFinderTest class' category: #'ToolsTests-Code Differ'!
DifferenceFinderTest class
	instanceVariableNames: ''!

!classDefinition: #DebuggerTest category: #'ToolsTests-Debugger'!
TestCase subclass: #DebuggerTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ToolsTests-Debugger'!
!classDefinition: 'DebuggerTest class' category: #'ToolsTests-Debugger'!
DebuggerTest class
	instanceVariableNames: ''!

!classDefinition: #QuickReturnDebuggingTest category: #'ToolsTests-Debugger'!
TestCase subclass: #QuickReturnDebuggingTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ToolsTests-Debugger'!
!classDefinition: 'QuickReturnDebuggingTest class' category: #'ToolsTests-Debugger'!
QuickReturnDebuggingTest class
	instanceVariableNames: ''!

!classDefinition: #SHST80RangeTypeTest category: #'ToolsTests-Syntax Highlighting'!
TestCase subclass: #SHST80RangeTypeTest
	instanceVariableNames: ''
	classVariableNames: 'ClassVar1'
	poolDictionaries: ''
	category: 'ToolsTests-Syntax Highlighting'!
!classDefinition: 'SHST80RangeTypeTest class' category: #'ToolsTests-Syntax Highlighting'!
SHST80RangeTypeTest class
	instanceVariableNames: ''!

!classDefinition: #TaskbarTest category: #'ToolsTests-Taskbar'!
TestCase subclass: #TaskbarTest
	instanceVariableNames: 'taskbar needsDelete'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ToolsTests-Taskbar'!
!classDefinition: 'TaskbarTest class' category: #'ToolsTests-Taskbar'!
TaskbarTest class
	instanceVariableNames: ''!

!classDefinition: #TestCaseTest category: #'ToolsTests-Testing'!
TestCase subclass: #TestCaseTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ToolsTests-Testing'!
!classDefinition: 'TestCaseTest class' category: #'ToolsTests-Testing'!
TestCaseTest class
	instanceVariableNames: ''!

!classDefinition: #TestSuiteTest category: #'ToolsTests-Testing'!
TestCase subclass: #TestSuiteTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ToolsTests-Testing'!
!classDefinition: 'TestSuiteTest class' category: #'ToolsTests-Testing'!
TestSuiteTest class
	instanceVariableNames: ''!


!TaskbarTest commentStamp: '<historical>' prior: 0!
Tests for the Taskbar.!

!ChangesTest methodsFor: 'assertions' stamp: 'jmv 31/Jul/2023, 4:35:38 pm (UTC)'!
assertIsLogged: aString times: stringCount and: aBlock

	| logContent startPosition indexOfSubstring |

	logContent := self userChangesForTestsFile textContents.
 
	startPosition := 1.
	stringCount <= 0
		ifTrue: [ self deny: (logContent includesSubString: aString) ]
		ifFalse:[
			stringCount timesRepeat: [
				indexOfSubstring := logContent findString: aString startingAt: startPosition.
				self assert: indexOfSubstring > 0.
				startPosition := indexOfSubstring + 1 ].
			indexOfSubstring := logContent findString: aString startingAt: startPosition.
			self assert: indexOfSubstring equals: 0 ].
		
	aBlock value: logContent value: startPosition.
	

	! !

!ChangesTest methodsFor: 'assertions' stamp: 'HAW 1/Nov/2019, 9:03:04 pm (UTC)'!
assertIsLoggedOnce: aString

	self assertIsLoggedOnce: aString and: [:logContents :nextPosition | ] ! !

!ChangesTest methodsFor: 'assertions' stamp: 'HAW 1/Nov/2019, 9:02:33 pm (UTC)'!
assertIsLoggedOnce: aString and: aBlock

	self assertIsLogged: aString times: 1 and: aBlock! !

!ChangesTest methodsFor: 'assertions' stamp: 'HAW 1/Nov/2019, 9:03:30 pm (UTC)'!
assertIsLoggedTwice: aString

	self assertIsLogged: aString times: 2 and: [:logContents :nextPosition | ] ! !

!ChangesTest methodsFor: 'user changes' stamp: 'jmv 16/May/2022, 1:26:36 pm (UTC)'!
changeUserChangesFileWhile: aBlock

	^ Preferences
		withTemporaryValue: self userChangesForTestsFileExtension
		of: #userChangesFileNameExtension
		do: aBlock.! !

!ChangesTest methodsFor: 'user changes' stamp: 'jmv 15/Dec/2023, 9:46:22 pm (UTC)'!
scanChangesFromFile

	| fileStream changeList |

	fileStream := (DirectoryEntry userChangesDirectory // Smalltalk userChangesFileName) readStream.
	changeList := ChangeList new scanFile: fileStream from: 0 to: fileStream size.

	^ changeList changeList.
! !

!ChangesTest methodsFor: 'user changes' stamp: 'jmv 2/Jul/2026, 7:18:42 pm (UTC)'!
userChangesForTestsFile

	^DirectoryEntry userChangesDirectory // 
	((FileIOAccessor default baseNameFor: Smalltalk imageName pathAndLocalName second),
	(Utilities authorInitialsPerSe ifNotNil: [ :ai | '-', ai] ifNil: ['']),
	self userChangesForTestsFileExtension).! !

!ChangesTest methodsFor: 'user changes' stamp: 'HAW 1/Nov/2019, 9:08:04 pm (UTC)'!
userChangesForTestsFileExtension

	^'.test.changes' ! !

!ChangesTest methodsFor: 'setUp/tearDown' stamp: 'jmv 26/Aug/2026, 1:06:00 am (UTC)'!
setUp
	self userChangesForTestsFile delete.
	super setUp.! !

!ChangesTest methodsFor: 'setUp/tearDown' stamp: 'HAW 17/Dec/2019, 2:36:27 pm (UTC)'!
tearDown

	self userChangesForTestsFile delete.
	super tearDown ! !

!ChangesTest methodsFor: 'test data' stamp: 'HAW 17/Dec/2019, 2:37:01 pm (UTC)'!
createTestDataClass

	^ self createClassNamed: self testDataClassName 
		! !

!ChangesTest methodsFor: 'test data' stamp: 'HAW 1/Nov/2019, 9:08:21 pm (UTC)'!
testDataClassName

	^ #LogChangesTestClass__! !

!ChangesTest methodsFor: 'scan tests' stamp: 'jmv 30/Apr/2026, 9:02:02 pm (UTC)'!
test01ScanNewClassChange

	| newClass newClassChange |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClassChange := self scanChangesFromFile last.

		self assert: newClassChange isClassDefinition.
		self assert: newClassChange changeClass equals: newClass.
		self deny: newClassChange stamp isNil.
	] ! !

!ChangesTest methodsFor: 'scan tests' stamp: 'jmv 30/Apr/2026, 9:02:47 pm (UTC)'!
test02ScanNewMethodChange

	| newMethodChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.

		newMethodChange := self scanChangesFromFile last.

		self assert: newMethodChange isMethodDefinition.
		self assert: newMethodChange methodSelector equals: #m1.
		self assert: newMethodChange changeClass equals: newClass.
		self deny: newMethodChange stamp isNil ] ! !

!ChangesTest methodsFor: 'scan tests' stamp: 'jmv 30/Apr/2026, 9:02:59 pm (UTC)'!
test03ScanMethodModifiedChange

	| methodModifiedChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.
		newClass compile: 'm1 ^ 2' classified: 'a-category'.

		methodModifiedChange := self scanChangesFromFile last.

		self assert: methodModifiedChange isMethodDefinition.
		self assert: methodModifiedChange methodSelector equals: #m1.
		self assert: methodModifiedChange changeClass equals: newClass.
		self deny: methodModifiedChange stamp isNil ]
! !

!ChangesTest methodsFor: 'scan tests' stamp: 'HAW 1/Nov/2019, 9:08:37 pm (UTC)'!
test04ScanMethodRemovalChange

	| methodRemovalChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.
		newClass removeSelector: #m1.

		methodRemovalChange := self scanChangesFromFile last.

		self assert: methodRemovalChange isMethodDeletion.
		self assert: methodRemovalChange methodSelector equals: #m1.
		self deny: methodRemovalChange stamp isNil ]
! !

!ChangesTest methodsFor: 'scan tests' stamp: 'jmv 30/Apr/2026, 9:05:44 pm (UTC)'!
test05ScanClassRemovalChange

	| classRemovalChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass removeFromSystem.

		classRemovalChange := self scanChangesFromFile last.

		self assert: classRemovalChange isClassDeletion.
		self assert: classRemovalChange changeClassName equals: self testDataClassName.
		self assert: classRemovalChange isClassDeletion.
		self deny: classRemovalChange stamp isNil ]

! !

!ChangesTest methodsFor: 'scan tests' stamp: 'jmv 4/May/2026, 5:15:42 pm (UTC)'!
test06ScanClassRenamedChange

	| classRenamedChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass rename: #NewTestClass__.

		classRenamedChange := self scanChangesFromFile last.

		self assert: classRenamedChange isClassRenamed.
		self assert: classRenamedChange changeClassName equals: self testDataClassName.
		self assert: classRenamedChange newClassName equals: #NewTestClass__.
		self deny: classRenamedChange stamp isNil. ]
! !

!ChangesTest methodsFor: 'log tests' stamp: 'jmv 26/Aug/2026, 12:45:14 am (UTC)'!
test01AddingAClassShouldBeLogged
	" Log format expected:

	!!classDefinition: #ClassName category: 'Category' stamp: changeStamp!!
	Object subclass: #ClassName
		instanceVariableNames: '...'
		classVariableNames: '...'
		poolDictionaries: '....'
		category: 'Category'!!
	"
	| newClass |
	
	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.

		self assertIsLoggedOnce: '!!classDefinition: #', self testDataClassName, ' category: #', self classCategoryOfTestData,' stamp:'.
		self assertIsLoggedOnce: newClass definition ] ! !

!ChangesTest methodsFor: 'log tests' stamp: 'HAW 1/Nov/2019, 9:08:48 pm (UTC)'!
test02AddingANewMethodShouldBeLogged
	" Log format expected:

	!!Class methodsFor: 'category' stamp: 'author stamp'!!
	methodSourceCode
	"
	| aClass |
	
	self changeUserChangesFileWhile: [
		aClass := self createTestDataClass.
		aClass compile: 'm1 ^ 1' classified: 'a-category'.

		self assertIsLoggedOnce: '!!', self testDataClassName, ' methodsFor: ''a-category'' stamp:'.
		self assertIsLoggedOnce: 'm1 ^ 1'] 
! !

!ChangesTest methodsFor: 'log tests' stamp: 'jmv 2/Oct/2026, 2:44:10 pm (UTC)'!
test03ChangingAMethodShouldBeLogged
	" Log format expected:

	!!Class methodsFor: 'category' stamp: 'author stamp'!!
	newMethodSourceCode
	"
	| aClass |
	
	self changeUserChangesFileWhile: [
		aClass := self createTestDataClass.
		aClass compile: 'm1 ^ 1' classified: 'a-category'.
		aClass compile: 'm1 ^ 2' classified: 'a-category'.

		self assertIsLoggedTwice: '!!', self testDataClassName , ' methodsFor: ''a-category'' stamp:'.
		self assertIsLoggedOnce: 'm1 ^ 2' ] ! !

!ChangesTest methodsFor: 'log tests' stamp: 'jmv 31/Jul/2023, 4:36:30 pm (UTC)'!
test04RemovingAMethodShouldBeLogged
	" Log format expected:

	!!methodRemoval: #ClassName selector stamp: changeStamp!!
	methodSourceCode
	"
	| newClass methodSource readStream |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.
		newClass removeSelector: #m1.

		self 
			assertIsLoggedOnce: '!!methodRemoval: ',self testDataClassName, ' #m1 stamp:'
			and: [ :logContents :nextPosition |
				readStream := ReadStream on: logContents.
		 		readStream position: nextPosition.
				readStream nextLine.
				methodSource := readStream nextLine.
		
				self assert: methodSource equals: 'm1 ^ 1!!' ]].! !

!ChangesTest methodsFor: 'log tests' stamp: 'jmv 31/Jul/2023, 4:36:57 pm (UTC)'!
test05RemovingAClassShouldBeLogged
	" Log format expected:

	!!classRemoval: #ClassName stamp: changeStamp!!
	classDefinitionMessage
	"
	| newClass definition loggedDefinition readStream |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		definition := newClass definition.
		newClass removeFromSystem.

		self 
			assertIsLoggedOnce: '!!classRemoval: #', self testDataClassName, ' stamp:'
			and: [ :logContents :nextPosition |
				readStream := ReadStream on: logContents.
		 		readStream position: nextPosition.
				readStream nextLine.
				loggedDefinition := readStream upToEnd.
				
				self assert: loggedDefinition equals: definition,'!!' ]].! !

!ChangesTest methodsFor: 'log tests' stamp: 'MGD 30/Mar/2020, 9:00:58 pm (UTC)'!
test06RenamingAClassShouldBeLogged
	" Log format expected:

	!!classRenamed: #OldName as: #NewName stamp  stamp:!!
	Smalltalk renameClassNamed: #OldName as: #NewName
	"
	| newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass rename: #NewTestClass__.

		self assertIsLoggedOnce: '!!classRenamed: #', self testDataClassName, ' as: #NewTestClass__ stamp:'.
		self assertIsLoggedOnce: 'Smalltalk renameClassNamed: #', self testDataClassName, ' as: #NewTestClass__'.
	]! !

!SmalltalkCompleterTest methodsFor: 'testing' stamp: 'jmv 14/Mar/2025, 2:24:30 pm (UTC)'!
testMessages
	"
	SmalltalkCompleterTest new testMessages
	"
	| fromSmalltalk fromUCompleter |
	fromSmalltalk := Smalltalk allImplementedMessages.
	fromUCompleter := Symbol allSymbols select: [ :s |
		SmalltalkCompleter isThereAnImplementorOf: s].

	"Some tests use #doSilently: when removing classes.
	This means that selectors may be garbage collected and not removed from SmalltalkCompleter."
	"self assert: fromSmalltalk = fromUCompleter asSet."
	self assert: (fromUCompleter includesAllOf: fromSmalltalk).! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 12:56:42 am (UTC)'!
test000_AutocompletesMessagesFor_GlobalVariables_WithSelectorsFromTheirClasses
	
	self
		assertEntriesWhenBrowsing: 'm1 SmalltalkCompleterTest '
		areSelectorsOf: SmalltalkCompleterTest class.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < SmalltalkCompleterTest '
		areUnaryAndBinarySelectorsOf: SmalltalkCompleterTest class.
	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 1:01:49 am (UTC)'!
test001_AutocompletesMessagesFor_Self_WithSelectorsFromTheBrowsedClass
	
	self
		assertEntriesWhenBrowsing: 'm1 self '
		areSelectorsOf: SmalltalkCompleterTest.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < self '
		areUnaryAndBinarySelectorsOf: SmalltalkCompleterTest.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 1:02:19 am (UTC)'!
test002_AutocompletesMessagesFor_Super_WithSelectorsFromTheBrowsedClass
	
	self
		assertEntriesWhenBrowsing: 'm1 super '
		areSelectorsOf: TestCase.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < super '
		areUnaryAndBinarySelectorsOf: TestCase.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 1:02:43 am (UTC)'!
test003_AutocompletesMessagesFor_Super_WithSelectorsForUnknownClassesWhenTheBrowsedClassDoesNotHaveASuperclass
	
	self browseClass: ProtoObject.
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 super '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 < super '.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 1:11:29 am (UTC)'!
test004_AutocompletesMessagesFor_True_WithSelectorsFromTrue
	
	self
		assertEntriesWhenBrowsing: 'm1 true '
		areSelectorsOf: True.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < true '
		areUnaryAndBinarySelectorsOf: True.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 1:12:17 am (UTC)'!
test005_AutocompletesMessagesFor_False_WithSelectorsFromFalse
	
	self
		assertEntriesWhenBrowsing: 'm1 false '
		areSelectorsOf: False.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < false '
		areUnaryAndBinarySelectorsOf: False.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 1:12:12 am (UTC)'!
test006_AutocompletesMessagesFor_Nil_WithSelectorsFromUndefinedObject
	
	self
		assertEntriesWhenBrowsing: 'm1 nil '
		areSelectorsOf: UndefinedObject.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < nil '
		areUnaryAndBinarySelectorsOf: UndefinedObject.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'jmv 5/Jul/2022, 8:26:48 pm (UTC)'!
test007_AutocompletesMessagesFor_Characters_WithSelectorsFromCharacter
	
	self
		assertEntriesWhenBrowsing: 'm1 $a '
		areSelectorsOf: $m class.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < $a '
		areUnaryAndBinarySelectorsOf: $m class.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 1:15:01 am (UTC)'!
test008_AutocompletesMessagesFor_Numbers_WithSelectorsFromTheirClass
	
	self
		assertEntriesWhenBrowsing: 'm1 1 '
		areSelectorsOf: SmallInteger.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < 1 '
		areUnaryAndBinarySelectorsOf: SmallInteger.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'jmv 5/Jul/2022, 8:27:59 pm (UTC)'!
test009_AutocompletesMessagesFor_Strings_WithSelectorsFromString
	
	self
		assertEntriesWhenBrowsing: 'm1 ''a'' '
		areSelectorsOf: 'm' class.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < ''a'' '
		areUnaryAndBinarySelectorsOf: 'm' class	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 3:37:45 am (UTC)'!
test010_AutocompletesMessagesFor_Symbols_WithSelectorsFromSymbol
	
	self
		assertEntriesWhenBrowsing: 'm1 #a '
		areSelectorsOf: Symbol.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < #a '
		areUnaryAndBinarySelectorsOf: Symbol.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 3:38:21 am (UTC)'!
test011_AutocompletesMessagesFor_StringSymbol_WithSelectorsFromSymbol
	
	self
		assertEntriesWhenBrowsing: 'm1 #''a'' '
		areSelectorsOf: Symbol.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < #''a'' '
		areUnaryAndBinarySelectorsOf: Symbol.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 7/May/2020, 3:42:33 am (UTC)'!
test012_AutocompletesMessagesFor_InstanceVariables
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 instanceVariable1 '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 + instanceVariable1 '.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 2/May/2020, 8:42:09 pm (UTC)'!
test013_AutocompletesMessagesFor_MethodArguments_WithSelectorsForUnknownClasses
	
	"I reference to SmalltalkCompleterTest directly and not thru 'self class' because this test has subclasses
	the #m1: and #m2: are defined in SmalltalkCompleterTest - Hernan"
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsingMethodWith: SmalltalkCompleterTest >> #m1:.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsingMethodWith: SmalltalkCompleterTest >> #m2: ! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 1:22:39 am (UTC)'!
test014_AutocompletesMessagesFor_TemporaryVariables_WithSelectorsForUnknownClasses
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 |a| a '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 |a| 1 + a '.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 1:25:42 am (UTC)'!
test015_AutocompletesMessagesFor_BlockArguments_WithSelectorsForUnknownClasses
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ :a | a '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ :a | 1 + a '.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 2:04:54 am (UTC)'!
test016_AutocompletesMessagesFor_BlockTemporaryVariables_WithSelectorsForUnknownClasses
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ | a | a '.
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 [ | a | 1 + a '.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'NPM 18/Mar/2020, 10:34:08 pm (UTC)'!
test017_AutocompletesMessagesFor_NilWorkspaceVariables_WithSelectorsForUnknownClasses
	
	self
		assertEntriesAreSelectorsForUnknownClassesForWorkspaceWith: 'x '
		binding: 'x'
		to: nil.
	
	self
		assertEntriesAreSelectorsForUnknownClassesForWorkspaceWith: '1 + x '
		binding: 'x'
		to: nil.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 2:05:36 am (UTC)'!
test017_AutocompletesMessagesFor_NonNilWorkspaceVariables_WithSelectorsFromTheirClasses
	
	self
		assertEntriesForWorkspaceWith: 'x '
		binding: 'x'
		to: 1
		areSelectorsOf: SmallInteger.
		
	self
		assertEntriesForWorkspaceWith: '1 < x '
		binding: 'x'
		to: 1
		areUnaryAndBinarySelectorsOf: SmallInteger.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 3:27:14 am (UTC)'!
test018_AutocompletesMessagesFor_ThisContext_WithSelectorsFromMethodContext
	
	self
		assertEntriesWhenBrowsing: 'm1 thisContext '
		areSelectorsOf: MethodContext.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < thisContext '
		areUnaryAndBinarySelectorsOf: MethodContext.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 3:27:39 am (UTC)'!
test019_AutocompletesMessagesFor_ClassVariables_WithSelectorsFromTheirClasses
	
	ClassVariableForTesting _ 1.
		
	self
		assertEntriesWhenBrowsing: 'm1 ClassVariableForTesting '
		areSelectorsOf: SmallInteger.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < ClassVariableForTesting '
		areUnaryAndBinarySelectorsOf: SmallInteger.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 3:28:01 am (UTC)'!
test020_AutocompletesMessagesFor_PoolConstants_WithSelectorsFromTheirClasses
	
	poolDictionaryForTesting at: #X put: 1.
	
	self
		assertEntriesWhenBrowsing: 'm1 X '
		areSelectorsOf: SmallInteger.
	
	self
		assertEntriesWhenBrowsing: 'm1 1 < X '
		areUnaryAndBinarySelectorsOf: SmallInteger.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 3:39:25 am (UTC)'!
test021_AutocompletesMessagesFor_BlockEndings_WithSelectorsFromBlockClosure
	
	self
		assertEntriesWhenBrowsing: 'm1 [] '
		areSelectorsOf: BlockClosure.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < [] '
		areUnaryAndBinarySelectorsOf: BlockClosure.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 3:39:55 am (UTC)'!
test022_AutocompletesMessagesFor_ArrayEnds_WithSelectorsFromArray
	
	self
		assertEntriesWhenBrowsing: 'm1 #() '
		areSelectorsOf: Array.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < #() '
		areUnaryAndBinarySelectorsOf: Array.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:04:04 am (UTC)'!
test023_AutocompletesMessagesFor_RightBraces_WithSelectorsFromArray
	
	self
		assertEntriesWhenBrowsing: 'm1 {} '
		areSelectorsOf: Array.
		
	self
		assertEntriesWhenBrowsing: 'm1 1 < {} '
		areUnaryAndBinarySelectorsOf: Array.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 7/May/2020, 3:43:31 am (UTC)'!
test023_AutocompletingMessagesFor_UnaryMessages
	
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 self class '.
		
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 1 < self class '.	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 7/May/2020, 3:44:50 am (UTC)'!
test024_AutocompletingMessagesFor_RightParenthesis
	
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 (1) '.
		
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 1 < (1) '.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 6:08:24 am (UTC)'!
test025_AutocompletesMessagesFor_UnknownIdentifiers_WithSelectorsForUnknownClasses
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 Foo1234 '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 foo1234 '.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 7/May/2020, 3:45:10 am (UTC)'!
test026_AutocompletingMessagesFor_Cascade
	
	self denyComputingEntriesIsSupportedWhenBrowsing: 'm1 self class; '.
	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:13:19 am (UTC)'!
test027_AutocompletesEntriesFor_GlobalVariables_WithIdentifiersBegginingWithThem
	
	self
		assertEntriesWhenBrowsing: 'm1 SmalltalkCompleterTes'
		areIdentifiersBeginningWith: 'SmalltalkCompleterTest'.
	! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:14:50 am (UTC)'!
test028_AutocompletesEntriesFor_PoolConstants_WithIdentifiersBegginingWithThem
	
	poolDictionaryForTesting at: #X put: 1.
	
	self
		assertEntriesWhenBrowsing: 'm1 X'
		areIdentifiersBeginningWith: 'X'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'NPM 18/Mar/2020, 11:56:46 pm (UTC)'!
test029_AutocompletesEntriesFor_WorkspaceVariables_WithIdentifiersBegginingWithThem
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: 'x'
		binding: 'x'
		to: 1. 
		
	self
		assertEntriesOf: completer
		areIdentifiersBeginningWith: 'x'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:15:11 am (UTC)'!
test030_AutocompletesEntriesFor_ClassVariables_WithIdentifiersBegginingWithThem
	
	ClassVariableForTesting _ 1.
	
	self
		assertEntriesWhenBrowsing: 'm1 ClassVariableForTestin'
		areIdentifiersBeginningWith: 'ClassVariableForTesting'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:15:28 am (UTC)'!
test031_AutocompletesEntriesFor_InstanceVariables_WithIdentifiersBegginingWithThem
	
	self
		assertEntriesWhenBrowsing: 'm1 instanceVariable'
		areIdentifiersBeginningWith: 'instanceVariable1'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 1:36:46 am (UTC)'!
test032_AutocompletesEntriesFor_MethodArguments_WithIdentifiersBegginingWithThem
	
	| completer |
	
	completer := self autocompleteEntriesBrowsingMethod: SmalltalkCompleterTest >> #m3:.
	
	self
		assert: completer
		analizedSelectorsFrom: nil
		canShowDocumentation: false
		detectedPossibleInvalidSelector: #()
		suggested: (completer computeIdentifierEntriesBeginningWith: 'arg1').! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:15:54 am (UTC)'!
test033_AutocompletesEntriesFor_TemporaryVariables_WithIdentifiersBegginingWithThem
	
	self
		assertEntriesWhenBrowsing: 'm1 |xx| x'
		areIdentifiersBeginningWith: 'xx'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:16:11 am (UTC)'!
test034_AutocompletesEntriesFor_BlockArguments_WithIdentifiersBegginingWithThem
	
	self
		assertEntriesWhenBrowsing: 'm1 [ :xx | x'
		areIdentifiersBeginningWith: 'xx'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:16:27 am (UTC)'!
test035_AutocompletesEntriesFor_BlockTemporaryVariables_WithIdentifiersBegginingWithThem
	
	self
		assertEntriesWhenBrowsing: 'm1 [ | xx | x'
		areIdentifiersBeginningWith: 'xx'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:16:43 am (UTC)'!
test036_AutocompletesEntriesFor_IncompleteIdentifiers_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 Obj'
		areIdentifiersBeginningWith: 'Obj'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:17:34 am (UTC)'!
test037_AutocompletesEntriesFor_ReservedNames_WithIdentifiersBegginingWithThem

	self assertEntriesWhenBrowsing: 'm1 self' areIdentifiersBeginningWith: 'self'.
	self assertEntriesWhenBrowsing: 'm1 supe' areIdentifiersBeginningWith: 'super'.
	self assertEntriesWhenBrowsing: 'm1 tru' areIdentifiersBeginningWith: 'true'.
	self assertEntriesWhenBrowsing: 'm1 fals' areIdentifiersBeginningWith: 'false'.
	self assertEntriesWhenBrowsing: 'm1 ni' areIdentifiersBeginningWith: 'nil'.
	self assertEntriesWhenBrowsing: 'm1 thisContex' areIdentifiersBeginningWith: 'thisContext'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 6/Jan/2026, 12:00:17 am (UTC)'!
test038_DoesNotAutocompleteEntriesFor_SameUndefinedIdentifier

	| completer |
	completer := self createCompleterForBrowsing: 'asddas: kdkk ads'.
	
	completer computeEntries.
	
	self denyHasEntries: completer ! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 6/Jan/2026, 12:10:25 am (UTC)'!
test038b_AutocompleteEntriesFor_UndefinedIdentifiers

	| completer undefinedIdentifierEntries |
	
	completer := self createCompleterForBrowsing: 'm1 
	| otherIdentifier |
	identifier1 + identifier2 + otherIdentifier.
	ident'.
	
	completer computeEntries.
	undefinedIdentifierEntries := self entriesToCompareFrom: completer entries.
	self assert: undefinedIdentifierEntries size equals: 2.
	self assert: (undefinedIdentifierEntries includes: 'identifier1').
	self assert: (undefinedIdentifierEntries includes: 'identifier2').
	self deny: completer canShowSelectorDocumentation! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:18:55 am (UTC)'!
test039_AutocompletesEntriesFor_BinaryMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 1 ~='
		areSelectorsOf: SmallInteger
		beginningWith: '~='! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:19:17 am (UTC)'!
test040_AutocompletesEntriesFor_IncompleteBinaryMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 1 ~'
		areSelectorsOf: SmallInteger
		beginningWith: '~'! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:19:42 am (UTC)'!
test041_AutocompletesEntriesFor_UnaryMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 1 not'
		areSelectorsOf: SmallInteger
		beginningWith: 'not'! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:19:53 am (UTC)'!
test042_AutocompletesEntriesFor_IncompleteUnaryMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 1 no'
		areSelectorsOf: SmallInteger
		beginningWith: 'no'! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:20:03 am (UTC)'!
test043_AutocompletesEntriesFor_KeywordMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 1 at:'
		areSelectorsOf: SmallInteger
		beginningWith: 'at:'! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 3/May/2020, 5:18:26 am (UTC)'!
test044_AutocompletesEntriesFor_IncompleteKeywordMessages_WithIdentifiersBegginingWithThem

	self
		assertEntriesWhenBrowsing: 'm1 self firstKeyword:'
		areSelectorsOf: SmalltalkCompleterTest
		beginningWith: 'firstKeyword:'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'MH-NMS 16/Oct/2023, 3:57:00 pm (UTC)'!
test045_AutocompletesEntriesFor_patternUnary_containsExactlyTwoMethods

	| completer |
	completer := self createCompleterForBrowsing: 'fl'.
	
	completer computeEntries.

	self assert: completer entries size equals: 2.
	self assert: completer entries includes: 'flag: aSymbol'.
	self assert: completer entries includes: 'flash'.! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'MH-NMS 16/Oct/2023, 5:35:15 pm (UTC)'!
test046_AutocompletesEntriesFor_patternUnary_notContainsAnyMethod

	| completer |
	completer := self createCompleterForBrowsing: 'flagggg'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'MH-NMS 16/Oct/2023, 5:35:25 pm (UTC)'!
test047_AutocompletesEntriesFor_patternUnary_doesNotAutocompleteMethodOfSameClass

	| completer |
	completer := self createCompleterForBrowsing: 'm1'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'MH-NMS 16/Oct/2023, 5:35:37 pm (UTC)'!
test048_AutocompletesEntriesFor_patternKeyword_containsExactlyOneMethod

	| completer |
	completer := self createCompleterForBrowsing: 'flag:'.
	
	completer computeEntries.

	self assert: completer entries size equals: 1.
	self assert: completer entries includes: 'flag: aSymbol'
! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'MH-NMS 14/Oct/2023, 10:13:52 am (UTC)'!
test049_AutocompletesEntriesFor_patternKeyword_notContainsAnyMethod

	| completer |
	completer := self createCompleterForBrowsing: 'flagg:'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'MH-NMS 16/Oct/2023, 4:00:36 pm (UTC)'!
test050_AutocompletesEntriesFor_patternKeyword_doesNotAutocompleteMethodOfSameClass

	| completer |
	completer := self createCompleterForBrowsing: 'm1:'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'MH-NMS 16/Oct/2023, 5:35:57 pm (UTC)'!
test051_AutocompletesEntriesFor_patternBinary_containsExactlyOneMethod

	| completer |
	completer := self createCompleterForBrowsing: '->'.
	
	completer computeEntries.

	self assert: completer entries size equals: 1.
	self assert: completer entries includes: '-> anObject'! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'MH-NMS 16/Oct/2023, 5:36:07 pm (UTC)'!
test052_AutocompletesEntriesFor_patternBinary_notContainsAnyMethod

	| completer |
	completer := self createCompleterForBrowsing: '*'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0! !

!SmalltalkCompleterTest methodsFor: 'tests' stamp: 'MH-NMS 16/Oct/2023, 5:36:03 pm (UTC)'!
test053_AutocompletesEntriesFor_patternBinary_doestNotAutocompleteMethodOfSameClass

	| completer |
	completer := self createCompleterForBrowsing: '+'.
	
	completer computeEntries.

	self assert: completer entries size equals: 0! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'jmv 29/Apr/2026, 5:38:01 pm (UTC)'!
createCompleterForBrowsing: sourceCode 
	
	| browser model |
	browser := Browser new.
	browser setSelectedSystemCategoryTreeItem: (SystemCategoryWrapper with: classToBrowse category name: classToBrowse category model: browser).
	browser classListIndex: (browser classList indexOf: classToBrowse).
	
	model := (PluggableTextModel on: browser) actualContents: sourceCode.
	browser editSelection: #newMessage.

	^ self createCompleterWith: model.! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'NPM 18/Mar/2020, 11:17:59 pm (UTC)'!
createCompleterForWorkspaceWith: sourceCode binding: aVariableName to: aValue
	
	| model |
	model := Workspace withText: sourceCode.
	(model bindingOf: aVariableName) value: aValue.
	
	^ self createCompleterWith: model! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 2/May/2020, 5:14:11 pm (UTC)'!
createCompleterWith: aModel
	
	^ (SmalltalkCompleter withModel: aModel)
		changePositionTo: aModel actualContents size;
		yourself! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 2/May/2020, 8:37:44 pm (UTC)'!
firstKeyword: a secondKeyword: b! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 2/May/2020, 8:55:18 pm (UTC)'!
m1: arg1 arg1 ! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 2/May/2020, 8:37:29 pm (UTC)'!
m2: arg1 1 + arg1 ! !

!SmalltalkCompleterTest methodsFor: 'test objects' stamp: 'HAW 2/May/2020, 8:56:16 pm (UTC)'!
m3: arg1 arg1! !

!SmalltalkCompleterTest methodsFor: 'test support' stamp: 'NPM 18/Mar/2020, 11:13:43 pm (UTC)'!
autocompleteEntriesBrowsing: sourceCode
	
	| completer |
	completer := self createCompleterForBrowsing: sourceCode.
	
	completer computeEntries.
	
	^ completer! !

!SmalltalkCompleterTest methodsFor: 'test support' stamp: 'HAW 3/May/2020, 1:51:30 am (UTC)'!
autocompleteEntriesBrowsingMethod: aCompiledMethod 
	
	| completer selector |
	
	selector := aCompiledMethod selector.
	completer := self createCompleterForBrowsing: aCompiledMethod sourceCode.
	"Not nice, but does it work - Hernan"
	completer textProviderOrModel instVarNamed: 'currentCompiledMethod' put: aCompiledMethod.
	completer textProviderOrModel selectedMessageName: selector.
	
	completer computeEntries.
	
	^ completer! !

!SmalltalkCompleterTest methodsFor: 'test support' stamp: 'NPM 18/Mar/2020, 11:07:19 pm (UTC)'!
autocompleteEntriesForWorkspaceWith: sourceCode binding: aVariableName to: aValue 
	
	| completer |
	completer := self
		createCompleterForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
		
	completer computeEntries.
	
	^ completer! !

!SmalltalkCompleterTest methodsFor: 'test support' stamp: 'NPM 18/Mar/2020, 10:32:34 pm (UTC)'!
browseClass: aClass 
	
	classToBrowse _ aClass.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 3/May/2020, 12:21:58 am (UTC)'!
assert: aCompleter analizedSelectorsFrom: classes canShowDocumentation: aBoolean detectedPossibleInvalidSelector: selectors suggested: entries 
	
	classes 
		ifNil: [ self assert: aCompleter selectorsClasses isNil ]
		ifNotNil: [ self assert: aCompleter selectorsClasses asSet = classes asSet ].
	self assert: aCompleter canShowSelectorDocumentation equals: aBoolean.
	self assert: aCompleter possibleInvalidSelectors asSet = selectors asSet.
	entries 
		ifNil: [ self assert: aCompleter entries isNil ]
		ifNotNil: [ self assert: (self entriesToCompareFrom: aCompleter entries) = (self entriesToCompareFrom: entries) ]
		
		! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 19/Mar/2020, 12:25:15 am (UTC)'!
assertEntriesAreSelectorsForUnknownClasses: aCompleter 
	
	| expectedEntries expectedPossibleInvalidSelectors |
	expectedPossibleInvalidSelectors _ Set new.
	expectedEntries _ aCompleter computeMessageEntriesForUnknowClassAddingPossibleInvalidSelectorsTo: expectedPossibleInvalidSelectors.
	
	self
		assert: aCompleter
		analizedSelectorsFrom: #()
		canShowDocumentation: true
		detectedPossibleInvalidSelector: expectedPossibleInvalidSelectors
		suggested: expectedEntries! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 18/Mar/2020, 11:06:02 pm (UTC)'!
assertEntriesAreSelectorsForUnknownClassesForWorkspaceWith: sourceCode binding: aVariableName to: aValue
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
	
	self assertEntriesAreSelectorsForUnknownClasses: completer.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 18/Mar/2020, 11:07:53 pm (UTC)'!
assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: sourceCode 
	
	| completer |
	completer := self autocompleteEntriesBrowsing: sourceCode.
	
	self assertEntriesAreSelectorsForUnknownClasses: completer.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 3/May/2020, 1:36:46 am (UTC)'!
assertEntriesAreSelectorsOfUnknownClassesWhenBrowsingMethodWith: aCompiledMethod 
	
	| completer |
	
	completer := self autocompleteEntriesBrowsingMethod: aCompiledMethod.
	
	self assertEntriesAreSelectorsForUnknownClasses: completer.
	
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 18/Mar/2020, 11:08:09 pm (UTC)'!
assertEntriesForWorkspaceWith: sourceCode binding: aVariableName to: aValue areSelectorsOf: aClass
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
	
	self
		assertEntriesOf: completer
		areAllSelectorsOf: aClass.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 18/Mar/2020, 11:19:49 pm (UTC)'!
assertEntriesForWorkspaceWith: sourceCode binding: aVariableName to: aValue areUnaryAndBinarySelectorsOf: aClass 
	
	| completer |
	completer := self
		autocompleteEntriesForWorkspaceWith: sourceCode
		binding: aVariableName
		to: aValue.
	
	self
		assertEntriesOf: completer
		areUnaryAndBinarySelectorsOf: aClass.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 17/Mar/2020, 10:27:01 pm (UTC)'!
assertEntriesOf: aCompleter areAllSelectorsOf: aClass

	self
		assertEntriesOf: aCompleter
		areAllSelectorsOf: aClass
		beginningWith: ''! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 2/May/2020, 9:38:48 pm (UTC)'!
assertEntriesOf: aCompleter areAllSelectorsOf: aClass beginningWith: aPrefix

	self assertEntriesOf: aCompleter areAllSelectorsOfAll: { aClass } beginningWith: aPrefix
! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 2/May/2020, 10:42:55 pm (UTC)'!
assertEntriesOf: aCompleter areAllSelectorsOfAll: classes beginningWith: aPrefix

	| suggested |

	suggested := classes inject: Set new into: [ :suggestedCollector :aClass |
		suggestedCollector 
			addAll: (aCompleter selectorsOf: aClass beginningWith: aPrefix);
			yourself ].
	
	self
		assert: aCompleter
		analizedSelectorsFrom: classes
		canShowDocumentation: true
		detectedPossibleInvalidSelector: #()
		suggested: suggested.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 19/Mar/2020, 12:25:15 am (UTC)'!
assertEntriesOf: aCompleter areIdentifiersBeginningWith: aPrefix

	self
		assert: aCompleter
		analizedSelectorsFrom: nil
		canShowDocumentation: false
		detectedPossibleInvalidSelector: #()
		suggested: (aCompleter computeIdentifierEntriesBeginningWith: aPrefix)! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 3/May/2020, 12:10:35 am (UTC)'!
assertEntriesOf: aCompleter areUnaryAndBinarySelectorsOf: aClass

	self
		assert: aCompleter
		analizedSelectorsFrom: {aClass}
		canShowDocumentation: true
		detectedPossibleInvalidSelector: #()
		suggested: (self using: aCompleter addUnaryAndBinarySelectorsOf: aClass).
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 18/Mar/2020, 11:58:16 pm (UTC)'!
assertEntriesWhenBrowsing: sourceCode areIdentifiersBeginningWith: aPrefix
	
	| completer |
	completer := self autocompleteEntriesBrowsing: sourceCode.
	
	self
		assertEntriesOf: completer
		areIdentifiersBeginningWith: aPrefix.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 18/Mar/2020, 10:55:09 pm (UTC)'!
assertEntriesWhenBrowsing: sourceCode areSelectorsOf: aClass
	
	self
		assertEntriesWhenBrowsing: sourceCode
		areSelectorsOf: aClass
		beginningWith: ''! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 18/Mar/2020, 10:58:41 pm (UTC)'!
assertEntriesWhenBrowsing: sourceCode areSelectorsOf: aClass beginningWith: aPrefix
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsing: sourceCode)
		areAllSelectorsOf: aClass
		beginningWith: aPrefix 
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 2/May/2020, 9:40:10 pm (UTC)'!
assertEntriesWhenBrowsing: sourceCode areSelectorsOfAll: classes
	
	self
		assertEntriesWhenBrowsing: sourceCode
		areSelectorsOfAll: classes
		beginningWith: ''! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 2/May/2020, 9:41:09 pm (UTC)'!
assertEntriesWhenBrowsing: sourceCode areSelectorsOfAll: classes beginningWith: aPrefix
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsing: sourceCode)
		areAllSelectorsOfAll: classes
		beginningWith: aPrefix 
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 18/Mar/2020, 10:59:22 pm (UTC)'!
assertEntriesWhenBrowsing: sourceCode areUnaryAndBinarySelectorsOf: aClass
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsing: sourceCode)
		areUnaryAndBinarySelectorsOf: aClass.
	
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 3/May/2020, 1:33:48 am (UTC)'!
assertEntriesWhenBrowsingMethod: aMethod areSelectorsOf: aClass
	
	self
		assertEntriesWhenBrowsingMethod: aMethod
		areSelectorsOf: aClass
		beginningWith: ''! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 3/May/2020, 1:36:46 am (UTC)'!
assertEntriesWhenBrowsingMethod: aMethod areSelectorsOf: aClass beginningWith: aPrefix
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsingMethod: aMethod)
		areAllSelectorsOf: aClass
		beginningWith: aPrefix 
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 3/May/2020, 1:36:46 am (UTC)'!
assertEntriesWhenBrowsingMethod: aMethod areUnaryAndBinarySelectorsOf: aClass
	
	self
		assertEntriesOf: (self autocompleteEntriesBrowsingMethod: aMethod)
		areUnaryAndBinarySelectorsOf: aClass.
	
	! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'NPM 19/Mar/2020, 12:25:15 am (UTC)'!
denyComputingEntriesIsSupportedWhenBrowsing: sourceCode 
	
	| completer |
	completer := self createCompleterForBrowsing: sourceCode.
	
	self
		should: [ completer computeEntries ]
		raise: Error.
	self
		assert: completer
		analizedSelectorsFrom: nil
		canShowDocumentation: true
		detectedPossibleInvalidSelector: #()
		suggested: nil.! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 5/Jan/2026, 11:57:14 pm (UTC)'!
denyHasEntries: aCompleter  

	self
		assert: aCompleter
		analizedSelectorsFrom: nil
		canShowDocumentation: false
		detectedPossibleInvalidSelector: #()
		suggested: #()! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 3/May/2020, 12:24:54 am (UTC)'!
entriesToCompareFrom: aCollectionOfEntries

	^(aCollectionOfEntries reject: [ :anEntry | 
		anEntry beginsWith: AutoCompleterSelectorsCollector categoryEntryHeader ]) asSet! !

!SmalltalkCompleterTest methodsFor: 'assertions' stamp: 'HAW 3/May/2020, 12:10:35 am (UTC)'!
using: aCompleter addUnaryAndBinarySelectorsOf: aClass

	^ aCompleter unaryAndBinarySelectorsOf: aClass beginningWith: ''! !

!SmalltalkCompleterTest methodsFor: 'setUp/tearDown' stamp: 'NPM 18/Mar/2020, 10:32:49 pm (UTC)'!
setUp

	super setUp.
	
	self browseClass: SmalltalkCompleterTest.
	
! !

!SmalltalkCompleterTest methodsFor: 'running' stamp: 'HAW 3/May/2020, 12:49:50 am (UTC)'!
performTest

	^SmalltalkCompleter changeEntriesLimitTo: SmallInteger maxVal during: [ super performTest ]! !

!SmalltalkCompleterTest class methodsFor: 'testing' stamp: 'HAW 2/May/2020, 5:21:14 pm (UTC)'!
isAbstract

	^true! !

!DynamicTypingSmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 7/May/2020, 3:43:31 am (UTC)'!
test023_AutocompletingMessagesFor_UnaryMessages
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 self class '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 + self class '.	! !

!DynamicTypingSmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 7/May/2020, 3:44:50 am (UTC)'!
test024_AutocompletingMessagesFor_RightParenthesis
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 (1) '.
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 1 < (1) '.! !

!DynamicTypingSmalltalkCompleterTest methodsFor: 'tests' stamp: 'HAW 7/May/2020, 3:45:10 am (UTC)'!
test026_AutocompletingMessagesFor_Cascade
	
	self assertEntriesAreSelectorsOfUnknownClassesWhenBrowsing: 'm1 self class; '.
	! !

!DynamicTypingSmalltalkCompleterTest class methodsFor: 'testing' stamp: 'HAW 2/May/2020, 8:27:37 pm (UTC)'!
isAbstract

	^DynamicTypingSmalltalkCompleter isForCurrentTypeSystem not! !

!MethodReferenceTest methodsFor: 'test actual class' stamp: 'jmv 31/Jul/2023, 4:25:57 pm (UTC)'!
testActualClassIfAbsentEvaluatesBlockIfClassDoesNotExist

	self 
		assert: ((MethodReference 
			classSymbol: #X_Y_Z 
			classIsMeta: false 
			methodSymbol: #xx) actualClassIfAbsent: [ 1])
		equals: 1.! !

!MethodReferenceTest methodsFor: 'test actual class' stamp: 'jmv 31/Jul/2023, 4:26:27 pm (UTC)'!
testActualClassIfAbsentReturnsClassIfClassExists

	self 
		assert: ((MethodReference 
			class: self class  
			selector: thisContext selector) actualClassIfAbsent: [ self fail])
		equals: self class.! !

!MethodReferenceTest methodsFor: 'test actual class' stamp: 'HAW 24/Oct/2019, 1:05:22 am (UTC)'!
testActualClassReturnNilWhenClassDoesNotExist

	self assert: (MethodReference classSymbol: #X_Y_Z classIsMeta: false methodSymbol: #xx) actualClass isNil! !

!MethodReferenceTest methodsFor: 'test actual class' stamp: 'jmv 31/Jul/2023, 4:26:49 pm (UTC)'!
testActualClassReturnsClassIfExists

	self 
		assert: (MethodReference 
			class: self class 
			selector: thisContext selector) actualClass 
		equals: self class.! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'jmv 31/Jul/2023, 4:28:00 pm (UTC)'!
testSourceCodeIfAbsentEvaluatesBlockIfClassDoesNotExist

	self 
		assert: ((MethodReference classSymbol: #X_Y_Z classIsMeta: false methodSymbol:#xx) sourceCodeIfAbsent: [ 1 ])
		equals: 1.! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'jmv 31/Jul/2023, 4:28:12 pm (UTC)'!
testSourceCodeIfAbsentEvaluatesBlockIfMethodDoesNotExist

	self 
		assert: ((MethodReference class: self class selector:#xx) sourceCodeIfAbsent: [ 1 ])
		equals: 1.! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'jmv 31/Jul/2023, 4:28:34 pm (UTC)'!
testSourceCodeIfAbsentReturnsSourceCodeIfExists

	self 
		assert: ((MethodReference class: self class selector: thisContext selector) sourceCodeIfAbsent: [ self fail ])
		equals: (self class>>thisContext selector) sourceCode.! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'jmv 31/Jul/2023, 4:28:54 pm (UTC)'!
testSourceCodeReturnsSourceCodeIfMethodExist

	self 
		assert: (MethodReference 
			class: self class 
			selector: thisContext selector) sourceCode
		equals: (self class compiledMethodAt: thisContext selector) sourceCode.! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'HAW 24/Oct/2019, 1:39:16 am (UTC)'!
testSourceCodeSignalExceptionIfClassDoesNotExist

	self 
		should: [ (MethodReference classSymbol: #X_Y_Z classIsMeta: false methodSymbol:#xx) sourceCode ]
		raise: Error
		withMessageText: MethodReference classDoesNotExistErrorMessage ! !

!MethodReferenceTest methodsFor: 'test source code' stamp: 'jmv 20/Sep/2023, 7:44:26 pm (UTC)'!
testSourceCodeSignalExceptionIfMethodDoesNotExist

	self 
		should: [ (MethodReference class: self class selector: #xx) sourceCode ]
		raise: Error
		withMessageText: 'key: #xx not found'! !

!MethodReferenceTest methodsFor: 'test compiled method' stamp: 'jmv 31/Jul/2023, 4:27:03 pm (UTC)'!
testCompiledMethodIfAbsentEvaluatesAbsentBlockWhenActualClassIsNil

	self 
		assert: ((MethodReference 
			classSymbol: #X_Y_Z 
			classIsMeta: false 
			methodSymbol: #xx) compiledMethodIfAbsent: [ 1])
		equals: 1.! !

!MethodReferenceTest methodsFor: 'test compiled method' stamp: 'jmv 31/Jul/2023, 4:27:19 pm (UTC)'!
testCompiledMethodIfAbsentEvaluatesAbsentBlockWhenSelectorIsNotImplemented

	self 
		assert: ((MethodReference 
			classSymbol: self class name
			classIsMeta: false 
			methodSymbol: #xx) compiledMethodIfAbsent: [ 1])
		equals: 1.! !

!MethodReferenceTest methodsFor: 'test compiled method' stamp: 'jmv 31/Jul/2023, 4:27:34 pm (UTC)'!
testCompiledMethodIfAbsentReturnsImplementedMethodWhenItExists

	self 
		assert: ((MethodReference 
			classSymbol: self class name
			classIsMeta: false 
			methodSymbol: thisContext selector) compiledMethodIfAbsent: [ self fail ])
		equals: thisContext method .! !

!MethodReferenceTest methodsFor: 'test string version' stamp: 'jmv 31/Jul/2023, 4:25:16 pm (UTC)'!
test01DefaultStringVersionIsClassAndSelector

	| stringVersion |
	
	stringVersion := (MethodReference method: thisContext method) stringVersion.
	
	self assert: stringVersion equals: self class name, ' ', thisContext selector.! !

!MethodReferenceTest methodsFor: 'test string version' stamp: 'jmv 31/Jul/2023, 4:25:41 pm (UTC)'!
test02CanPrefixStringVersion

	| methodReference prefix |
	
	prefix := 'a prefix'.
	methodReference := MethodReference method: thisContext method.
	methodReference prefixStringVersionWith: prefix.
	
	self assert: methodReference stringVersion equals: '[',prefix,'] - ',self class name, ' ', thisContext selector.! !

!MethodReferenceTest methodsFor: 'test testing' stamp: 'HAW 2/Jan/2020, 10:49:00 am (UTC)'!
testReferencesParameterAtReturnsFalseWhenInvalid

	self deny: ((MethodReference 
		classSymbol: #X_Y_Z 
		classIsMeta: false 
		methodSymbol: #xx) referencesParameterAt: 1)! !

!MethodReferenceTest methodsFor: 'test testing' stamp: 'HAW 2/Jan/2020, 10:49:03 am (UTC)'!
testReferencesParameterAtReturnsTrueWhenTheParameterIsReferenced

	self assert: ((MethodReference 
		classSymbol: #MethodReference 
		classIsMeta: false 
		methodSymbol: #referencesParameterAt:) referencesParameterAt: 1)! !

!DifferenceFinderTest methodsFor: 'all' stamp: 'LC 24/Jan/2010, 3:29:00 pm (UTC)'!
testCharacters
	| lcs |
	lcs := DifferenceFinder charactersOf: 'GAC' and: 'AGCAT'.
	lcs compute.
	self
		assert: (lcs lcsAt: 1 at: 1) anyOne size = 0;
		assert: (lcs lcsAt: 1 at: 2) anyOne size = 1;
		assert: (lcs lcsAt: 1 at: 3) anyOne size = 1;
		assert: (lcs lcsAt: 1 at: 4) anyOne size = 1;
		assert: (lcs lcsAt: 1 at: 5) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 1) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 2) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 3) anyOne size = 1;
		assert: (lcs lcsAt: 2 at: 4) anyOne size = 2;
		assert: (lcs lcsAt: 2 at: 5) anyOne size = 2;
		assert: (lcs lcsAt: 3 at: 1) anyOne size = 1;
		assert: (lcs lcsAt: 3 at: 2) anyOne size = 1;
		assert: (lcs lcsAt: 3 at: 3) anyOne size = 2;
		assert: (lcs lcsAt: 3 at: 4) anyOne size = 2;
		assert: (lcs lcsAt: 3 at: 5) anyOne size = 2
! !

!DifferenceFinderTest methodsFor: 'all' stamp: ''!
testCharacters2
	| finder x y |
	finder := DifferenceFinder charactersOf: 'GAC' and: 'AGCAT'.
	finder compute.
	self assert: finder differences size = 3.
	finder differences do: [:diff | 
		x := String streamContents: [:strm | 
			diff
				do: [:char :condition | (#(#removed #unchanged) includes: condition)
					ifTrue: [strm nextPut: char]]].
		self assert: x = 'GAC'.
		y := String streamContents: [:strm | 
			diff
				do: [:char :condition | (#(#inserted #unchanged) includes: condition)
					ifTrue: [strm nextPut: char]]].
		self assert: y = 'AGCAT']
! !

!DifferenceFinderTest methodsFor: 'all' stamp: ''!
testCode
	| old new finder |
	old := 'differencesText
	| change sourceString current diff rtf selectedString |
	change := self selectedMethod ifNil: [self currentClass].
	current := change currentVersion.
	sourceString := current isNil
		ifTrue: ['']
		ifFalse: [current isCompiledMethod ifTrue: [(self
			formatSource: current sourceString
			inClass: current classField)
			sourceCode] ifFalse: [current sourceString]].
	selectedString := (current isNil or: [current isCompiledMethod not])
		ifTrue: [change sourceString]
		ifFalse: [((self
			formatSource: change sourceString
			inClass: current classField) ifNil: [change]) sourceCode].
	diff := TextDiffBuilder from: sourceString to: selectedString.
	rtf := RTFText new setFont: TextFont.
	rtf setTabStops: self tabStops.
	diff run; printPatchSequenceOn: rtf.
	^rtf contents contents'.
	new := 'differencesText
	| change sourceString current finder rtf selectedString |
	change := self selectedMethod ifNil: [self currentClass].
	current := change currentVersion.
	sourceString := current isNil ifTrue: [''] ifFalse: [
		current isCompiledMethod
			ifTrue: [(self
				formatSource: current sourceString
				inClass: current classField)
				sourceCode]
			ifFalse: [current sourceString]].
	selectedString := (current isNil or: [current isCompiledMethod not])
		ifTrue: [change sourceString]
		ifFalse: [((self
			formatSource: change sourceString
			inClass: current classField)
			ifNil: [change])
			sourceCode].
	finder := DifferenceFinder wordsOf: sourceString and: selectedString.
	finder compute.
	rtf := RTFText new setFont: TextFont.
	rtf setTabStops: self tabStops.
	finder differences first printTextOn: rtf.
	^rtf contents contents'.
	finder := DifferenceFinder wordsOf: old and: new.
	finder compute
! !

!DifferenceFinderTest methodsFor: 'all' stamp: ''!
testLines
	| finder alan ian x y |
	alan := 'The best way to
	predict
	the future is to
	invent
	it.
	A. Kay'.
	ian := 'The best way to
	invent
	the future is to
	not predicting
	it.
	Ian Piumarta'.
	finder := DifferenceFinder linesOf: alan and: ian.
	finder compute.
	finder differences do: [:diff | 
		x := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#removed #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: x = alan.
		y := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#inserted #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: y = ian]
! !

!DifferenceFinderTest methodsFor: 'all' stamp: ''!
testWords
	| finder alan ian x y |
	alan := 'The best way to
	predict
	the future is to
	invent
	it.
	A. Kay'.
	ian := 'The best way to
	invent 	the future is to
	not predicting
	it.
	Ian Piumarta'.
	finder := DifferenceFinder wordsOf: alan and: ian.
	finder compute.
	finder differences do: [:diff | 
		x := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#removed #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: x = alan.
		y := String streamContents: [:strm | 
			diff
				do: [:chunk :condition | (#(#inserted #unchanged) includes: condition)
					ifTrue: [strm nextPutAll: chunk]]].
		self assert: y = ian]
! !

!DebuggerTest methodsFor: 'tests - method categories' stamp: 'jmv 18/May/2026, 12:12:44 am (UTC)'!
testCategoriesAreAddedSortedByAndSeparatedByClass

	| objectCategories protoObjectCategories categoriesPrompter |

	objectCategories := Object methodCategoriesAsSortedCollection.
	protoObjectCategories := ProtoObject methodCategoriesAsSortedCollection removeAllFoundIn: objectCategories; yourself.

	"Test pre-conditions. May seem unnecesary but they are not if we take time into account - Hernan"
	self assert: Object superclass equals: ProtoObject.
	self assert: ProtoObject superclass equals: nil.
	self assert: objectCategories notEmpty.
	self assert: protoObjectCategories notEmpty.

	categoriesPrompter := MethodCategoriesPrompter startingFrom: Object rejectingFirst: false.

	self assert: categoriesPrompter lines size equals: 2.
	self assert: categoriesPrompter lines first equals: 1.
	self assert: (categoriesPrompter categories copyFrom: 2 to: categoriesPrompter lines second) asArray equals: objectCategories asArray.
	self assert: (categoriesPrompter categories copyFrom: categoriesPrompter lines second + 1 to: categoriesPrompter categories size) asArray equals: protoObjectCategories asArray.! !

!DebuggerTest methodsFor: 'tests - method categories' stamp: 'jmv 18/May/2026, 12:12:59 am (UTC)'!
testMetaclassCategoriesIncludesInstanceCreation

	| categoriesPrompter |

	categoriesPrompter := MethodCategoriesPrompter startingFrom: ProtoObject class rejectingFirst: false.

	self assert: categoriesPrompter categories second equals: Categorizer instanceCreation.! !

!QuickReturnDebuggingTest methodsFor: 'private' stamp: 'FJG 13/Nov/2023, 6:01:10 pm (UTC)'!
debuggerOn: aBlock
	| debugger |
	
	debugger := Debugger newDebugging: aBlock.
	
	debugger expandStack.
	
	[debugger interruptedContext method == aBlock outerContext method ]
		whileFalse: [debugger send].
		
	^ debugger! !

!QuickReturnDebuggingTest methodsFor: 'private' stamp: 'FJG 8/Nov/2023, 11:12:58 pm (UTC)'!
quickReturn
	" Example:
		QuickReturnDebuggingTest new halt quickReturn
	"
	
	^ 2! !

!QuickReturnDebuggingTest methodsFor: 'tests' stamp: 'FJG 8/Nov/2023, 11:12:03 pm (UTC)'!
test01_debuggerCanStepIntoQuickReturnMethods
	| aDebugger |
	aDebugger := self debuggerOn: [		self quickReturn 	].
	
	aDebugger send.
	
	self assert: aDebugger interruptedContext method equals: self class >> #quickReturn.! !

!QuickReturnDebuggingTest methodsFor: 'tests' stamp: 'FJG 8/Nov/2023, 11:12:20 pm (UTC)'!
test02_debuggerCanProceedFromAQuickReturnMethod
	| aDebugger result |
	aDebugger := self debuggerOn: [		
		result := self quickReturn 
	].
	aDebugger send.
	
	aDebugger proceed.
	
	self assert: result equals: self quickReturn.! !

!QuickReturnDebuggingTest methodsFor: 'tests' stamp: 'FJG 10/Nov/2023, 3:20:47 am (UTC)'!
test03_debuggerCanStepThroughFromAQuickReturnMethod
	| aDebugger result |
	aDebugger := self debuggerOn: [		
		result := self quickReturn 
	].
	aDebugger send.
	
	aDebugger stepIntoBlock.
	
	self assert: aDebugger interruptedContext method equals: thisContext method.
	aDebugger proceed.
	self assert: result equals: self quickReturn.! !

!QuickReturnDebuggingTest methodsFor: 'tests' stamp: 'FJG 10/Nov/2023, 3:21:12 am (UTC)'!
test04_debuggerCanStepFromAQuickReturnMethod
	| aDebugger result |
	aDebugger := self debuggerOn: [		
		result := self quickReturn 
	].
	aDebugger send.
	
	aDebugger doStep.
	
	self assert: aDebugger interruptedContext method equals: thisContext method.
	aDebugger proceed.
	self assert: result equals: self quickReturn.! !

!QuickReturnDebuggingTest methodsFor: 'tests' stamp: 'FJG 10/Nov/2023, 3:21:20 am (UTC)'!
test05_debuggerCanSendFromAQuickReturnMethod
	| aDebugger result |
	aDebugger := self debuggerOn: [		
		result := self quickReturn 
	].
	aDebugger send.
	
	aDebugger send.
	
	self assert: aDebugger interruptedContext method equals: thisContext method.
	aDebugger proceed.
	self assert: result equals: self quickReturn.! !

!QuickReturnDebuggingTest methodsFor: 'tests' stamp: 'FJG 10/Nov/2023, 3:23:04 am (UTC)'!
test06_debuggerCanRestartFromAQuickReturnMethod
	| aDebugger result |
	aDebugger := self debuggerOn: [		
		result := self quickReturn 
	].
	aDebugger send.
	
	aDebugger restart.
	
	self assert: aDebugger interruptedContext method equals: self class >> #quickReturn.
	aDebugger proceed.
	self assert: result equals: self quickReturn.! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 4:16:37 pm (UTC)'!
test00MessagesForTestingAreImplemented

	self assert: (self respondsTo: #_zz).
	self assert: (self respondsTo: #!!).
	! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 3:56:57 pm (UTC)'!
test01TypeOfMessageSentToLiteralNumberAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '1'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 3:58:55 pm (UTC)'!
test02TypeOfMessageSentToLiteralStringAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '''a'''! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 3:58:39 pm (UTC)'!
test03TypeOfMessageSentToLiteralSymbolAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '#a'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 3:59:12 pm (UTC)'!
test04TypeOfMessageSentToLiteralArrayAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '#(1)'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 3:59:23 pm (UTC)'!
test05TypeOfMessageSentToLiteralBraceArrayAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '{1}'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 3:59:33 pm (UTC)'!
test06TypeOfMessageSentToLiteralBlockAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '[1]'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 3:59:42 pm (UTC)'!
test07TypeOfMessageSentToLiteralCharacterAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: '$a'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 3:59:51 pm (UTC)'!
test08TypeOfMessageSentToLiteralNilAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: 'nil'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 3:59:59 pm (UTC)'!
test09TypeOfMessageSentToLiteralTrueAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: 'true'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 4:00:08 pm (UTC)'!
test10TypeOfMessageSentToLiteralFalseAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: 'false'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 15/Jul/2021, 2:47:18 pm (UTC)'!
test11TypeOfMessageSentToSelfAreObtainedCorrectly

	self 
		assertTypeOfMessageSend: 'm1 self _zz'
		is: #undefinedUnary
		in: Object
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ].
	self 
		assertTypeOfMessageSend: 'm1 self size'
		is: #unary
		in: Object
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ].
		
	"Binary messages have been disabled - Hernan
	self 
		assertTypeOfMessageSend: 'm1 self !! 2'
		is: #undefinedBinary
		in: Object
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ]."
		
	self 
		assertTypeOfMessageSend: 'm1 self = 2'
		is: #binary
		in: Object
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ].! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 4:00:26 pm (UTC)'!
test12TypeOfMessageSentToSuperAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: 'super'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 15/Jul/2021, 2:47:00 pm (UTC)'!
test13TypesIsUndefinedForAnyMessageSendToSuperFromProtoObject

	self 
		assertTypeOfMessageSend: 'm1 super size'
		is: #undefinedUnary
		in: ProtoObject
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ].
		
	"binaryMessages have been disabled - Hernan
	self 
		assertTypeOfMessageSend: 'm1 super = 2'
		is: #undefinedBinary
		in: ProtoObject
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ]."! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 4:00:37 pm (UTC)'!
test14TypeOfMessageSentToGlobalVarAreObtainedCorrectly

	self assertTypesAreValidWhenMessageSendTo: 'Smalltalk'! !

!SHST80RangeTypeTest methodsFor: 'tests' stamp: 'HAW 5/Oct/2020, 4:24:04 pm (UTC)'!
test15TypeOfMessageSentToClassVarAreObtainedCorrectly

	ClassVar1 := 1.
	self assertTypesAreValidWhenMessageSendTo: 'ClassVar1'! !

!SHST80RangeTypeTest methodsFor: 'assertions' stamp: 'HAW 5/Oct/2020, 4:02:43 pm (UTC)'!
assertTypeOfBinaryMessageSend: aSourceCode is: aExpectedType

	self 
		assertTypeOfMessageSend: aSourceCode 
		is: aExpectedType 
		receiverRange: [ :ranges | ranges antepenultimate ]
		messageRange: [ :ranges | ranges penultimate ]
! !

!SHST80RangeTypeTest methodsFor: 'assertions' stamp: 'jmv 31/Jul/2023, 3:32:34 pm (UTC)'!
assertTypeOfMessageSend: aSourceCode 
	is: aExpectedType
	in: aClassOrMetaClass 
	receiverRange: aReceiverRangeBlock 
	messageRange: aMessageRangeBlock

	| parser ranges st80RangeType type |
	
	parser := SHParserST80 new.
	ranges := parser
		workspace: nil;
		classOrMetaClass: aClassOrMetaClass ;
		source: aSourceCode;
		parse;
		ranges.
		
	st80RangeType := SHST80RangeType for: aSourceCode in: aClassOrMetaClass.
	st80RangeType lastRange: (aReceiverRangeBlock value: ranges).
	type := st80RangeType ofCurrentRangeOrMessageSendIn: (aMessageRangeBlock value: ranges).
	
	self assert: type equals: aExpectedType
	! !

!SHST80RangeTypeTest methodsFor: 'assertions' stamp: 'HAW 5/Oct/2020, 4:23:51 pm (UTC)'!
assertTypeOfMessageSend: aSourceCode 
	is: aExpectedType 
	receiverRange: aReceiverRangeBlock 
	messageRange: aMessageRangeBlock

	self 
		assertTypeOfMessageSend: aSourceCode 
		is: aExpectedType
		in: self class
		receiverRange: aReceiverRangeBlock 
		messageRange: aMessageRangeBlock! !

!SHST80RangeTypeTest methodsFor: 'assertions' stamp: 'HAW 5/Oct/2020, 4:02:52 pm (UTC)'!
assertTypeOfUnaryMessageSend: aSourceCode is: aExpectedType

	self 
		assertTypeOfMessageSend: aSourceCode 
		is: aExpectedType 
		receiverRange: [ :ranges | ranges penultimate ]
		messageRange: [ :ranges | ranges last ]
! !

!SHST80RangeTypeTest methodsFor: 'assertions' stamp: 'HAW 15/Jul/2021, 2:45:30 pm (UTC)'!
assertTypesAreValidWhenMessageSendTo: aReceiverAsString

	self assertTypeOfUnaryMessageSend: 'm1 ', aReceiverAsString, ' _zz' is: #undefinedUnary.
	self assertTypeOfUnaryMessageSend: 'm1 ', aReceiverAsString, ' size' is: #unary.

	" binary messages has beed disabled by now - Hernan
	self assertTypeOfBinaryMessageSend: 'm1 ', aReceiverAsString, ' !! 2' is: #undefinedBinary.
	self assertTypeOfBinaryMessageSend: 'm1 ', aReceiverAsString, ' = 2' is: #binary
	"! !

!SHST80RangeTypeTest methodsFor: 'messages for testing' stamp: 'HAW 5/Oct/2020, 4:15:40 pm (UTC)'!
!! something

	"Do not remove this method because it exists for its name to have implementors - Hernan"! !

!SHST80RangeTypeTest methodsFor: 'messages for testing' stamp: 'HAW 5/Oct/2020, 4:15:50 pm (UTC)'!
_zz

	"Do not remove this method because it exists for its name to have implementors - Hernan"! !

!TaskbarTest methodsFor: 'Running' stamp: 'jmv 20/Mar/2024, 7:45:15 pm (UTC)'!
setUp

	super setUp.
	needsDelete := UISupervisor ui taskbar isNil.
	taskbar := UISupervisor ui hideTaskbar; showTaskbar; taskbar.
	taskbar screenSizeChanged.
	"taskbar world runStepMethods"! !

!TaskbarTest methodsFor: 'Running' stamp: 'jmv 28/Dec/2017, 8:12:13 pm (UTC)'!
tearDown

	needsDelete ifTrue: [
		UISupervisor ui hideTaskbar ]! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 28/Dec/2017, 8:12:16 pm (UTC)'!
testClassSingleton

	self should: [ taskbar == UISupervisor ui taskbar ].
! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 3/Sep/2024, 12:10:32 am (UTC)'!
testHorizontalAlignment

	self should: [ taskbar positionInWorld x = 0 ]! !

!TaskbarTest methodsFor: 'test cases' stamp: 'jmv 28/Dec/2017, 8:12:01 pm (UTC)'!
testWidth
	
	self should: [ taskbar morphWidth = UISupervisor ui morphWidth ]! !

!TestCaseTest methodsFor: 'testing' stamp: 'jpb 3/Aug/2019, 3:47:26 am (UTC)'!
testIsTestCase
	self assert: (TestCase new is: #TestCase).! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 17/Mar/2019, 11:31:39 am (UTC)'!
testShouldFailErrorsWhenNoErrorIsSignaled

	self
		should: [ self shouldFail: [] ]
		raise: TestResult failure! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 17/Mar/2019, 11:32:19 am (UTC)'!
testShouldFailPassesWhenAnErrorIsSignaled

	self shouldFail: [ self error: 'an error' ]! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 5/Apr/2018, 6:32:15 pm (UTC)'!
testShouldRaiseWithExceptionDoCanExpectException

	"This test is important because TestFailure is a subclass of Exception, therefore if we are expecting Exception to be raised it should not 
	catch the TestFailure - Hernan"
	
	| failureRaised |
	
	failureRaised := false.
	
	[self should: [] raise: Exception ]
		on: TestResult failure
		do: [ :failure | failureRaised := true ].
		
	self assert: failureRaised ! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HernanWilkinson 20/Jan/2017, 12:52:39 am (UTC)'!
testShouldRaiseWithExceptionDoFailsWhenNoExceptionIsSignaled
	
	| failureSignaled |
	
	failureSignaled := false.
	
	[self 
		should: []
		raise: ZeroDivide 
		withExceptionDo: [:signaledException | self error: 'should not evaluate this block' ]]
		on: TestResult failure
		do: [:failure | failureSignaled := true ].
		
	self assert: failureSignaled ! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'jmv 31/Jul/2023, 4:29:46 pm (UTC)'!
testShouldRaiseWithExceptionDoFailsWhenOtherExceptionTypeIsSignaled

	| exceptionToRaise |
	
	exceptionToRaise := Error new.
	
	[self 
		should: [ exceptionToRaise signal ]
		raise: ZeroDivide 
		withExceptionDo: [:signaledException | self error: 'should not evalaute this block' ]]
		on: Error
		do: [:anError | self assert: anError equals: anError ].! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'jmv 31/Jul/2023, 4:30:07 pm (UTC)'!
testShouldRaiseWithExceptionDoPassSignaledExceptionToAssertionsBlock

	| exceptionToRaise |
	
	exceptionToRaise := Error new.
	
	self 
		should: [ exceptionToRaise signal ]
		raise: exceptionToRaise class
		withExceptionDo: [:signaledException |
			self assert: signaledException equals: exceptionToRaise ].! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HernanWilkinson 20/Jan/2017, 12:51:45 am (UTC)'!
testShouldRaiseWithExceptionDoValuesAssertionsBlockWhenExceptionIsRaised

	| exceptionToRaise assertionsBlockEvaluated |
	
	exceptionToRaise := Error new.
	assertionsBlockEvaluated := false.
	
	self 
		should: [ exceptionToRaise signal ]
		raise: exceptionToRaise class
		withExceptionDo: [:signaledException |
			assertionsBlockEvaluated := true ].
		
	self assert: assertionsBlockEvaluated ! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 6/Feb/2019, 5:37:19 pm (UTC)'!
testShouldRaiseWithMessageTextDoesNotFailWithRightMessageText

	| messageText |
	
	messageText := 'some message'.
	
	self 
		shouldnt: [ 
			self 
				should: [ self error: messageText ]
				raise: Error
				withMessageText: messageText ]
		raise: TestResult failure 
		! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 6/Feb/2019, 5:38:00 pm (UTC)'!
testShouldRaiseWithMessageTextFailsWithDifferentMessageText

	self 
		should: [ 
			self 
				should: [ self error: 'some message' ]
				raise: Error
				withMessageText: '' ]
		raise: TestResult failure! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 17/Mar/2019, 10:42:30 am (UTC)'!
testShoulndFailErrorsWhenAnErrorIsSignaled

	self
		should: [ self shouldntFail: [ self fail ]]
		raise: TestResult failure! !

!TestCaseTest methodsFor: 'exception testing tests' stamp: 'HAW 17/Mar/2019, 10:41:26 am (UTC)'!
testShoulndFailPassesWhenNoErrorIsSignaled

	self shouldntFail: [ 1+2 ]! !

!TestCaseTest methodsFor: 'changes' stamp: 'HAW 15/May/2019, 10:33:28 pm (UTC)'!
testAssertChangesByPassesWhenActionChangesConditionByTheSpecifiedAmount

	|aCollection|

	aCollection := OrderedCollection new.
	
	self assert: [ aCollection add: 1; add: 2 ] changes: [ aCollection size ] by: 2! !

!TestCaseTest methodsFor: 'changes' stamp: 'HAW 15/May/2019, 10:34:29 pm (UTC)'!
testAssertChangesFromToPassesWhenActionChangesConditionFromAndToTheSpecifiedValues

	|aCollection|

	aCollection := OrderedCollection with: 1.
	
	self assert: [ aCollection add: 2 ] changes: [ aCollection size ] from: 1 to: 2! !

!TestCaseTest methodsFor: 'changes' stamp: 'HAW 15/May/2019, 10:33:34 pm (UTC)'!
testAssertChangesPassesWhenConditionIsAlteredByAction

	|aCollection|

	aCollection := OrderedCollection new.
	
	self assert: [ aCollection add: 1 ] changes: [ aCollection size ]! !

!TestCaseTest methodsFor: 'changes' stamp: 'HAW 15/May/2019, 10:33:39 pm (UTC)'!
testAssertDoeNotChangePassesWhenConditionIsNotAlteredByAction

	|aCollection|

	aCollection := OrderedCollection with: 1.
	
	self assert: [ aCollection sum ] doesNotChange: [ aCollection size ]! !

!TestCaseTest methodsFor: 'includes' stamp: 'jmv 20/Mar/2024, 10:35:09 pm (UTC)'!
testAssertIncludesFailsWhenElementIsNotIncludedInCollection

	| collection anElement |

	anElement := 1.
	collection := #().

	self should: [ self assert: collection includes: anElement ] 
		 raise: TestResult failure
		withExceptionDo: [ :anException | | expectedError |
			expectedError := collection asString, ' does not include ', anElement asString.
			self assert: (anException messageText endsWith: expectedError) ]! !

!TestCaseTest methodsFor: 'includes' stamp: 'HAW 15/May/2019, 10:50:04 pm (UTC)'!
testAssertIncludesShouldNotFailWhenElementIsInCollection
	
	| collection anElement |
	
	anElement _ 1.
	collection _ Array with: anElement.
	
	self 
		shouldnt: [ self assert: collection includes: anElement ] 
		raise: TestResult failure! !

!TestCaseTest methodsFor: 'should take less than' stamp: 'jmv 10/Mar/2022, 3:42:41 pm (UTC)'!
testShouldNotTakeMoreThanFailsWhenClosureTakesMoreThanTheLimit

	self shouldFail: [ self should: [(Delay forMilliseconds: 50) wait ] notTakeMoreThan: 10 milliSeconds ]! !

!TestCaseTest methodsFor: 'should take less than' stamp: 'HAW 15/May/2019, 11:07:04 pm (UTC)'!
testShouldNotTakeMoreThanPassesWhenClosureTakesLessThanTheLimit

	self shouldntFail: [ self should: [] notTakeMoreThan: 1 milliSeconds ]
		! !

!TestCaseTest methodsFor: 'symbols' stamp: 'jmv 19/Jun/2024, 1:39:01 pm (UTC)'!
testIsNotTestSelectorWhenHasParameters

	self deny: (TestCase isTestSelector: #test01:)! !

!TestCaseTest methodsFor: 'symbols' stamp: 'jmv 19/Jun/2024, 1:38:48 pm (UTC)'!
testIsNotTestSelectorWhenItDoesNotStartsWithTest

	self deny: (TestCase isTestSelector: #tes01)! !

!TestCaseTest methodsFor: 'symbols' stamp: 'jmv 19/Jun/2024, 1:38:23 pm (UTC)'!
testIsTestSelectorWhenStartsWithTestAndHasNoParameters

	self assert: (TestCase isTestSelector: #test01)! !

!TestSuiteTest methodsFor: 'assertions' stamp: 'jmv 31/Jul/2023, 4:30:31 pm (UTC)'!
assert: expectedTestSuite hasSameTestsAs: resultTestSuite

	| expectedTests resultTests |
	
	expectedTests := expectedTestSuite tests.
	resultTests := resultTestSuite tests.
	
	self assert: resultTests size equals: expectedTests size.
	self assert: (expectedTests allSatisfy: [:expectedTest | resultTests anySatisfy: [:resultTest | self is: expectedTest equalTo: resultTest ]]).! !

!TestSuiteTest methodsFor: 'assertions' stamp: 'jmv 13/Mar/2018, 12:41:35 am (UTC)'!
assertIncludesThisTest: aTestSuite 
	
	| thisTestSelector |
	
	thisTestSelector _ thisContext sender selector.
	^aTestSuite tests anySatisfy: [ :aTestCase | aTestCase class = self class and: [aTestCase selector = thisTestSelector ]]! !

!TestSuiteTest methodsFor: 'assertions' stamp: 'HAW 10/Feb/2017, 7:20:29 pm (UTC)'!
is: expectedTest equalTo: resultTest

	^expectedTest class = resultTest class and: [ expectedTest selector =  resultTest selector ]! !

!TestSuiteTest methodsFor: 'test support' stamp: 'HAW 29/Oct/2019, 3:13:21 pm (UTC)'!
hardCodedReferenceToSelfClass

	^TestSuiteTest ! !

!TestSuiteTest methodsFor: 'test support' stamp: 'HAW 3/Mar/2017, 8:37:00 pm (UTC)'!
quickMethodTest

	"stub for a quick method - Hernan"! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 10/Feb/2017, 7:22:49 pm (UTC)'!
test01SuiteForTestCaseClassHasTheSameTestAsCreatingTheSuiteFromTheTestCaseClass

	self assert: self class buildSuite hasSameTestsAs: (TestSuite forClass: self class)! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 10/Feb/2017, 7:24:17 pm (UTC)'!
test02SuiteForNoTestCaseClassUsesTestClassWithSameNameEndingWithTest

	self assert: self class buildSuite hasSameTestsAs: (TestSuite forClass: TestSuite)! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 29/Oct/2019, 3:12:11 pm (UTC)'!
test03SuiteForNoTestCaseClassAndNoTestClassLooksForReferencesInTests

	| objectTestSuite referencesToObject testCaseClasses |
	
	"I need to be sure that Object has no test class for the test to make sense - Hernan"
	self assert: self class testCaseClass isNil.
	
	objectTestSuite _ TestSuite forClass: self class.
	referencesToObject _ self class allCallsOn collect: [:aMethodReference | aMethodReference actualClass ]. 
	testCaseClasses _ (objectTestSuite tests collect: [:aTestCase | aTestCase class ]) asSet.

	self assert: objectTestSuite tests notEmpty.
	self assert: (testCaseClasses allSatisfy: [:aTestCaseClass | (aTestCaseClass is: #TestCaseClass) and: [ referencesToObject includes: aTestCaseClass ]])
	! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 10/Feb/2017, 7:41:52 pm (UTC)'!
test04SuiteForTestMethodIncludesOnlyTheMethod

	self assert: (TestSuite new addTest: (self class selector: thisContext selector)) hasSameTestsAs: (TestSuite forCompiledMethod: thisContext method)! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 10/Feb/2017, 7:56:00 pm (UTC)'!
test05SuiteForNoTestMethodIncludesTestCaseSendingTheCompiledMethodSelector

	| compiledMethodSuite compiledMethodSelector senders |
	
	compiledMethodSelector _ #addTest:.
	compiledMethodSuite _ TestSuite forCompiledMethod: (TestSuite compiledMethodAt: compiledMethodSelector).
	senders _ (Smalltalk allCallsOn: compiledMethodSelector) collect: [:aMethodReference | aMethodReference selector ].
	
	self assert: compiledMethodSuite tests notEmpty.
	self assert: (compiledMethodSuite tests allSatisfy: [:aTestCase | (aTestCase class is: #TestCaseClass) and: [ senders includes: aTestCase selector]])! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 10/Feb/2017, 8:39:30 pm (UTC)'!
test06ForSystemCategoryWithTestCasesIncludesOnlyTestCasesClasses

	| suite |
	
	suite _ TestSuite forSystemCategoryNamed: self class category using: SystemOrganization.
	
	"I just make a simple assertion that verifies this test is included - Hernan"
	self assertIncludesThisTest: suite ! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 10/Feb/2017, 8:41:00 pm (UTC)'!
test07ForSystemCategoryWithNoTestCasesIncludesTestClassesTests

	| suite |
	
	suite _ TestSuite forSystemCategoryNamed: TestSuite category using: SystemOrganization.
	
	"I just make a simple assertion that verifies this test is included - Hernan"
	self assertIncludesThisTest: suite ! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 10/Feb/2017, 8:41:08 pm (UTC)'!
test08ForMessageCategoryContainingTestCasesIncludesThoseTestCases

	| suite classOrganizer |
	
	classOrganizer _ self class organization.
	suite _ TestSuite forMessageCategoryNamed: (classOrganizer categoryOfElement: thisContext selector) of: self class categorizedWith: classOrganizer.

	"I just make a simple assertion that verifies this test is included - Hernan"
	self assertIncludesThisTest: suite ! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 10/Feb/2017, 8:38:46 pm (UTC)'!
test09ForMessageCategoryWihoutTestIsEmpty

	| suite classOrganizer |
	
	classOrganizer _ self class organization.
	suite _ TestSuite forMessageCategoryNamed: (classOrganizer categoryOfElement: #assert:hasSameTestsAs:) of: self class categorizedWith: classOrganizer.

	self assert: suite tests isEmpty! !

!TestSuiteTest methodsFor: 'tests - instance creation' stamp: 'HAW 21/Mar/2025, 1:40:58 am (UTC)'!
test10SuiteForClassCategoryTreeIncludesAllTestsInCategoryTree

	| categoryTreeSuite suiteFromCategoryRoot |
	
	categoryTreeSuite := TestSuite forSystemCategoryTreeOf: self class category using: SystemOrganization.
	suiteFromCategoryRoot := TestSuite forSystemCategoryNamed: (SystemOrganization rootCategoryOf: self class category) using: SystemOrganization.
	
	self 
		assert: (categoryTreeSuite tests collect: [ :test | test methodForTest ]) 
		equals: (suiteFromCategoryRoot tests collect: [ :test | test methodForTest ])! !

!SmalltalkCompleter methodsFor: '*ToolsTests-Autocompletion' stamp: 'NPM 17/Mar/2020, 7:12:11 am (UTC)'!
changePositionTo: newPosition 
	
	position _ newPosition! !
