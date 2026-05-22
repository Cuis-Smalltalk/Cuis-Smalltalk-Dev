Class {
	#name : 'ChangesTest',
	#superclass : 'DynamicallyCodeCreationTest',
	#category : 'BaseImageTests-Tools-Changes',
	#package : 'BaseImageTests',
	#tag : 'Tools-Changes'
}

{ #category : 'assertions' }
ChangesTest >> assertIsLogged: aString times: stringCount and: aBlock [

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
	

	
]

{ #category : 'assertions' }
ChangesTest >> assertIsLoggedOnce: aString [

	self assertIsLoggedOnce: aString and: [:logContents :nextPosition | ] 
]

{ #category : 'assertions' }
ChangesTest >> assertIsLoggedOnce: aString and: aBlock [

	self assertIsLogged: aString times: 1 and: aBlock
]

{ #category : 'assertions' }
ChangesTest >> assertIsLoggedTwice: aString [

	self assertIsLogged: aString times: 2 and: [:logContents :nextPosition | ] 
]

{ #category : 'user changes' }
ChangesTest >> changeUserChangesFileWhile: aBlock [

	^ Preferences
		withTemporaryValue: self userChangesForTestsFileExtension
		of: #userChangesFileNameExtension
		do: aBlock.
]

{ #category : 'test data' }
ChangesTest >> createTestDataClass [

	^ self createClassNamed: self testDataClassName 
		
]

{ #category : 'user changes' }
ChangesTest >> scanChangesFromFile [

	| fileStream changeList |

	fileStream := (DirectoryEntry userChangesDirectory // Smalltalk userChangesFileName) readStream.
	changeList := ChangeList new scanFile: fileStream from: 0 to: fileStream size.

	^ changeList changeList.

]

{ #category : 'setUp/tearDown' }
ChangesTest >> tearDown [

	self userChangesForTestsFile delete.
	super tearDown 
]

{ #category : 'log tests' }
ChangesTest >> test01AddingAClassShouldBeLogged [
	" Log format expected:

	!classDefinition: #ClassName category: 'Category' stamp: changeStamp!
	Object subclass: #ClassName
		instanceVariableNames: '...'
		classVariableNames: '...'
		poolDictionaries: '....'
		category: 'Category'!
	"
	| newClass |
	
	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.

		self assertIsLoggedOnce: '!classDefinition: #', self testDataClassName, ' category: #''', self classCategoryOfTestData,''' stamp:'.
		self assertIsLoggedOnce: newClass definition ] 
]

{ #category : 'scan tests' }
ChangesTest >> test01ScanNewClassChange [

	| newClass newClassChange |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClassChange := self scanChangesFromFile last.

		self assert: newClassChange isClassDefinition.
		self assert: newClassChange changeClass equals: newClass.
		self deny: newClassChange stamp isNil.
	] 
]

{ #category : 'log tests' }
ChangesTest >> test02AddingANewMethodShouldBeLogged [
	" Log format expected:

	!Class methodsFor: 'category' stamp: 'author stamp'!
	methodSourceCode
	"
	| aClass |
	
	self changeUserChangesFileWhile: [
		aClass := self createTestDataClass.
		aClass compile: 'm1 ^ 1' classified: 'a-category'.

		self assertIsLoggedOnce: '!', self testDataClassName, ' methodsFor: ''a-category'' stamp:'.
		self assertIsLoggedOnce: 'm1 ^ 1'] 

]

{ #category : 'scan tests' }
ChangesTest >> test02ScanNewMethodChange [

	| newMethodChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.

		newMethodChange := self scanChangesFromFile last.

		self assert: newMethodChange isMethodDefinition.
		self assert: newMethodChange methodSelector equals: #m1.
		self assert: newMethodChange changeClass equals: newClass.
		self deny: newMethodChange stamp isNil ] 
]

{ #category : 'log tests' }
ChangesTest >> test03ChangingAMethodShouldBeLogged [
	" Log format expected:

	!Class methodsFor: 'category' stamp: 'author stamp'!
	newMethodSourceCode
	"
	| aClass |
	
	self changeUserChangesFileWhile: [
		aClass := self createTestDataClass.
		aClass compile: 'm1 ^ 1' classified: 'a-category'.
		aClass compile: 'm1 ^ 2' classified: 'a-category'.

		self assertIsLoggedTwice: '!', self testDataClassName , ' methodsFor: ''a-category'' stamp:'.
		self assertIsLoggedOnce: 'prior: '.
		self assertIsLoggedOnce: 'm1 ^ 2' ] 
]

{ #category : 'scan tests' }
ChangesTest >> test03ScanMethodModifiedChange [

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

]

{ #category : 'log tests' }
ChangesTest >> test04RemovingAMethodShouldBeLogged [
	" Log format expected:

	!methodRemoval: #ClassName selector stamp: changeStamp!
	methodSourceCode
	"
	| newClass methodSource readStream |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.
		newClass removeSelector: #m1.

		self 
			assertIsLoggedOnce: '!methodRemoval: ',self testDataClassName, ' #m1 stamp:'
			and: [ :logContents :nextPosition |
				readStream := ReadStream on: logContents.
		 		readStream position: nextPosition.
				readStream nextLine.
				methodSource := readStream nextLine.
		
				self assert: methodSource equals: 'm1 ^ 1!' ]].
]

{ #category : 'scan tests' }
ChangesTest >> test04ScanMethodRemovalChange [

	| methodRemovalChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass compile: 'm1 ^ 1' classified: 'a-category'.
		newClass removeSelector: #m1.

		methodRemovalChange := self scanChangesFromFile last.

		self assert: methodRemovalChange isMethodDeletion.
		self assert: methodRemovalChange methodSelector equals: #m1.
		self deny: methodRemovalChange stamp isNil ]

]

{ #category : 'log tests' }
ChangesTest >> test05RemovingAClassShouldBeLogged [
	" Log format expected:

	!classRemoval: #ClassName stamp: changeStamp!
	classDefinitionMessage
	"
	| newClass definition loggedDefinition readStream |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		definition := newClass definition.
		newClass removeFromSystem.

		self 
			assertIsLoggedOnce: '!classRemoval: #', self testDataClassName, ' stamp:'
			and: [ :logContents :nextPosition |
				readStream := ReadStream on: logContents.
		 		readStream position: nextPosition.
				readStream nextLine.
				loggedDefinition := readStream upToEnd.
				
				self assert: loggedDefinition equals: definition,'!' ]].
]

{ #category : 'scan tests' }
ChangesTest >> test05ScanClassRemovalChange [

	| classRemovalChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass removeFromSystem.

		classRemovalChange := self scanChangesFromFile last.

		self assert: classRemovalChange isClassDeletion.
		self assert: classRemovalChange changeClassName equals: self testDataClassName.
		self assert: classRemovalChange isClassDeletion.
		self deny: classRemovalChange stamp isNil ]


]

{ #category : 'log tests' }
ChangesTest >> test06RenamingAClassShouldBeLogged [
	" Log format expected:

	!classRenamed: #OldName as: #NewName stamp  stamp:!
	Smalltalk renameClassNamed: #OldName as: #NewName
	"
	| newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass rename: #NewTestClass__.

		self assertIsLoggedOnce: '!classRenamed: #', self testDataClassName, ' as: #NewTestClass__ stamp:'.
		self assertIsLoggedOnce: 'Smalltalk renameClassNamed: #', self testDataClassName, ' as: #NewTestClass__'.
	]
]

{ #category : 'scan tests' }
ChangesTest >> test06ScanClassRenamedChange [

	| classRenamedChange newClass |

	self changeUserChangesFileWhile: [
		newClass := self createTestDataClass.
		newClass rename: #NewTestClass__.

		classRenamedChange := self scanChangesFromFile last.

		self assert: classRenamedChange isClassRenamed.
		self assert: classRenamedChange changeClassName equals: self testDataClassName.
		self assert: classRenamedChange newClassName equals: #NewTestClass__.
		self deny: classRenamedChange stamp isNil. ]

]

{ #category : 'test data' }
ChangesTest >> testDataClassName [

	^ #LogChangesTestClass__
]

{ #category : 'user changes' }
ChangesTest >> userChangesForTestsFile [

	^DirectoryEntry userChangesDirectory // 
	((FileIOAccessor default baseNameFor: Smalltalk imageName pathAndLocalName second), self userChangesForTestsFileExtension).
]

{ #category : 'user changes' }
ChangesTest >> userChangesForTestsFileExtension [

	^'.test.changes' 
]

{ #category : 'user changes' }
ChangesTest >> userChangesForTestsFileName [

	^(FileIOAccessor default baseNameFor: Smalltalk imageName), self userChangesForTestsFileExtension
]
