"
I add a number of facilities to basic Behaviors:
	Named instance variables
	Category organization for methods
	The notion of a name of this class (implemented as subclass responsibility)
	The maintenance of a ChangeSet, and logging changes on a file
	Most of the mechanism for fileOut.
	
I am an abstract class, in particular, my facilities are intended for inheritance by two subclasses, Class and Metaclass.

The slots 'organization' and 'methodDict' should ONLY be accessed by message in order for things to work during ImageSegment>>discoverActiveClasses (q.v.).
"
Class {
	#name : 'ClassDescription',
	#superclass : 'Behavior',
	#instVars : [
		'instanceVariables',
		'organization'
	],
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'utilities' }
ClassDescription class >> printPackageExtensionCategories [
	"In a bare image, without any packages, should print nothing
	ClassDescription printPackageExtensionCategories
	ClassOrganizer allInstancesDo: [:co | co removeEmptyCategories].
	"
	ClassDescription allSubInstances do: [ :class | | classOrganization |
		classOrganization := class organization.
		classOrganization categories do: [ :category |
				(classOrganization isPackageCategoryExtension: category) ifTrue: [
					{class. category} print ]]].
]

{ #category : 'compiling' }
ClassDescription >> acceptsLoggingOfCompilation [
	"weird name is so that it will come lexically before #compile, so that a clean build can make it through.  7/7/96 sw"

	^ true
]

{ #category : 'accessing method dictionary' }
ClassDescription >> addAndClassifySelector: selector withMethod: compiledMethod inCategory: category methodTimeStamp: aTimeStampString notifying: requestor [
	| priorMethodOrNil priorCategoryOrNil |
	priorMethodOrNil := self compiledMethodAt: selector ifAbsent: nil.
	priorCategoryOrNil := self whichCategoryIncludesSelector: selector.
	self addSelectorSilently: selector withMethod: compiledMethod.
	SystemChangeNotifier uniqueInstance doSilently: [self organization classify: selector under: category].
	priorMethodOrNil
		ifNil: [
			SystemChangeNotifier uniqueInstance 
				methodAdded: compiledMethod
				selector: selector
				inProtocol: category
				class: self
				methodTimeStamp: aTimeStampString
				requestor: requestor ]
		ifNotNil: [
			SystemChangeNotifier uniqueInstance 
				methodChangedFrom: priorMethodOrNil 
				to: compiledMethod 
				selector: selector 
				inClass: self 
				requestor: requestor.
			category = priorCategoryOrNil ifFalse: [
				SystemChangeNotifier uniqueInstance
					selectorRecategorized: selector
					from: priorCategoryOrNil
					to: category
					inClass: self ]].
]

{ #category : 'instance variables' }
ClassDescription >> addInstVarName: aString  [
	"Add the argument, aString, as one of the receiver's instance variables."

	self subclassResponsibility
]

{ #category : 'instance variables' }
ClassDescription >> allInstVarNamesEverywhere [
	"Answer the set of inst var names used by the receiver, all superclasses, and all subclasses"

	| aList |
	aList := OrderedCollection new.
	(self allSuperclasses , self withAllSubclasses asOrderedCollection) do:
		[:cls | aList addAll: cls instVarNames].
	^ aList asSet

	"BorderedBoxMorph allInstVarNamesEverywhere"
]

{ #category : 'accessing method dictionary' }
ClassDescription >> allMethodCategoriesIntegratedThrough: mostGenericClass [
	"Answer a list of all the method categories of the receiver and all its superclasses, up through mostGenericClass"

	| aColl |
	aColl := OrderedCollection new.
	self withAllSuperclasses do: [ :aClass |
		(aClass includesBehavior: mostGenericClass)
			ifTrue: [ aColl addAll: aClass methodCategories ]].
	
	^ aColl asSet asSortedCollection: [ :a :b | a asLowercase < b asLowercase ]

"ColorTileMorph allMethodCategoriesIntegratedThrough: TileMorph"
]

{ #category : 'method dictionary' }
ClassDescription >> allMethodsInCategory: aSymbol [
	"Answer a list of all the method categories of the receiver and all its superclasses"

	| aColl |
	aColl := OrderedCollection new.
	self withAllSuperclasses do: [ :aClass |
		aColl addAll:
			((aSymbol isNil or: [aSymbol == ClassOrganizer allCategory])
				ifTrue:
					[aClass organization allMethodSelectors]
				ifFalse:
					[aClass organization listAtCategoryNamed: aSymbol])].
	^ aColl asSet sorted

"
Morph allMethodsInCategory: #initialization
Morph allMethodsInCategory: ClassOrganizer allCategory :: size
Morph allMethodsInCategory: nil :: size
"
]

{ #category : 'system-support' }
ClassDescription >> allUnreferencedClassVariables [
	"Answer a list of the names of all the receiver's unreferenced class vars, including those defined in superclasses"
	| aList |
	aList := OrderedCollection new.
	self withAllSuperclasses reverseDo: [ :aClass |
		aClass classVarNames do: [ :var |
			(Smalltalk isThereAReferenceTo: (aClass classPool associationAt: var)) ifFalse: [ aList add: var ]]].
	^ aList
]

{ #category : 'accessing variables' }
ClassDescription >> allUnreferencedInstanceVariables [

	"Return a list of the instance variables known to the receiver which are not referenced in the receiver or any of its subclasses OR superclasses"

	^ self allInstVarNames reject: [ :instanceVariableName | | definingClass |
		definingClass := self classThatDefinesInstanceVariable: instanceVariableName.
		definingClass isInstanceVariableNamedReferencedInHierarchy: instanceVariableName ]
]

{ #category : 'instance variables' }
ClassDescription >> browseClassVarRefs  [
	"Put up a menu offering all class variable names; if the user chooses one, open up a message-list browser on all methods that refer to the selected class variable"

	| lines labelStream allVars index owningClasses |
	lines := OrderedCollection new.
	allVars := OrderedCollection new.
	owningClasses := OrderedCollection new.
	labelStream := String writeStream.
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars := class classVarNames asArray sort.
		vars do: [ :var |
			labelStream position = 0 ifFalse: [labelStream newLine].
			labelStream nextPutAll: var.
			allVars add: var.
			owningClasses add: class].
		vars isEmpty ifFalse: [ lines add: allVars size ]].
	labelStream contents isEmpty ifTrue: [^Smalltalk beep]. "handle nil superclass better"
	index := (PopUpMenu labels: labelStream contents lines: lines) startUpMenu.
	index = 0 ifTrue: [^ self].
	Smalltalk browseAllCallsOn:
		((owningClasses at: index) classPool associationAt: (allVars at: index))
]

{ #category : 'initialization' }
ClassDescription >> canUpdateFrom: oldClass [
	"If some compiled method that will become invalid is currently being executed,
	the shape conversion of instances is extremely insecure.
	Answer if migration of instances looks safe"
	| answer newMethod oldMethod selector newActualClass oldActualClass |
	answer := true.
	Processor 
		processesDo: [ :p | ]
		withStackFramestDo: [ :process :context |
			(context receiver isKindOf: oldClass) ifTrue: [
				oldActualClass := context receiver class.
				self withAllSubclassesDo: [ :c |
					c name = oldActualClass name ifTrue: [
						newActualClass := c ]].
				selector := context method selector.
				oldMethod := oldActualClass lookupSelector: selector.
				newMethod := newActualClass lookupSelector: selector.
				oldMethod = newMethod ifFalse: [
					('Class reshaping: ', context receiver class name, ' has some instance running #', selector, '. This CompiledMethod would become invalid.') print.
					answer := false ].
				]]
		runningProcessSearchStart: nil.
	^answer
]

{ #category : 'organization' }
ClassDescription >> category [
	"Answer the system organization category for the receiver."

	^SystemOrganization categoryOfElement: self name
]

{ #category : 'organization' }
ClassDescription >> category: requested  [
	"Categorize the receiver under the system category, cat, removing it from 
	any previous categorization."

	| oldCategory newCategory |
	requested isString ifFalse: [^self errorCategoryName].
	oldCategory := self category.
	newCategory := (requested isNil or: [ requested isEmpty ])
		ifFalse: [requested withBlanksTrimmed]
		ifTrue: [SystemOrganization class defaultClassCategory].
	SystemOrganization classify: self name under: newCategory.
	SystemChangeNotifier uniqueInstance classRecategorized: self from: oldCategory to: newCategory.
]

{ #category : 'accessing method dictionary' }
ClassDescription >> changeRecordsAt: selector [
	"Return a list of ChangeRecords for all versions of the method at selector. Source code can be retrieved by sending string to any one.  Return nil if the method is absent."

	"(Pen changeRecordsAt: #go:) collect: [:cRec | cRec string]"
	| aList |
	aList := VersionsBrowser new
			scanVersionsOf: (self compiledMethodAt: selector ifAbsent: [^ nil])
			class: self meta: self isMeta
			category: (self whichCategoryIncludesSelector: selector)
			selector: selector.
	^ aList ifNotNil: [aList changeList]
]

{ #category : 'instance variables' }
ClassDescription >> checkForInstVarsOK: instVarString [
	"Return true if instVarString does no include any names used in a subclass"
	| instVarArray |
	instVarArray := Scanner new scanFieldNames: instVarString.
	self allSubclasses do:
		[:cl | cl instVarNames do:
			[:n | (instVarArray includes: n)
				ifTrue: [self error: n , ' is already used in ' , cl name.
						^ false]]].
	^ true
]

{ #category : 'instance variables' }
ClassDescription >> chooseClassVarName  [
	"Present the user with a list of class variable names and answer the one selected, or nil if none"

	| lines labelStream  allVars index |
	lines := OrderedCollection new.
	allVars := OrderedCollection new.
	labelStream := String writeStream.
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars := class classVarNames asArray sort.
		vars do: [ :var |
			labelStream position = 0 ifFalse: [labelStream newLine].
			labelStream nextPutAll: var.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream contents isEmpty ifTrue: [^Smalltalk beep]. "handle nil superclass better"
	index := (PopUpMenu labels: labelStream contents lines: lines) startUpMenu.
	index = 0 ifTrue: [^ nil].
	^ allVars at: index
]

{ #category : 'instance variables' }
ClassDescription >> chooseDefiningInstanceVariableAlphabeticallyWith: aCaption thenDo: aBlock [

	^self
		chooseDefiningInstanceVariableAlphabeticallyWith: aCaption
		thenDo: aBlock
		ifNone: [ ^ self inform: 'There are no instance variables defined in ', self name ] 
]

{ #category : 'instance variables' }
ClassDescription >> chooseDefiningInstanceVariableAlphabeticallyWith: aCaption thenDo: aBlock ifNone: noneBlock [

	| vars index |
	"Put up a menu of the instance variables in the receiver, presented in alphabetical order, and when the user chooses one, evaluate aBlock with the chosen variable as its parameter."

	vars := self instVarNames sorted.
	vars isEmpty ifTrue: [^ noneBlock value ].

	index := (PopUpMenu labelArray: vars lines: #()) startUpWithCaption: aCaption.
	index = 0 ifTrue: [^ self].
	aBlock value: (vars at: index)
]

{ #category : 'instance variables' }
ClassDescription >> chooseInstVarAlphabeticallyThenDo: aBlock [
	| allVars index |
	"Put up a menu of all the instance variables in the receiver, presented in alphabetical order, and when the user chooses one, evaluate aBlock with the chosen variable as its parameter."

	allVars := self allInstVarNames sorted.
	allVars isEmpty ifTrue: [^ self inform: 'There are no
instance variables'].

	index := (PopUpMenu labelArray: allVars lines: #()) startUpWithCaption: 'Instance variables in
', self name.
	index = 0 ifTrue: [^ self].
	aBlock value: (allVars at: index)
]

{ #category : 'instance variables' }
ClassDescription >> chooseInstVarThenDo: aBlock [
	"Put up a menu of all the instance variables in the receiver, and when
the user chooses one, evaluate aBlock with the chosen variable as its
parameter.  If the list is 6 or larger, then offer an alphabetical
formulation as an alternative. triggered by a 'show alphabetically' item
at the top of the list."

	| lines labelStream allVars index count offerAlpha |
	(count := self allInstVarNames size) = 0 ifTrue: 
		[ ^ self inform: 'There are no\instance variables.' withNewLines ].

	allVars := OrderedCollection new.
	lines := OrderedCollection new.
	labelStream := String writeStream.

	(offerAlpha := count > 5)
		ifTrue: [
			lines add: 1.
			allVars add: 'show alphabetically'.
			labelStream nextPutAll: allVars first].
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars := class instVarNames.
		vars do: [ :var |
			labelStream position = 0 ifFalse: [labelStream newLine].
			labelStream nextPutAll: var.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	(lines notEmpty and: [ lines last = allVars size ]) ifTrue: [
		lines removeLast ].  "dispense with inelegant line beneath last item"
	index := (PopUpMenu labels: labelStream contents lines: lines)
startUpWithCaption: 'Instance variables in
', self name.
	index = 0 ifTrue: [^ self].
	(index = 1 and: [offerAlpha]) ifTrue: [
		^ self chooseInstVarAlphabeticallyThenDo: aBlock].
	aBlock value: (allVars at: index)
]

{ #category : 'fileIn/Out' }
ClassDescription >> classComment: aString [
	"Store the comment, aString or Text or RemoteString, associated with the class we are orgainzing.  Empty string gets stored only if had a non-empty one before."
	^ self classComment: aString stamp: '<historical>'
]

{ #category : 'fileIn/Out' }
ClassDescription >> classComment: aString stamp: aStamp [
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| ptr header oldCommentRemoteStr |
	aString isRemote ifTrue: [
		SystemChangeNotifier uniqueInstance classCommented: self.
		^ self organization classComment: aString stamp: aStamp].

	oldCommentRemoteStr := self organization commentRemoteStr.
	(aString isEmpty) & (oldCommentRemoteStr == nil) ifTrue: [^ self organization classComment: nil].
		"never had a class comment, no need to write empty string out"

	ptr := oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].
	SourceFiles ifNotNil: [ | file |
		(file := SourceFiles at: 2) ifNotNil: [
			file setToEnd; newLine; nextPut: $!.	"directly"
			header := String streamContents: [:strm | strm nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
				aStamp storeOn: strm.
				strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
			file nextChunkPut: header]].
	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.
	Smalltalk userChangesAppendDo: [ :stream |
		stream newLine; nextPut: $!.	"directly"
		header := String streamContents: [:strm | strm nextPutAll: self name;
			nextPutAll: ' commentStamp: '.
			aStamp storeOn: strm.
			strm nextPutAll: ' prior: '; nextPutAll: ptr printString].
		stream nextChunkPut: header.
		stream newLine; nextChunkPut: aString ].
	SystemChangeNotifier uniqueInstance classCommented: self.
]

{ #category : 'class variables' }
ClassDescription >> classPool [

	self subclassResponsibility 
]

{ #category : 'instance variables' }
ClassDescription >> classThatDefinesClassVariable: classVarName [
	"Answer the class that defines the given class variable"

	(self classPool includesKey: classVarName asSymbol) ifTrue: [^ self]. 
	^ superclass ifNotNil: [superclass classThatDefinesClassVariable: classVarName]
]

{ #category : 'instance variables' }
ClassDescription >> classThatDefinesInstanceVariable: instVarName [
	(instanceVariables notNil and: [instanceVariables includes: instVarName asPlainString]) ifTrue: [^ self]. 
	^ superclass ifNotNil: [superclass classThatDefinesInstanceVariable: instVarName]
]

{ #category : 'printing' }
ClassDescription >> classVariablesString [
	"Answer a string of my class variable names separated by spaces."

	^String streamContents: [ :stream | 
		self classPool keys sort 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : 'accessing' }
ClassDescription >> classVersion [
	"Default.  Any class may return a later version to inform readers that use ReferenceStream.  8/17/96 tk"
	"This method allows you to distinguish between class versions when the shape of the class 
	hasn't changed (when there's no change in the instVar names).
	In the conversion methods you usually can tell by the inst var names 
	what old version you have. In a few cases, though, the same inst var 
	names were kept but their interpretation changed (like in the layoutFrame).
	By changing the class version when you keep the same instVars you can 
	warn older and newer images that they have to convert."
	^ 0
]

{ #category : 'accessing class hierarchy' }
ClassDescription >> classesThatImplementAllOf: selectorSet [
	"Return an array of any classes that implement all the messages in selectorSet."

	| found remaining |
	found := OrderedCollection new.
	selectorSet do:
		[:sel | (self methodDict includesKey: sel) ifTrue: [found add: sel]].
	found isEmpty
		ifTrue: [^ self subclasses inject: Array new
						into: [:subsThatDo :sub |
							subsThatDo , (sub classesThatImplementAllOf: selectorSet)]]
		ifFalse: [remaining := selectorSet copyWithoutAll: found.
				remaining isEmpty ifTrue: [^ Array with: self].
				^ self subclasses inject: Array new
						into: [:subsThatDo :sub |
							subsThatDo , (sub classesThatImplementAllOf: remaining)]]
]

{ #category : 'accessing' }
ClassDescription >> comment [
	"Answer the receiver's comment. (If missing, supply a template) "
	^self smalltalkComment.
]

{ #category : 'accessing' }
ClassDescription >> comment: aStringOrText [
	"Set the receiver's comment to be the argument, aStringOrText."

	self theNonMetaClass classComment: aStringOrText asPlainString.
]

{ #category : 'accessing' }
ClassDescription >> comment: aStringOrText stamp: aStamp [
	"Set the receiver's comment to be the argument, aStringOrText."

	self theNonMetaClass classComment: aStringOrText asPlainString stamp: aStamp.
]

{ #category : 'fileIn/Out' }
ClassDescription >> commentStamp: changeStamp [
	self organization commentStamp: changeStamp.
    ^ self commentStamp: changeStamp prior: 0
]

{ #category : 'fileIn/Out' }
ClassDescription >> commentStamp: changeStamp prior: indexAndOffset [
	"Prior source link ignored when filing in."

	^ ClassCommentReader new setClass: self
				category: #Comment
				changeStamp: changeStamp

]

{ #category : 'compiling' }
ClassDescription >> compile: code classified: heading  [
	"Compile the argument, code, as source code in the context of the receiver and
	install the result in the receiver's method dictionary under the classification 
	indicated by the second argument, heading. nil is to be notified if an error occurs.
	The argument code is either a string or an object that converts to a string or a
	PositionableStream on an object that converts to a string."

	^self
		compile: code
		classified: heading
		notifying: nil
]

{ #category : 'compiling' }
ClassDescription >> compile: text classified: category notifying: requestor [
	| stamp |
	stamp := self acceptsLoggingOfCompilation ifTrue: [Utilities changeStamp] ifFalse: [nil].
	^ self compile: text classified: category
		withStamp: stamp notifying: requestor

 
]

{ #category : 'compiling' }
ClassDescription >> compile: text classified: category withStamp: changeStamp libraryName: aStringOrNil notifying: requestor [
	^ self compile: text classified: category withStamp: changeStamp libraryName: aStringOrNil notifying: requestor logSource: self acceptsLoggingOfCompilation
]

{ #category : 'compiling' }
ClassDescription >> compile: code classified: category withStamp: changeStamp libraryName: aStringOrNil notifying: requestor logSource: logSource [
	
	| methodAndNode selector newMethod isExistingMethod updatedCode libName |
	methodAndNode := self basicCompile: code notifying: requestor 
							trailer: self defaultMethodTrailer ifFail: [^nil].
	selector := methodAndNode selector.
	newMethod := methodAndNode method.
	libName := aStringOrNil.
	isExistingMethod := self includesSelector: selector.
	isExistingMethod
		ifTrue: [
			(self theNonMetaClass isOkToChangeMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method modification not allowed'].
			"If libraryName is provided, it takes precedence. If not, assume it doesn't change."
			libName isNil ifTrue: [ libName := (self compiledMethodAt: selector) propertyValueAt: #libraryName ]]
		ifFalse: [
			(self theNonMetaClass isOkToAddMethod: selector isMeta: self isMeta)
				ifFalse: [self error: 'Method addition not allowed']].
	libName ifNotNil: [ newMethod propertyValueAt: #libraryName put: libName ].
	logSource ifTrue: [
		"Re-fetching source code may be needed if, for example, some variable declaration was corrected during compilation."
		updatedCode := CodeProvider normalizeCodeForSaving: (requestor ifNil: [code] ifNotNil: [requestor text]) in: self.
		self logMethodSource: updatedCode forMethodWithNode: methodAndNode 
			inCategory: category withStamp: changeStamp libraryName: libName
			notifying: requestor.
	].
	self addAndClassifySelector: selector withMethod: newMethod
		inCategory: category methodTimeStamp: changeStamp notifying: requestor.
	^ selector
]

{ #category : 'compiling' }
ClassDescription >> compile: text classified: category withStamp: changeStamp notifying: requestor [

	^ self compile: text classified: category withStamp: changeStamp libraryName: nil notifying: requestor logSource: self acceptsLoggingOfCompilation
]

{ #category : 'compiling' }
ClassDescription >> compile: code notifying: requestor  [
	"Refer to the comment in Behavior|compile:notifying:." 

	^self compile: code
		 classified: ClassOrganizer defaultMethodCategory
		 notifying: requestor
]

{ #category : 'compiling' }
ClassDescription >> compileSilently: code classified: category [
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: code classified: category notifying: nil.
]

{ #category : 'compiling' }
ClassDescription >> compileSilently: code classified: category notifying: requestor [
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ SystemChangeNotifier uniqueInstance 
		doSilently: [self compile: code classified: category withStamp: nil libraryName: nil notifying: requestor logSource: false].
]

{ #category : 'copying' }
ClassDescription >> copy: sel from: class  [
	"Install the method associated with the first argument, sel, a message 
	selector, found in the method dictionary of the second argument, class, 
	as one of the receiver's methods. Classify the message under -As yet not 
	classified-."

	self copy: sel
		from: class
		classified: nil
]

{ #category : 'copying' }
ClassDescription >> copy: sel from: class classified: cat  [
	"Install the method associated with the first arugment, sel, a message 
	selector, found in the method dictionary of the second argument, class, 
	as one of the receiver's methods. Classify the message under the third 
	argument, cat."

	| code category |
	"Useful when modifying an existing class"
	code := class sourceCodeAt: sel.
	code ifNotNil: [
			category := cat
				ifNil: [ class organization categoryOfElement: sel].
			(self methodDict includesKey: sel)
				ifTrue: [code = (self sourceCodeAt: sel) 
							ifFalse: [self error: self name 
										, ' ' 
										, sel 
										, ' will be redefined if you proceed.']].
			self compile: code classified: category]
]

{ #category : 'copying' }
ClassDescription >> copyAll: selArray from: class  [
	"Install all the methods found in the method dictionary of the second argument, class,
	as the receiver's methods.  Classify the messages under -as yet not classified-"

	self copyAll: selArray
		from: class
		classified: nil
]

{ #category : 'copying' }
ClassDescription >> copyAll: selArray from: class classified: cat  [
	"Install all the methods found in the method dictionary of the second argument, class,
	as the receiver's methods.  Classify the messages under the third argument, cat."

	selArray do: 
		[:s | self copy: s
				from: class
				classified: cat]
]

{ #category : 'copying' }
ClassDescription >> copyAllCategoriesFrom: aClass  [
	"Specify that the categories of messages for the receiver include all of those found
	in the class, aClass.  Install each of the messages found in these categories into the
	method dictionary of the receiver, classified under the appropriate categories."

	aClass organization categories do: [:cat | self copyCategory: cat from: aClass]
]

{ #category : 'copying' }
ClassDescription >> copyCategory: cat from: class  [
	"Specify that one of the categories of messages for the receiver is cat, as 
	found in the class, class. Copy each message found in this category."

	self copyCategory: cat
		from: class
		classified: cat
]

{ #category : 'copying' }
ClassDescription >> copyCategory: cat from: aClass classified: newCat  [
	"Specify that one of the categories of messages for the receiver is the third argument,
	newCat.  Copy each message found in the category cat in class aClass into this
	new category."

	self copyAll: (aClass organization listAtCategoryNamed: cat)
		from: aClass
		classified: newCat
]

{ #category : 'copying' }
ClassDescription >> copyMethodDictionaryFrom: donorClass [
	"Copy the method dictionary of the donor class over to the receiver"

	methodDict := donorClass copyOfMethodDictionary.
	self organization: donorClass organization copy
]

{ #category : 'fileIn/Out' }
ClassDescription >> definition [
	"Answer a String that defines the receiver."

	^String streamContents: [ :strm |
		strm
			nextPutAll: (superclass
				ifNotNil: [ superclass name ]
				"Avoid hardcoding 'ProtoObject', for robusness against improbable rename."
				ifNil: [ ProtoObject name asString ]);
			nextPutAll: self kindOfSubclass;
			store: self name.
		strm
			newLine;
			tab;
			nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString.
		strm
			newLine;
			tab;
			nextPutAll: 'classVariableNames: ';
			store: self classVariablesString.
		strm
			newLine;
			tab;
			nextPutAll: 'poolDictionaries: ';
			store: self sharedPoolsString.
		strm
			newLine;
			tab;
			nextPutAll: 'category: ';
			store: self category asString.

		superclass ifNil: [ 
			strm nextPutAll: '.'; newLine.
			strm nextPutAll: self name.
			strm space; nextPutAll: 'superclass: nil' ]]
]

{ #category : 'fileIn/Out' }
ClassDescription >> definitionPreamble [

	^self definitionPreambleWithoutStamp, Utilities changeStampField
]

{ #category : 'fileIn/Out' }
ClassDescription >> definitionPreambleWithoutStamp [

	^'classDefinition: ', self name printString, ' category: ', self category printString
]

{ #category : 'initialization' }
ClassDescription >> doneCompiling [
	"A ClassBuilder has finished the compilation of the receiver.
	This message is a notification for a class that needs to do some
	cleanup / reinitialization after it has been recompiled."
]

{ #category : 'private' }
ClassDescription >> errorCategoryName [
	self error: 'Category name must be a String'
]

{ #category : 'fileIn/Out' }
ClassDescription >> fileOutCategory: catName [
	"FileOut the named category"

	DirectoryEntry fileOutsDirQuery: (self name , '-' , catName , '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self fileOutCategory: catName on: fileStream moveSource: false toFile: 0 ].
]

{ #category : 'fileIn/Out' }
ClassDescription >> fileOutCategory: aSymbol on: aFileStream moveSource: moveSource toFile: fileIndex  [
	"File a description of the receiver's category, aString, onto aFileStream. If 
	moveSource, is true, then set the method source pointer to the new file position.
	Note when this method is called with moveSource=true, it is condensing the
	.sources file, and should only write one preamble per method category."

	| selectors |

	aFileStream newLine.
	selectors := (aSymbol isNil or: [aSymbol == ClassOrganizer allCategory])
				ifTrue: [ self organization allMethodSelectors ]
				ifFalse: [ self organization listAtCategoryNamed: aSymbol ].

	"Overridden to preserve author stamps in sources file regardless"
	selectors do: [:sel |
		self printMethodChunk: sel 
			withPreamble: true
			on: aFileStream 
			moveSource: moveSource 
			toFile: fileIndex].
	^ self
]

{ #category : 'fileIn/Out' }
ClassDescription >> fileOutChangedMessages: aSet on: aFileStream  [
	"File a description of the messages of the receiver that have been 
	changed (i.e., are entered into the argument, aSet) onto aFileStream."

	self fileOutChangedMessages: aSet
		on: aFileStream
		moveSource: false
		toFile: 0
]

{ #category : 'fileIn/Out' }
ClassDescription >> fileOutChangedMessages: aSet on: aFileStream moveSource: moveSource toFile: fileIndex [
	"File a description of the messages of this class that have been
	changed (i.e., are entered into the argument, aSet) onto aFileStream.  If
	moveSource, is true, then set the method source pointer to the new file position.
	Note when this method is called with moveSource=true, it is condensing the
	.changes file, and should only write a preamble for every method."

	| org categories |

	org := self organization.
	categories := org categories.

	categories ifNotNil: [ categories do: [ :cat |  | sels |
		sels := (org listAtCategoryNamed: cat) select: [:sel | aSet includes: sel].
		sels do: [:sel |
			self printMethodChunk: sel withPreamble: true on: aFileStream moveSource: moveSource toFile: fileIndex]]]
]

{ #category : 'fileIn/Out' }
ClassDescription >> fileOutMethod: selector [
	"Write source code of a single method on a file.  Make up a name for the file."

	| nameBody |
	(selector == #Comment) ifTrue: [^ self inform: 'Sorry, cannot file out class comment in isolation.'].
	(self includesSelector: selector) ifFalse: [^ self error: 'Selector ', selector asPlainString, ' not found'].
	nameBody := self name , '-' , (selector copyReplaceAll: ':' with: '').
	DirectoryEntry fileOutsDirQuery: (nameBody asFileName, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self printMethodChunk: selector withPreamble: true
			on: fileStream moveSource: false toFile: 0 ].
]

{ #category : 'fileIn/Out' }
ClassDescription >> fileOutOn: aFileStream  [
	"File a description of the receiver on aFileStream."

	self fileOutOn: aFileStream
		moveSource: false
		toFile: 0
]

{ #category : 'fileIn/Out' }
ClassDescription >> fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex  [
	"File a description of the receiver on aFileStream. If the boolean argument,
	moveSource, is true, then set the trailing bytes to the position of aFileStream and
	to fileIndex in order to indicate where to find the source code."

	^self
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: true
		sortMethodsByCategory: true.
]

{ #category : 'fileIn/Out' }
ClassDescription >> fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: ignoredBool sortMethodsByCategory: byCategory [
	"File a description of the receiver on aFileStream. If the boolean
	argument, moveSource, is true, then set the trailing bytes to the position
	of aFileStream and to fileIndex in order to indicate where to find the
	source code."

	| categories |

	aFileStream nextPut: $!; nextChunkPut: self definitionPreambleWithoutStamp; newLine.
	aFileStream nextChunkPut: self definition.

	self organization
		putCommentOnFile: aFileStream
		numbered: fileIndex
		moveSource: moveSource
		forClass: self.
	categories := byCategory
		ifTrue: [self organization categories ifNil: [#()]]
		ifFalse: [{ClassOrganizer allCategory}].
	categories do: [ :heading |
		self fileOutCategory: heading
			on: aFileStream
			moveSource: moveSource
			toFile: fileIndex ].
]

{ #category : 'fileIn/Out' }
ClassDescription >> fileOutOrganizationOn: aFileStream [
	"File a description of the receiver's organization on aFileStream."

	self fileOutOrganizationOn: aFileStream excludingExtensions: false
]

{ #category : 'fileIn/Out' }
ClassDescription >> fileOutOrganizationOn: aFileStream excludingExtensions: shouldExcludeExtensions [
	"File a description of the receiver's organization on aFileStream.
	Excludes extentions categories if shouldExcludeExtensions"

	| organizationString |
	
	aFileStream newLine; nextPut: $!.
	aFileStream nextChunkPut: self name, ' reorganize'; newLine.
	organizationString := String streamContents: [ :aStream |
		self organization printOn: aStream excludingExtensions: shouldExcludeExtensions ].
	aFileStream nextChunkPut: organizationString; newLine
]

{ #category : 'instance variables' }
ClassDescription >> forceNewFrom: anArray [
    "Create a new instance of the class and fill
    its instance variables up with the array."
    | object max |

    object := self new.
    max := self instSize.
    anArray withIndexDo: [:each :index |
        index > max ifFalse:
            [object instVarAt: index put: each]].
    ^ object
]

{ #category : 'accessing' }
ClassDescription >> hasComment [
	"return whether this class truly has a comment other than the default"
	| org |
	org := self theNonMetaClass organization.
	^org classComment notNil and: [
		org classComment isEmpty not ].

]

{ #category : 'testing' }
ClassDescription >> hasDefinition [
	^ true
]

{ #category : 'accessing method dictionary' }
ClassDescription >> induceMDFault [
	"Stache a copy of the methodDict in the organization slot (hack!),
	and set the methodDict to nil.  This will induce an MD fault on any message send.
	See: ClassDescription>>recoverFromMDFault
	and ImageSegment>>discoverActiveClasses."

	organization := Array with: methodDict with: organization.
	methodDict := nil.
	self flushCache
]

{ #category : 'private' }
ClassDescription >> instVarMappingFrom: oldClass [
	"Return the mapping from instVars of oldClass to new class that is used for converting old instances of oldClass."
	| oldInstVarNames |
	oldInstVarNames := oldClass allInstVarNames.
	^self allInstVarNames 
			collect: [:instVarName | oldInstVarNames indexOf: instVarName].

]

{ #category : 'instance variables' }
ClassDescription >> instVarNameForIndex: index [
	"Answer the named instance variable with index index or nil if none."

	| superInstSize |
	index > self instSize ifTrue: [^nil].
	superInstSize := superclass ifNil: [0] ifNotNil: [superclass instSize].
	index > superInstSize ifTrue: [
		^instanceVariables at: index - superInstSize].
	^superclass ifNotNil: [ superclass instVarNameForIndex: index ]
]

{ #category : 'accessing' }
ClassDescription >> instVarNames [
	"Answer an Array of the receiver's instance variable names."

	^instanceVariables ifNil: [ #() ]
]

{ #category : 'compiling' }
ClassDescription >> instVarNamesAndOffsetsDo: aBinaryBlock [
	"This is part of the interface between the compiler and a class's instance or field names.
	 The class should enumerate aBinaryBlock with the instance variable name strings and
	 their integer offsets.  The order is important. Names evaluated later will override the
	 same names occurring earlier."

	| superInstSize |
	(superInstSize := superclass ifNotNil: [superclass instSize] ifNil: [0]) > 0 ifTrue: [
		superclass instVarNamesAndOffsetsDo: aBinaryBlock].
	1 to: self instSize - superInstSize do: [ :i | 
		aBinaryBlock value: (instanceVariables at: i) value: i + superInstSize]
]

{ #category : 'printing' }
ClassDescription >> instanceVariablesString [
	"Answer a string of my instance variable names separated by spaces."

	^String streamContents: [ :stream |
		self instVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : 'compiling' }
ClassDescription >> isOkToAddMethod: selector isMeta: isMeta [
	"A hook allowing some classes to disallow and/or react to addition of certain selectors before the change is committed"
	^true
]

{ #category : 'compiling' }
ClassDescription >> isOkToChangeMethod: selector isMeta: isMeta [
	"A hook allowing some classes to disallow and/or react to recompilation of certain selectors before the change is committed"
	^true
]

{ #category : 'compiling' }
ClassDescription >> isOkToRemoveMethod: selector isMeta: isMeta [
	"A hook allowing some classes to disallow and/or react to removal of certain selectors before the change is committed"
	^true
]

{ #category : 'source code management' }
ClassDescription >> linesOfCode [
"
Object linesOfCode
"
	"An approximate measure of lines of.
	Includes comments, but excludes blank lines.
	If asked to a class, also include its metaclass (i.e. the class side).
	If asked to a metaclass (the class side), don't include the class (the instance side)."

	| lines |
	lines := 0.
	self selectorsDo: [ :sel |
		lines := lines + (self compiledMethodAt: sel) linesOfCode ].
	^self isMeta
		ifTrue: [ lines]
		ifFalse: [ lines + self class linesOfCode].
"
(SystemOrganization categories select: [:c | 'Kernel*' match: c]) sum: [:c |
	(SystemOrganization superclassOrderIn: c) sum: [:cl | cl linesOfCode]]
"
"
Smalltalk allClasses sum: [:cl | cl linesOfCode]
"
]

{ #category : 'accessing' }
ClassDescription >> localBindingOf: varNameSymbol [

	self subclassResponsibility 
]

{ #category : 'private' }
ClassDescription >> logMethodSource: code forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp libraryName: aStringOrNil notifying: requestor [
	| priorMethodOrNil |

	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.

	aCompiledMethodWithNode method
		putSource: code
		fromParseNode: aCompiledMethodWithNode node
		class: self
		category: category
		withStamp: changeStamp
		libraryName: aStringOrNil
		inFile: 2
		priorMethod: priorMethodOrNil.
]

{ #category : 'source code management' }
ClassDescription >> messageSendsCount [
"
Object messageSendsCount
"
	"Number of message sends in code
	If asked to a class, also include its metaclass (i.e. the class side).
	If asked to a metaclass (the class side), don't include the class (the instance side)."

	| lines |
	lines := 0.
	self selectorsDo: [ :sel |
		lines := lines + (self compiledMethodAt: sel) messageSendsCount ].
	^self isMeta
		ifTrue: [ lines]
		ifFalse: [ lines + self class messageSendsCount].
"
(SystemOrganization categories select: [:c | 'Kernel*' match: c]) sum: [:c |
	(SystemOrganization superclassOrderIn: c) sum: [:cl | cl messageSendsCount]]
"
"
Smalltalk allClasses sum: [:cl | cl messageSendsCount]
"
]

{ #category : 'source code management' }
ClassDescription >> messageSendsCountInCategory: aCategory [
	"Number of message sends in code for methods in a method category.
	Object messageSendsCountInCategory: 'accessing'
	"

	| lines |
	lines := 0.
	(self selectorsInCategory: aCategory) do: [ :sel |
		lines := lines + (self compiledMethodAt: sel) messageSendsCount ].
	^lines
]

{ #category : 'source code management' }
ClassDescription >> messagesCountInCategory: aCategory [
	"Number of methods in a method category.
	Object messagesCountInCategory: 'accessing'
	"

	| lines |
	lines := 0.
	(self selectorsInCategory: aCategory) do: [ :sel |
		lines := lines + 1 ].
	^lines
]

{ #category : 'accessing method dictionary' }
ClassDescription >> methodCategories [

	^self organization categories
]

{ #category : 'accessing method dictionary' }
ClassDescription >> methodCategoriesAsSortedCollection [

	^self methodCategories asSortedCollection: [ :leftCategory :rightCategory | leftCategory asLowercase < rightCategory asLowercase ]

]

{ #category : 'fileIn/Out' }
ClassDescription >> methods [
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*
	Answer a ClassCategoryReader for compiling messages that are not classified.
	Some chunk files may not include class category in the class declarations."

	^ ClassCategoryReader new setClass: self category: ClassOrganizer defaultMethodCategory.
]

{ #category : 'fileIn/Out' }
ClassDescription >> methodsFor: categoryName [
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*
	Answer a ClassCategoryReader for compiling the messages in the category, categoryName, of the receiver."

	^ ClassCategoryReader new setClass: self category: categoryName asSymbol

	"(False methodsFor: 'logical operations') inspect"
]

{ #category : 'fileIn/Out' }
ClassDescription >> methodsFor: categoryName library: libraryName stamp: changeStamp [
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*"

	^ self methodsFor: categoryName library: libraryName stamp: changeStamp prior: 0
]

{ #category : 'fileIn/Out' }
ClassDescription >> methodsFor: categoryName library: libraryName stamp: changeStamp prior: indexAndOffset [
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*
	Prior source link ignored when filing in."

	^ ClassCategoryReader new
			setClass: self
			category: categoryName asSymbol
			changeStamp: changeStamp
			libraryName: libraryName

"Most importantly, return the new ClassCategoryReader, so a fileIn will let it seize control.  So method will be placed in the proper category.  See the transfer of control where ReadWriteStream fileIn calls scanFrom:"
]

{ #category : 'fileIn/Out' }
ClassDescription >> methodsFor: categoryName stamp: changeStamp [
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*"

	^ self methodsFor: categoryName stamp: changeStamp prior: 0
]

{ #category : 'fileIn/Out' }
ClassDescription >> methodsFor: categoryName stamp: changeStamp prior: indexAndOffset [
	"May appear in method preambles in chunk files. Therefore may be called during #fileIn.
	See #methods, #methodsFor:*, scanCategory:*
	Prior source link ignored when filing in."
	
	^ ClassCategoryReader new setClass: self
				category: categoryName asSymbol
				changeStamp: changeStamp

"Most importantly, return the new ClassCategoryReader, so a fileIn will let it seize control.  So method will be placed in the proper category.  See the transfer of control where ReadWriteStream fileIn calls scanFrom:"
]

{ #category : 'fileIn/Out' }
ClassDescription >> moveChangesTo: newFile  [
	"Used in the process of condensing changes, this message requests that 
	the source code of all methods of the receiver that have been changed 
	should be moved to newFile.
	
	Write the class's definition before the changes if there are any so that the changes file is a potential application recovery vehicle, not just a crash recovery vehicle."

	| changes |
	changes := self methodDict keys select: [:sel | (self methodDict at: sel) fileIndex > 1].
	
         changes isEmpty ifTrue:
                 [^self].
         newFile newLine; newLine; nextChunkPut: self definition; newLine.

	self fileOutChangedMessages: changes asSet
		on: newFile
		moveSource: true
		toFile: 2
]

{ #category : 'private' }
ClassDescription >> newInstanceFrom: oldInstance variable: variable size: instSize map: map [
	"Create a new instance of the receiver based on the given old instance.
	The supplied map contains a mapping of the old instVar names into
	the receiver's instVars"
	| new |
	variable
		ifTrue: [new := self basicNew: oldInstance basicSize]
		ifFalse: [new := self basicNew].
	1 to: instSize do: 
		[:offset |  (map at: offset) > 0 ifTrue:
			[new instVarAt: offset
					put: (oldInstance instVarAt: (map at: offset))]].
	variable 
		ifTrue: [1 to: oldInstance basicSize do: 
					[:offset |
					new basicAt: offset put: (oldInstance basicAt: offset)]].
	^new
]

{ #category : 'release' }
ClassDescription >> obsolete [
	"Make the receiver obsolete."
	superclass removeSubclass: self.
	self organization: nil.
	super obsolete.
]

{ #category : 'organization' }
ClassDescription >> organization [
	"Answer the instance of ClassOrganizer that represents the organization 
	of the messages of the receiver."

	organization ifNil:
		[self organization: (ClassOrganizer withUnclassifiedSelectors: self methodDict keys sort)].
	(organization isMemberOf: Array) ifTrue:
		[self recoverFromMDFaultWithTrace].
	
	"Making sure that subject is set correctly. It should not be necessary."
	organization ifNotNil: [organization setSubject: self].
	^ organization
]

{ #category : 'initialization' }
ClassDescription >> organization: aClassOrg [
	"Install an instance of ClassOrganizer that represents the organization of the messages of the receiver."

	aClassOrg ifNotNil: [aClassOrg setSubject: self].
	organization := aClassOrg
]

{ #category : 'fileIn/Out' }
ClassDescription >> printCategoryChunk: category on: aFileStream withStamp: changeStamp libraryName: aStringOrNil priorMethod: priorMethod [
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	Always include a stamp, even if it is empty.
	It may also have a libraryName."

	aFileStream newLine; nextPut: $!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		aStringOrNil ifNotNil: [
			strm nextPutAll: ' library: '; print: aStringOrNil ].
		strm nextPutAll: ' stamp: '; print: (changeStamp ifNil: ['']).
		priorMethod notNil ifTrue: [
			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer].
		]).
]

{ #category : 'fileIn/Out' }
ClassDescription >> printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex [
	"Copy the source code for the method associated with selector onto the fileStream.
	If moveSource true, then also set the source code pointer of the method."

	| compiledMethod oldPos newPos sourceFile endPos methodCode methodCodeText codeStyler |
	compiledMethod := self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR  -  THIS METHOD IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ outStream].

	doPreamble ifTrue: [
		self
			printCategoryChunk: (self organization categoryOfElement: selector)
			on: outStream
			withStamp: compiledMethod authorAndStamp
			libraryName: (compiledMethod propertyValueAt: #libraryName)
			priorMethod: nil.
		outStream newLine ].

	((compiledMethod fileIndex = 0
		or: [(SourceFiles at: compiledMethod fileIndex) == nil])
		or: [(oldPos := compiledMethod filePosition) = 0])
			ifTrue: [
				"The source code is not accessible.  We must decompile..."
				outStream nextChunkPut: compiledMethod decompileString]
			ifFalse: [
				sourceFile := SourceFiles at: compiledMethod fileIndex.
				sourceFile position: oldPos.
				"Copy the method chunk"
				fileIndex = 0 ifFalse: [
					outStream padTo: SourceFiles pointerScaleForWriting put: $  ].
				newPos := outStream position.
				"Copy method chunk"
				methodCode := sourceFile nextChunk.
				(methodCode isEmpty or: [ methodCode isSeparators ]) ifTrue: [
					"This is very unlikely, but possible source file is corrupted."
					methodCode := compiledMethod decompileString ].
				"Convert method code to use ANSI assignments"
				(Preferences at: #fileOutANSIassignment) ifTrue: [
					codeStyler := SHTextStylerST80 new.
					codeStyler classOrMetaClass: compiledMethod methodClass.
					methodCodeText := methodCode asText.
					codeStyler instVarNamed: 'formattedText' put: methodCodeText.
					codeStyler getReady.
					codeStyler parseText.
					codeStyler replaceStringForRangesWithType: #assignment with: ':=' offset: 0.
					methodCode := methodCodeText string ].
				outStream nextChunkPut: methodCode.
				moveSource ifTrue: [    "Set the new method source pointer"
					endPos := outStream position.
					compiledMethod checkOKToAdd: endPos - newPos at: newPos in: compiledMethod fileIndex.
					compiledMethod setSourcePosition: newPos inFile: fileIndex]].
	doPreamble ifTrue: [ outStream nextChunkPut: ' ' ].
	outStream newLine.
]

{ #category : 'printing' }
ClassDescription >> printNameOn: aStream [
	aStream nextPutAll: 'a subclass of '.
	superclass notNil
		ifTrue: [ superclass printNameOn: aStream ]
		ifFalse: [ aStream nextPutAll: 'nil' ]
]

{ #category : 'printing' }
ClassDescription >> printOn: aStream  [
	aStream nextPutAll: self name
]

{ #category : 'printing' }
ClassDescription >> printWithClosureAnalysisOn: aStream  [

	aStream nextPutAll: self name
]

{ #category : 'fileIn/Out' }
ClassDescription >> putClassCommentToCondensedChangesFile: aFileStream [
	"Called when condensing changes.  If the receiver has a class comment, and if that class comment does not reside in the .sources file, then write it to the given filestream, with the resulting RemoteString being reachable from the source file #2.  Note that any existing backpointer into the .sources file is lost by this process -- a situation that maybe should be fixed someday."

	| header aCommentRemoteStr aStamp |
	self isMeta ifTrue: [^ self].  "bulletproofing only"
	((aCommentRemoteStr := self organization commentRemoteStr) isNil or:
		[aCommentRemoteStr sourceFileNumber = 1]) ifTrue: [^ self].

	aFileStream newLine; nextPut: $!.
	header := String streamContents: [:strm | 
		strm 
			nextPutAll: self name;
			nextPutAll: ' commentStamp: '.
		(aStamp := self organization commentStamp ifNil: ['<historical>']) storeOn: strm.
		strm nextPutAll: ' prior: 0'].
	aFileStream nextChunkPut: header.
	aFileStream newLine.
	self organization classComment: (RemoteString newString: self organization classComment onFileNumber: 2 toFile: aFileStream) stamp: aStamp
]

{ #category : 'accessing method dictionary' }
ClassDescription >> recoverFromMDFault [
	"This method handles methodDict faults to support, eg, discoverActiveClasses (qv)."
	(organization isMemberOf: Array) ifFalse: [^ self error: 'oops'].
	methodDict := organization first.
	organization := organization second.

]

{ #category : 'accessing method dictionary' }
ClassDescription >> recoverFromMDFaultWithTrace [
	"This method handles emthodDict faults to support, eg, discoverActiveClasses (qv)."
	self recoverFromMDFault.
	Smalltalk at: #MDFaultDict ifPresent:
		[:faultDict | faultDict at: self name put:
			(String streamContents: [ :strm |
				(thisContext stackOfSize: Debugger defaultDebugStackSize) do: [ :item |
					strm print: item; newLine]])]

"Execute the following statement to induce MD fault tracing.  This means that, not only will all active classes be recorded but, after a test run, MDFaultDict will contain, for every class used, a stack trace showing how it came to be used.  This statement should be executed just prior to any such text, in order to clear the traces.

	Smalltalk at: #MDFaultDict put: Dictionary new.

"
]

{ #category : 'fileIn/Out' }
ClassDescription >> reformatAll  [
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing"
	self selectorsDo: [:sel | self reformatMethodAt: sel]
]

{ #category : 'fileIn/Out' }
ClassDescription >> reformatMethodAt: selector [
	| newCodeString method | 
	newCodeString := self compilerClass new
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method := self compiledMethodAt: selector.
	method
		putSource: newCodeString
		fromParseNode: nil
		class: self
		category: (self organization categoryOfElement: selector)
		withStamp: Utilities changeStamp
		libraryName: (method propertyValueAt: #libraryName)
		inFile: 2
		priorMethod: method.
]

{ #category : 'accessing method dictionary' }
ClassDescription >> removeCategory: aString  [
	"Remove each of the messages categorized under aString in the method 
	dictionary of the receiver. Then remove the category aString."
	| categoryName |
	categoryName := aString asSymbol.
	(self organization listAtCategoryNamed: categoryName) do:
		[:sel | self removeSelector: sel].
	self organization removeCategory: categoryName
]

{ #category : 'instance variables' }
ClassDescription >> removeInstVarName: aString  [
	"Remove the argument, aString, as one of the receiver's instance 
	variables. Create an error notification if the argument is not found."

	self subclassResponsibility
]

{ #category : 'accessing method dictionary' }
ClassDescription >> removeSelector: selector  [
	| priorMethod priorProtocol | 
	"Remove the message whose selector is given from the method 
	dictionary of the receiver, if it is there. Answer nil otherwise."

	priorMethod := self compiledMethodAt: selector ifAbsent: [^ nil].
	(self theNonMetaClass isOkToRemoveMethod: selector isMeta: self isMeta)
		ifFalse: [self error: 'Method removal not allowed'].
	priorProtocol := self whichCategoryIncludesSelector: selector.
	SystemChangeNotifier uniqueInstance doSilently: [
		self organization removeElement: selector].
	super removeSelector: selector.
	SystemChangeNotifier uniqueInstance 
			methodRemoved: priorMethod selector: selector inProtocol: priorProtocol class: self.
]

{ #category : 'accessing method dictionary' }
ClassDescription >> removeSelectorIfInBaseSystem: selector [
	"Remove the message whose selector is given from the method 
	dictionary of the receiver, if it is there and not part of a package. Answer nil otherwise."
	| priorProtocol | 

	self compiledMethodAt: selector ifAbsent: [^ nil].
	priorProtocol := self whichCategoryIncludesSelector: selector.
	CodePackage
		packageOfMethodCategory: priorProtocol
		ofClass: self
		ifNone: [
			"If remove is actually done, then include it in the current change set for the base system,
			as a regular remove."
			self removeSelector: selector ]
]

{ #category : 'accessing class hierarchy' }
ClassDescription >> removeUninstantiatedSubclassesSilently [
	"Remove the classes of any subclasses that have neither instances nor subclasses.  Answer the number of bytes reclaimed"
	"Player removeUninstantiatedSubclassesSilently"

	| candidatesForRemoval  oldFree |

	oldFree := Smalltalk garbageCollect.
	candidatesForRemoval :=
		self subclasses select: [:c |
			(c instanceCount = 0) and: [c subclasses isEmpty]].
	candidatesForRemoval do: [:c | c removeFromSystem].
	^ Smalltalk garbageCollect - oldFree
]

{ #category : 'fileIn/Out' }
ClassDescription >> reorganize [
	"During fileIn, !Rectangle reorganize! allows Rectangle to seize control and treat the next chunk as its organization.  See the transfer of control where ReadWriteStream fileIn calls scanFrom:"

	^self organization
]

{ #category : 'accessing method dictionary' }
ClassDescription >> selectorsInCategory: aSymbol  [
	"Answer a list of the method selectors of the receiver that are in category named aName"
	
	| aColl |
	aColl := Set withAll: ((aSymbol isNil or: [aSymbol == ClassOrganizer allCategory])
			ifTrue: [self organization allMethodSelectors]
			ifFalse: [self organization listAtCategoryNamed: aSymbol]).
	^ aColl asArray sort
]

{ #category : 'initialization' }
ClassDescription >> setInstVarNames: instVarArray [
	"Private - for class initialization only"
	| required |
	required := self instSize.
	superclass ifNotNil: [
		required := required - superclass instSize].
	instVarArray size = required
		ifFalse:[
			^self error: required printString, ' instvar names are required'].
	instanceVariables := instVarArray size = 0
		ifFalse: [
			instVarArray == Array
				ifTrue: [ instVarArray ]
				ifFalse: [ Array withAll: instVarArray ]].
]

{ #category : 'printing' }
ClassDescription >> sharedPoolsString [
	"Answer a string of my shared pool names separated by spaces."

	^String streamContents: [ :stream |
		self sharedPools 
			do: [ :each |
				stream nextPutAll: (Smalltalk
					keyAtIdentityValue: each 
					ifAbsent: [ 'private' ]) ]
			separatedBy: [ stream space ] ]
]

{ #category : 'accessing' }
ClassDescription >> smalltalkComment [
	"Answer the receiver's comment. (If missing, supply a template) "
	| aString |
	aString := self theNonMetaClass organization classComment.
	aString isEmpty ifFalse: [^ aString].
	^
'Main comment stating the purpose of this class and relevant relationship to other classes.

Possible useful expressions for doIt or printIt.

Structure:
 instVar1		type -- comment about the purpose of instVar1
 instVar2		type -- comment about the purpose of instVar2

Any further useful comments about the general approach of this implementation.'
]

{ #category : 'printing' }
ClassDescription >> storeOn: aStream [
	"Classes and Metaclasses have global names."

	aStream nextPutAll: self name
]

{ #category : 'accessing class hierarchy' }
ClassDescription >> subclasses [
	^ Array new
]

{ #category : 'initialization' }
ClassDescription >> superclass: aClass methodDictionary: mDict format: fmt [
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	instanceVariables := nil.
	self organization: nil.
]

{ #category : 'accessing' }
ClassDescription >> theMetaClass [
	"Sent to a class or metaclass, always return the metaclass"

	^self class
]

{ #category : 'accessing' }
ClassDescription >> theNonMetaClass [
	"Sent to a class or metaclass, always return the class"

	^self
]

{ #category : 'initialize-release' }
ClassDescription >> updateInstances: oldInstances from: oldClass isMeta: isMeta [
	"Recreate any existing instances of the argument, oldClass, as instances of the receiver,
	 which is a newly changed class. Permute variables as necessary, and forward old instances
	 to new instances.  Answer nil to defeat old clients that expect an array of old instances.
	 The old behaviour, which necessitated a global GC, exchanged identities and answered
	 the old instances.  But no clients used the result.  This way we avoid the unnecessary GC,"
	| map variable instSize newInstances |

	oldInstances isEmpty ifTrue:
		[^nil]. "no instances to convert"
	isMeta ifTrue:
		[(oldInstances size = 1
		  and: [self soleInstance class == self
				or: [self soleInstance class == oldClass]]) ifFalse:
			[^self error: 'Metaclasses can only have one instance']].
	map := self instVarMappingFrom: oldClass.
	variable := self isVariable.
	instSize := self instSize.
	newInstances := Array new: oldInstances size.
	1 to: oldInstances size do:
		[:i|
		newInstances
			at: i
			put: (self newInstanceFrom: (oldInstances at: i) variable: variable size: instSize map: map)].
	"Now perform a bulk mutation of old instances into new ones"
	oldInstances elementsForwardIdentityTo: newInstances.
	^nil
]

{ #category : 'initialize-release' }
ClassDescription >> updateMethodBindingsTo: aBinding [
	"ClassBuilder support for maintaining valid method bindings."
	methodDict do: [:method| method methodClassAssociation: aBinding]
]

{ #category : 'compiling' }
ClassDescription >> wantsChangeSetLogging [
	"Answer whether code submitted for the receiver should be remembered by the changeSet mechanism.  7/12/96 sw"


	^ true
]

{ #category : 'compiling' }
ClassDescription >> wantsRecompilationProgressReported [
	"Answer whether the receiver would like progress of its recompilation reported interactively to the user."

	^ true
]

{ #category : 'organization' }
ClassDescription >> whichCategoryIncludesSelector: aSelector  [
	"Answer the category of the argument, aSelector, in the organization of 
	the receiver, or answer nil if the receiver does not inlcude this selector."

	(self includesSelector: aSelector)
		ifTrue: [^ self organization categoryOfElement: aSelector]
		ifFalse: [^nil]
]

{ #category : 'instance variables' }
ClassDescription >> withClassesThatDefineInHierarchyInstanceVariable: aName do: foundBlock ifNone: noneBlock [

	^(self classThatDefinesInstanceVariable: aName)
		ifNil: [ self withSubclassesThatDefineInstanceVariable: aName do: foundBlock ifNone: noneBlock ]
		ifNotNil: [ :definingClass | foundBlock value: (Array with: definingClass) ]
]

{ #category : 'instance variables' }
ClassDescription >> withSubclassesThatDefineInstanceVariable: aName do: foundBlock ifNone: noneBlock [

	| definingSubclasses |

	definingSubclasses := self allSubclasses select: [ :aSubclass |
		aSubclass definesInstanceVariableNamed: aName ].

	^definingSubclasses isEmpty
		ifTrue: [ noneBlock value ]
		ifFalse: [ foundBlock value: definingSubclasses ]
]

{ #category : 'organization' }
ClassDescription >> zapOrganization [
	"Remove the organization of this class by message categories.
	This is typically done to save space in small systems.  Classes and methods
	created or filed in subsequently will, nonetheless, be organized"

	self organization: nil.
	self isMeta ifFalse: [self class zapOrganization]
]
