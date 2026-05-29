'From Cuis7.7 [latest update: #7969] on 29 May 2026 at 12:49:32 pm'!

!Object methodsFor: 'events-registering' stamp: 'rww 30/Dec/2002 10:37:00'!
when: anEventNameSymbol
send: aMessageSelector
to: anObserver
with: messageArgument
	"Make the given object one of the receiver's observers (dependents).
	Receivers of this message, i.e. objects being observed, are usually called Models.
	Observing can also be set up sending one of the #when:send:to: or #addDependent: messages.
	Models will send themselves #changed:, #changed or one of the #triggerEvent* messages to notify possible observers.
	If appropriate, it is best to make Models inherit from ActiveModel, for performance reasons."

	self
		when: anEventNameSymbol
		send: aMessageSelector
		to: anObserver
		withArguments: {messageArgument}! !

!Object methodsFor: 'message handling' library: #MinimalKernel01 stamp: 'di 26/Mar/1999 07:52:00'!
perform: aSymbol with: anObject 
	"Send the selector, aSymbol, to the receiver with anObject as its argument.
	Fail if the number of arguments expected by the selector is not one.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: {anObject}! !

!Object methodsFor: 'message handling' stamp: 'di 26/Mar/1999 07:52:00'!
perform: aSymbol with: firstObject with: secondObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not two.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol withArguments: {firstObject. secondObject}! !

!Object methodsFor: 'message handling' stamp: 'di 26/Mar/1999 07:51:00'!
perform: aSymbol with: firstObject with: secondObject with: thirdObject 
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not three.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol
		withArguments: {firstObject. secondObject. thirdObject}! !

!Object methodsFor: 'message handling' stamp: 'jmv 29/Oct/2024 14:54:47'!
perform: aSymbol with: firstObject with: secondObject with: thirdObject with: fourth
	"Send the selector, aSymbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not three.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 83>
	^ self perform: aSymbol
		withArguments: {firstObject. secondObject. thirdObject. fourth}! !


!Parser methodsFor: 'primitives' stamp: 'eem 7/Dec/2025 15:26:35'!
externalFunctionDeclaration
	"Parse the function declaration for a call to an external library.
	(1) Create an instance of ExternalLibraryFunction and install it as first literal.
	(2) Add a pragma to primitive call 120."
	| descriptorClass callType modifier fn |
	descriptorClass := Smalltalk at: #ExternalFunction ifAbsent: [ ^ false ].
	callType := descriptorClass callingConventionFor: here.
	callType == nil ifTrue:[^false].
	[modifier := descriptorClass callingConventionModifierFor: token.
	 modifier notNil] whileTrue:
		[self advance.
		 callType := callType bitOr: modifier].
	"Parse return type"
	self advance.
	fn := self
			parseAnExternalFunction: descriptorClass
			callType: callType
			checkReturnType: nil
			error: [:reasonString| ^self expected: reasonString].
	fn ifNotNil:
		[self allocateLiteral: fn].
	(self matchToken: 'error:')
		ifTrue:
			[| errorCodeVariable |
			 errorCodeVariable := here.
			(hereType == #string
			 or: [hereType == #word]) ifFalse:[^self expected: 'error code (a variable or string)'].
			 self advance.
			 self addPragma: (Pragma keyword: #primitive:error: arguments: {120. errorCodeVariable}).
			 fn ifNotNil: [fn setErrorCodeName: errorCodeVariable]]
		ifFalse:
			[self addPragma: (Pragma keyword: #primitive: arguments: #(120))].
	^true! !


!Encoder methodsFor: 'initialization' stamp: 'jmv 5/Feb/2024 18:16:43'!
init: aClass context: aContext notifying: req
	requestor := req.
	class := aClass.
	nTemps := 0.
	supered := false.
	self initScopeAndLiteralTables.
	"No Tweak Fields in Cuis"
	class variablesAndOffsetsDo: [ :variable "<String>" :offset "<Integer>" |
		scopeTable
			at: variable
			put: (offset >= 0
					ifTrue: [InstanceVariableNode new
								name: variable index: offset]
					ifFalse: [MaybeContextInstanceVariableNode new
								name: variable index: offset negated])].
	aContext ifNotNil: [
		| homeNode |
		homeNode := self bindTemp: Encoder doItInContextName.
		"0th temp = aContext passed as arg"
		aContext tempNames withIndexDo: [ :variable :index |
			variable ~= Encoder doItInContextName ifTrue: [
				scopeTable
					at: variable
					put: (MessageAsTempNode new
						receiver: homeNode
						selector: #namedTempAt:
						arguments: {self encodeLiteral: index}
						precedence: 3
						from: self)]]].
	sourceRanges := Dictionary new.
	globalSourceRanges := OrderedCollection new.! !


!MessageNode methodsFor: 'printing' stamp: 'di 1/May/2000 23:20:00'!
printIfNil: aStream indent: level

	self printReceiver: receiver on: aStream indent: level.

	^self printKeywords: selector key
		arguments: {arguments first}
		on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'cmm 1/Mar/2010 14:39:00'!
printIfOn: aStream indent: level 
	receiver ifNotNil: 
		[ receiver
			printOn: aStream
			indent: level
			precedence: precedence ].
	(arguments last isJust: NodeNil) ifTrue: [ ^ self
			printKeywords: #ifTrue:
			arguments: {arguments first}
			on: aStream
			indent: level ].
	(arguments last isJust: NodeFalse) ifTrue: [ ^ self
			printKeywords: #and:
			arguments: {arguments first}
			on: aStream
			indent: level ].
	(arguments first isJust: NodeNil) ifTrue: [ ^ self
			printKeywords: #ifFalse:
			arguments: {arguments last}
			on: aStream
			indent: level ].
	(arguments first isJust: NodeTrue) ifTrue: [ ^ self
			printKeywords: #or:
			arguments: {arguments last}
			on: aStream
			indent: level ].
	self
		printKeywords: #ifTrue:ifFalse:
		arguments: arguments
		on: aStream
		indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'jmv 1/Mar/2010 10:20:00'!
printToDoOn: aStream indent: level

	| limitNode |
	self printReceiver: receiver on: aStream indent: level.

	(arguments last == nil or: [(arguments last isMemberOf: AssignmentNode) not])
		ifTrue: [limitNode := arguments first]
		ifFalse: [limitNode := arguments last value].
	(selector key = #to:by:do:
	 and: [(arguments at: 2) isConstantNumber
	 and: [(arguments at: 2) key = 1]])
		ifTrue: [self printKeywords: #to:do:
					arguments: {limitNode. arguments at: 3}
					on: aStream indent: level]
		ifFalse: [self printKeywords: selector key
					arguments: {limitNode} , arguments allButFirst
					on: aStream indent: level]! !

!MessageNode methodsFor: 'printing' stamp: 'eem 2/Jun/2008 11:47:00'!
printWithClosureAnalysisIfNil: aStream indent: level

	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.

	^self printWithClosureAnalysisKeywords: selector key
		arguments: {arguments first}
		on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'eem 2/Jun/2008 11:47:00'!
printWithClosureAnalysisIfOn: aStream indent: level

	receiver ifNotNil:
		[receiver printWithClosureAnalysisOn: aStream indent: level + 1 precedence: precedence].
	(arguments last isJust: NodeNil) ifTrue:
		[^self printWithClosureAnalysisKeywords: #ifTrue: arguments: {arguments first}
					on: aStream indent: level].
	(arguments last isJust: NodeFalse) ifTrue:
		[^self printWithClosureAnalysisKeywords: #and: arguments: {arguments first}
					on: aStream indent: level].
	(arguments first isJust: NodeNil) ifTrue:
		[^self printWithClosureAnalysisKeywords: #ifFalse: arguments: {arguments last}
					on: aStream indent: level].
	(arguments first isJust: NodeTrue) ifTrue:
		[^self printWithClosureAnalysisKeywords: #or: arguments: {arguments last}
					on: aStream indent: level].
	self printWithClosureAnalysisKeywords: #ifTrue:ifFalse: arguments: arguments
					on: aStream indent: level! !

!MessageNode methodsFor: 'printing' stamp: 'jmv 1/Mar/2010 10:20:00'!
printWithClosureAnalysisToDoOn: aStream indent: level

	| limitNode |
	self printWithClosureAnalysisReceiver: receiver on: aStream indent: level.

	limitNode := (arguments last == nil
				or: [arguments last isAssignmentNode not])
					ifTrue: [arguments first]
					ifFalse: [arguments last value].
	(selector key = #to:by:do:
	 and: [(arguments at: 2) isConstantNumber
	 and: [(arguments at: 2) key = 1]])
		ifTrue: [self printWithClosureAnalysisKeywords: #to:do:
					arguments: {limitNode. arguments at: 3}
					on: aStream indent: level]
		ifFalse: [self printWithClosureAnalysisKeywords: selector key
					arguments: {limitNode} , arguments allButFirst
					on: aStream indent: level]! !

!MessageNode methodsFor: 'macro transformations' stamp: 'nice 12/Jan/2011 22:07:00'!
toDoFromWhileWithInit: initStmt
	"Return nil, or a to:do: expression equivalent to this whileTrue:"
	| variable increment limit toDoBlock body test |
	(selector key == #whileTrue:
	 and: [initStmt isAssignmentNode
	 and: [initStmt variable isTemp]]) ifFalse:
		[^nil].
	body := arguments last statements.
	variable := initStmt variable.
	increment := body last toDoIncrement: variable.
	(increment == nil
	 or: [receiver statements size ~= 1]) ifTrue:
		[^nil].
	test := receiver statements first.
	"Note: test chould really be checked that <= or >= comparison
	jibes with the sign of the (constant) increment"
	(test isMessageNode
	 and: [(limit := test toDoLimit: variable) notNil]) ifFalse:
		[^nil].
	"The block must not overwrite the limit"
	(limit isVariableNode and: [body anySatisfy: [:e | e isAssignmentNode and: [e variable = limit]]])
		ifTrue: [^nil]. 
	toDoBlock := BlockNode statements: body allButLast returns: false.
	toDoBlock arguments: {variable}.
	variable scope: -1.
	variable beBlockArg.
	^MessageNode new
		receiver: initStmt value
		selector: (SelectorNode new key: #to:by:do: code: #macro)
		arguments: {limit. increment. toDoBlock}
		precedence: precedence! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 3/Sep/2025 10:13:15'!
transformIfNilIfNotNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."

	| ifNotNilArg |
	(Preferences at: #avoidIfNilOptimization) ifTrue: [
		^false ].
	
	ifNotNilArg := arguments at: 2.
	((self checkBlock: (arguments at: 1) as: 'Nil arg' from: encoder maxArgs: 0)
	  and: [self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := AssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := MessageNode new
					receiver: receiver
					selector: #==
					arguments: {NodeNil}
					precedence: 2
					from: encoder.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true! !

!MessageNode methodsFor: 'macro transformations' stamp: 'jmv 3/Sep/2025 10:13:21'!
transformIfNotNilIfNil: encoder
	"vb: Changed to support one-argument ifNotNil: branch. In the 1-arg case we
	 transform the receiver to
		(var := receiver)
	 which is further transformed to
		(var := receiver) == nil ifTrue: .... ifFalse: ...
	 This does not allow the block variable to shadow an existing temp, but it's no different
	 from how to:do: is done."
	
	| ifNotNilArg |
	(Preferences at: #avoidIfNilOptimization) ifTrue: [
		^false ].
	
	ifNotNilArg := arguments at: 1.
	((self checkBlock: ifNotNilArg as: 'NotNil arg' from: encoder maxArgs: 1)
	  and: [self checkBlock: (arguments at: 2) as: 'Nil arg' from: encoder maxArgs: 0]) ifFalse:
		[^false].

	ifNotNilArg numberOfArguments = 1 ifTrue:
		[receiver := AssignmentNode new
						variable: ifNotNilArg firstArgument
						value: receiver].

	selector := SelectorNode new key: #ifTrue:ifFalse: code: #macro.
	receiver := MessageNode new
					receiver: receiver
					selector: #==
					arguments: {NodeNil}
					precedence: 2
					from: encoder.
	arguments swap: 1 with: 2.
	arguments do: [:arg| arg noteOptimizedIn: self].
	^true! !


!DecompilerConstructor methodsFor: 'constructor' stamp: 'Squeak1.2 29/Jun/1997 10:40:55'!
codeConstants
	"Answer with an array of the objects representing self, true, false, nil,
	-1, 0, 1, 2."

	^{NodeSelf. NodeTrue. NodeFalse. NodeNil}
		, ((-1 to: 2) collect: [:i | LiteralNode new key: i code: LdMinus1 + i + 1])! !


!Form methodsFor: 'analyzing' stamp: 'jmv 2/Apr/2016 23:06:00'!
cgForPixelValue: pv orNot: not
	"Return the center of gravity for all pixels of value pv.
	Note:  If orNot is true, then produce the center of gravity for all pixels
	that are DIFFERENT from the supplied (background) value"
	| pixCount weighted xAndY |
	xAndY := {(self xTallyPixelValue: pv orNot: not).
					(self yTallyPixelValue: pv orNot: not)} collect: [ :profile |	"For both x and y profiles..."
		pixCount := 0.  weighted := 0.
		profile withIndexDo: [ :t :i |
			pixCount := pixCount + t.
			weighted := weighted + (t*i)].
		pixCount = 0  "Produce average of nPixels weighted by coordinate"
			ifTrue: [0.0]
			ifFalse: [weighted asFloat / pixCount asFloat - 1.0]].

	^ xAndY first @ xAndY last
"
| r f cg |
[Sensor isAnyButtonPressed] whileFalse: [
	r := Sensor mousePoint extent: 50@50.
	f := Form extent: r extent depth: 16.
	f fromDisplay: r.
	cg := f cgForPixelValue: (Color black pixelValueForDepth: f depth) orNot: false.
	f displayAt: 0@0.
	Display fill: (cg extent: 4) fillColor: Color red ]
"! !


!Form class methodsFor: 'examples' stamp: 'len 9/May/2022 17:58:33'!
xorHack: size  "Display restoreAfter: [Form xorHack: 256]"
	"Draw a smiley face or stick figure, and end with option-click.
	Thereafter image gets 'processed' as long as you have button down.
	If you stop at just the right time, you'll see you figure upside down,
	and at the end of a full cycle, you'll see it perfectly restored.
	Dude -- this works in color too!!"
	| rect form i bb |
	rect := 5@5 extent: size@size.
	Display fillWhite: rect; border: (rect expandBy: 2) width: 2.
	Display border: (rect topRight - (0@2) extent: rect extent*2 + 4) width: 2.
	Form exampleSketch.
	form := Form fromDisplay: rect.
	bb := form boundingBox.
	i := 0.
	[Sensor isMouseButton2Pressed] whileFalse:
		[[Sensor isMouseButton1Pressed] whileTrue:
			[i := i + 1.
			{0@1. 0@ -1. 1@0. -1@0} do:
				[:d | form copyBits: bb from: form at: d
					clippingBox: bb rule: Form reverse ].
			form displayAt: rect topLeft.
			i+2\\size < 4 ifTrue: [(Delay forMilliseconds: 300) wait]].
		(form magnify: form boundingBox by: 2@2) displayAt: rect topRight + (2@0).
		Sensor waitButton].! !


!TrueTypeFont methodsFor: 'objects from disk' stamp: 'jmv 17/May/2019 09:52:48'!
objectForDataStream: refStrm

	"I am about to be written on an object file.  Write a textual reference instead.
	Warning: This saves a lot of space, but might fail if using other fonts than those in AvailableFonts"

	^ DiskProxy
		global: #FontFamily
		selector: #familyName:pointSize:
		args: {self familyName. self pointSize}! !


!ClassDescription methodsFor: 'instance variables' stamp: 'HAW 4/Mar/2019 15:26:00'!
withClassesThatDefineInHierarchyInstanceVariable: aName do: foundBlock ifNone: noneBlock

	^(self classThatDefinesInstanceVariable: aName)
		ifNil: [ self withSubclassesThatDefineInstanceVariable: aName do: foundBlock ifNone: noneBlock ]
		ifNotNil: [ :definingClass | foundBlock value: {definingClass} ]! !


!Class methodsFor: 'class variables' stamp: 'HAW 28/Jun/2020 16:34:20'!
removeClassVarName: aString
	"Remove the class variable whose name is the argument, aString, from
	the names defined in the receiver, a class. Create an error notification if
	aString is not a class variable or if it is still being used in the code of
	the class."

	| aSymbol |
	aSymbol := aString asSymbol.
	(classPool isNil or: [(classPool includesKey: aSymbol) not])
		ifTrue: [ ^self error: aString, ' is not a class variable'].
	self withAllSubclasses do:[:subclass |
		{subclass. subclass class} do: [ :classOrMeta |
			(classOrMeta whichSelectorsReferTo: (classPool associationAt: aSymbol))
				isEmpty ifFalse: [
					InMidstOfFileinNotification signal ifTrue: [
						Transcript newLine; show: self name, ' (' , aString , ' is Undeclared) '.
						^Undeclared declare: aSymbol from: classPool ].
					(self confirm: (aString,' is still used in code of class ', classOrMeta name,
						'.\Is it okay to move it to Undeclared?') withNewLines)
						ifTrue: [ ^Undeclared declare: aSymbol from: classPool ]
						ifFalse: [ ^self ]]]].
	classPool removeKey: aSymbol.
	classPool isEmpty ifTrue: [ classPool := nil ]! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 26/Sep/2024 10:55:35'!
addCategory: catString before: nextCategory
	"Add a new category named heading.
	If default category exists and is empty, remove it.
	If nextCategory is nil, then add the new one at the end,
	otherwise, insert it before nextCategory."
	
	| index newCategory default |
	
	newCategory := catString withBlanksTrimmed asSymbol.
	
	"heading already exists, so done"
	(categoryArray indexOf: newCategory) > 0 ifTrue: [^self].	
	
	index := categoryArray indexOf: nextCategory ifAbsent: [categoryArray size + 1].
	categoryArray := categoryArray
		copyReplaceFrom: index
		to: index-1
		with: {newCategory}.
	categoryStops := categoryStops
		copyReplaceFrom: index
		to: index-1
		with: {index = 1
				ifTrue: [0]
				ifFalse: [categoryStops at: index-1]}.

	"remove empty default category"
	default := self class defaultCategory.
	(newCategory ~= default and: [(self listAtCategoryNamed: default) isEmpty])
		ifTrue: [self removeCategory: default]! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 14/May/2026 15:23:25'!
classify: element under: heading suppressIfDefault: aBoolean
	"Store the argument, element, in the category named heading.   If aBoolean is true, then invoke special logic such that the classification is NOT done if the new heading is the Default and the element already had a non-Default classification -- useful for filein"

	| catName catIndex elemIndex realHeading default |

	default := self class defaultCategory.

	realHeading := heading isNil
		ifTrue: [default]
		ifFalse: [heading withBlanksTrimmed ].

	"done if already under that category"
	(catName := self categoryOfElement: element) = realHeading ifTrue: [^ self].

	catName ifNotNil: [
		"return if non-Default category already assigned in memory"
		(aBoolean and: [realHeading = default]) ifTrue: [^ self].
		"remove if in another category"
		self removeElement: element].

	(categoryArray indexOf: realHeading) = 0 ifTrue: [
		self aboutToAddCategory: realHeading.
		self addCategory: realHeading ].

	catIndex := categoryArray indexOf: realHeading.
	elemIndex := catIndex > 1
		ifTrue: [categoryStops at: catIndex - 1]
		ifFalse: [0].
	[(elemIndex := elemIndex + 1) <= (categoryStops at: catIndex)
		and: [element >= (elementArray at: elemIndex)]] whileTrue.

	"elemIndex is now the index for inserting the element. Do the insertion before it."
	elementArray := elementArray copyReplaceFrom: elemIndex to: elemIndex-1 with: {element}.

	"add one to stops for this and later categories"
	catIndex to: categoryArray size do: [ :i |
		categoryStops at: i put: (categoryStops at: i) + 1].

	(self listAtCategoryNamed: default) isEmpty ifTrue: [self removeCategory: default].! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 13/Sep/2007 21:01:00'!
moveCategoryBottom: catString
	| category newCategories |
	category := catString asSymbol.
	newCategories := (categoryArray copyWithout: category), {category}.
	self categories: newCategories.
	^ newCategories size! !

!Categorizer methodsFor: 'accessing' stamp: 'jmv 13/Sep/2007 20:59:00'!
moveCategoryTop: catString
	| category newCategories |
	category := catString asSymbol.
	newCategories := {category}, (categoryArray copyWithout: category).
	self categories: newCategories! !


!RunArray class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
new: size withAll: value 
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	size = 0 ifTrue: [^self new].
	^self runs: {size} values: {value}! !


!Text class methodsFor: 'instance creation' stamp: 'Squeak1.2 29/Jun/1997 10:40:55'!
string: aString attribute: att
	"Answer an instance of me whose characters are aString.
	att is a TextAttribute."

	^self string: aString attributes: {att}! !


!SystemDictionary methodsFor: 'class names' stamp: 'HAW 4/Dec/2019 10:54:28'!
prepareToRenameClass: aClass from: oldName to: newName

	"Rename the class, aClass, to have the title newName."

	| oldref i category |

	category := SystemOrganization categoryOfElement: oldName.
	SystemOrganization classify: newName under: category.
	SystemOrganization removeElement: oldName.
	oldref := self associationAt: oldName.
	self removeKey: oldName.
	oldref key: newName.
	self add: oldref.  "Old association preserves old refs"
	{StartUpList. ShutDownList} do:
		[:list |  i := list indexOf: aClass name ifAbsent: [0].
		i > 0 ifTrue: [list at: i put: newName]].
	self flushClassNameCache.
	
	SystemChangeNotifier uniqueInstance aboutToRenameClass: aClass from: oldName to: newName inCategory: category.
	! !


!UniFileStream class methodsFor: 'error handling' stamp: 'jmv 13/Aug/2025 16:40:08'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir := fullFileName asFullFileEntry parent.
	files := dir fileNames.
	fileName := fullFileName asFullFileEntry name.
	choices := fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection := (PopUpMenu labelArray: choices lines: {5} )
		startUpWithCaption: fullFileName asFullFileEntry name, '
does not exist.'.
	selection = 0 ifTrue: ["esc, click outside" ^ nil ].
	selection = choices size ifTrue: ["cancel" ^ nil ].
	selection < (choices size - 1) ifTrue: [
		newName := (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName := self request: 'Enter a new file name' initialAnswer: fileName.
		"If Cancel was pressed, no file should be opened - Hernan"
		newName isEmpty ifTrue: [ ^nil ]].
	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !


!DataStream class methodsFor: 'examples' stamp: 'jmv 10/Mar/2018 21:53:12'!
example
    "An example and test of DataStream/ReferenceStream.
     11/19/92 jhm: Use self testWith:."
    "DataStream example"
    "ReferenceStream example"
    | input sharedPoint |

    "Construct the test data."
    input := Array new: 9.
    input at: 1 put: nil.
    input at: 2 put: true.
    input at: 3 put: (Form extent: 63 @ 50 depth: 8).
		(input at: 3) fillWithColor: `Color lightBlue`.
    input at: 4 put: #(3 3.0 'three').
    input at: 5 put: false.
    input at: 6 put: 1024 @ -2048.
    input at: 7 put: #x.
    input at: 8 put: {sharedPoint := 0 @ -30000}.
    input at: 9 put: sharedPoint.

    "Write it out, read it back, and return it for inspection."
    ^ self testWith: input! !


!ReferenceStream class methodsFor: 'as yet unclassified' stamp: 'jmv 10/Mar/2018 21:54:43'!
example2
"Here is the way to use DataStream and ReferenceStream:
	rr := ReferenceStream fileNamed: ''test.obj''.
	rr nextPut: <your object>.
	rr close.

To get it back:
	rr := ReferenceStream fileNamed: ''test.obj''.
	<your object> := rr next.
	rr close.
"
"An example and test of DataStream/ReferenceStream.
	 11/19/92 jhm: Use self testWith:."
	"ReferenceStream example2"
	| input sharedPoint |

	"Construct the test data."
	input := Array new: 9.
	input at: 1 put: nil.
	input at: 2 put: true.
	input at: 3 put: false.
	input at: 4 put: #(-4 -4.0 'four' four).
	input at: 5 put: (Form extent: 63 @ 50 depth: 8).
		(input at: 5) fillWithColor: `Color lightOrange`.
	input at: 6 put: 1024 @ -2048.
	input at: 7 put: input. "a cycle"
	input at: 8 put: {sharedPoint := 0 @ -30000}.
	input at: 9 put: sharedPoint.

	"Write it out, read it back, and return it for inspection."
	^ self testWith: input
! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 17/Feb/2010 23:04:00'!
instVarInfo: anObject
	"Return the object to write on the outgoing file that contains the structure of each class we are about to write out.  Must be an Array whose first element is 'class structure'.  Its second element is a Dictionary of pairs of the form #Rectangle -> #(<classVersion> 'origin' 'corner').  "

	"Make a pass through the objects, not writing, but recording the classes.  Construct a database of their inst vars and any version info (classVersion)."

	| dummy refs cls newSupers |
	structures := Dictionary new.
	superclasses := Dictionary new.
	dummy := ReferenceStream on: (DummyStream on: nil).
		"Write to a fake Stream, not a file"
	"Collect all objects"
	dummy rootObject: anObject.	"inform him about the root"
	dummy nextPut: anObject.
	refs := dummy references.
	objCount := refs size.		"for progress bar"
		"Note that Dictionary must not change its implementation!!  If it does, how do we read this reading information?"
	refs keysDo: [:each | 
		cls := each class.
		"cls isObsolete ifTrue: [self error: 'Trying to write ', cls name]."
		(cls class ~~ Metaclass) & (cls isObsolete not) ifTrue: [
			structures at: cls name put: false]].
	"Save work by only computing inst vars once for each class"
	newSupers := Set new.
	structures at: #Point put: false.	"writeRectangle: does not put out class pointer"
	structures at: #Rectangle put: false.
	structures at: #LargePositiveInteger put: false.	"used in slow case of WordArray"
	structures keysDo: [:nm | 
		cls := (nm endsWith: ' class') 
			ifFalse: [Smalltalk at: nm]
			ifTrue: [(Smalltalk at: nm substrings first asSymbol) class].
		cls allSuperclasses do: [:aSuper |
			structures at: aSuper name ifAbsent: [newSupers add: aSuper name]]].
			"Don't modify structures during iteration"
	newSupers do: [:nm | structures at: nm put: 3].	"Get all superclasses into list"
	structures keysDo: [:nm | "Nothing added to classes during loop"
		cls := (nm endsWith: ' class') 
			ifFalse: [Smalltalk at: nm]
			ifTrue: [(Smalltalk at: nm substrings first asSymbol) class].
		structures at: nm put: 
			{cls classVersion}, cls allInstVarNames.
		superclasses at: nm ifAbsent: [
				superclasses at: nm put: cls superclass name]].
	^ {'class structure'. structures. 'superclasses'. superclasses}! !

!SmartRefStream methodsFor: 'read write' stamp: 'jmv 13/Mar/2012 23:14:00'!
verifyStructure
	"Compare the incoming inst var name lists with the existing classes.  Prepare tables that will help to restructure those who need it (renamed, reshaped, steady).    If all superclasses are recorded in the file, only compare inst vars of this class, not of superclasses.  They will get their turn.  "


	| newClass newList oldList converting |

	self flag: #bobconv.	

	converting := OrderedCollection new.
	structures keysDo: [:nm "an old className (symbol)" |
		"For missing classes, there needs to be a method in SmartRefStream like 
			#rectangleoc2 that returns the new class."
		newClass := self mapClass: nm.	   "does (renamed at: nm put: newClass name)"
		newClass class == String ifTrue: [^ newClass].  "error, fileIn needed"
		newList := {newClass classVersion}, newClass allInstVarNames.
		oldList := structures at: nm.
		newList = oldList 
			ifTrue: [steady add: newClass]  "read it in as written"
			ifFalse: [converting add: newClass name]
	].
	false & converting isEmpty not ifTrue: ["debug" 
			self inform: 'These classes are being converted from existing methods:\' withNewLines,
				converting asArray printString].
! !

!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 8/Mar/2024 14:59:00'!
conversionMethodsFor: classList
	| oldStruct newStruct list |
	"Each of these needs a conversion method.  Hard part is the comment in it.  Return a MethodSet."

	list := OrderedCollection new.
	classList do: [:cls |
		oldStruct := structures at: cls name ifAbsent: [#()].
		newStruct := {cls classVersion}, cls allInstVarNames.
		self writeConversionMethodIn: cls fromInstVars: oldStruct to: newStruct 
				renamedFrom: nil.
		list add: cls name, ' convertToCurrentVersion:refStream:'.
		].

	^ MethodSet new initializeMessageList: list.! !

!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 28/Apr/2022 11:59:14'!
writeClassRenameMethod: sel was: oldName fromInstVars: oldList 
	"The class coming is unknown.  Ask the user for the existing class it maps to.  If got one, write a method, and restart the obj fileIn.  If none, write a dummy method and get the user to complete it later.  "

	| tell choice newName answ code oldVer newList newVer instSel |
	self flag: #bobconv.
	tell := 'Reading an instance of ' , oldName 
				, '.
Which modern class should it translate to?'.
	answ := (PopUpMenu 
				labels: 'Let me type the name now
Let me think about it
Let me find a conversion file on the disk') 
					startUpWithCaption: tell.
	answ = 1 
		ifTrue: [
			tell := 'Name of the modern class {1} should translate to:' format: {oldName}.
			choice := self request: tell initialAnswer: ''.	"class name"
			choice isEmpty 
				ifTrue: [answ := 'conversion method needed']
				ifFalse: [
					newName := choice.
					answ := Smalltalk at: newName asSymbol
								ifAbsent: ['conversion method needed'].
					answ class == String 
						ifFalse: [renamed at: oldName asSymbol put: answ name]]].
	answ = 3 | (answ = 0) 
		ifTrue: [
			byteStream close.
			^'conversion method needed'].
	answ = 2 ifTrue: [answ := 'conversion method needed'].
	answ = 'conversion method needed' 
		ifTrue: [
			byteStream close.
			newName := 'PutNewClassHere'].
	answ class == String 
		ifFalse: 
			[oldVer := self versionSymbol: (structures at: oldName).
			newList := {answ classVersion} , answ allInstVarNames.
			newVer := self versionSymbol: newList.
			instSel := 'convert' , oldVer , ':' , newVer , ':'].
	code := WriteStream on: (String new: 500).
	code
		nextPutAll: sel;
		newLine.
	answ class == String 
		ifFalse: [
			code
				newLine;
				tab;
				nextPutAll: 'reshaped at: #' , oldName , ' put: #' , instSel , '.'.
			code
				newLine;
				tab;
				tab;
				nextPutAll: '"Be sure to define that conversion method in class ' 
							, answ name , '"'].
	code
		newLine;
		tab;
		nextPutAll: '^ ' , newName.	"Return new class"
	self class compile: code contents classified: 'conversion'.
	newName = 'PutNewClassHere' 
		ifTrue: [
			self 
				inform: 'Please complete the following method and 
then read-in the object file again.'.
			Smalltalk browseAllImplementorsOf: sel asSymbol].
	self flag: #violateBasicLayerPrinciples.
	"SmartRefStream should not refer to UI!!!!!!!!!! (sd)"

	"The class version number only needs to change under one specific circumstance.  That is when the first letters of the instance variables have stayed the same, but their meaning has changed.  A conversion method is needed, but this system does not know it.  
	If this is true for class Foo, define classVersion in Foo class.  
	Beware of previous object fileouts already written after the change in meaning, but before bumping the version number.  They have the old (wrong) version number, say 2.  If this is true, your method must be able to test the data and successfully read files that say version 2 but are really 3."
	^answ! !


!SmartRefStream class methodsFor: 'as yet unclassified' stamp: 'jmv 13/Mar/2012 12:47:00'!
cleanUpCategories
	| list valid removed newList newVers |
	"Look for all conversion methods that can't be used any longer.  Delete them."
	" SmartRefStream cleanUpCategories "

	"Two part selectors that begin with convert and end with a digit."
	"convertasossfe0: varDict asossfeu0: smartRefStrm"
	list := Symbol selectorsContaining: 'convert'.
	list := list select: [:symb | (symb beginsWith: 'convert') & (symb allButLast last isDigit)
				ifTrue: [(symb numArgs = 2)]
				ifFalse: [false]].
	valid := 0.  removed := 0.
	list do: [:symb |
		(Smalltalk allClassesImplementing: symb) do: [:newClass |
			newList := {newClass classVersion}, newClass allInstVarNames.
			newVers := self new versionSymbol: newList.
			(symb endsWith: (':',newVers,':')) 
				ifFalse: [
					"method is useless because can't convert to current shape"
					newClass removeSelector: symb.	"get rid of it"
					removed := removed + 1]
				ifTrue: [valid := valid + 1]]].
	Transcript
		newLine;
		show: 'Removed: ';
		print: removed; 
		show: '		Kept: ';
		print: valid;
		show: ' '! !


!BlockClosure methodsFor: 'objects from disk' stamp: 'jmv 26/Sep/2019 09:26:56'!
objectForDataStream: refStrm
	"I am about to be written on an object file.
	Write a textual reference if possible. If not, attempt converting to a serializable object.
	This might also fail. See #onBlockClosure:"

	self isCleanClosure ifTrue: [
		^ DiskProxy
			global: #Compiler
			selector: #evaluate:
			args: {self decompile decompileString} ].
	^self asSerializable! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'nice 10/May/2020 12:33:00'!
case: dist
	"statements = keyStmts CascadeFlag keyValueBlock ... keyStmts"

	| nextCase thenJump stmtStream elements b node cases otherBlock myExits |
	nextCase := pc + dist.

	"Now add CaseFlag & keyValueBlock to statements"
	statements addLast: stack removeLast.
	"Trick: put a flag on the stack.
	If it is the last case before otherwise: block, then
	- there won't be a dup of caseOf: receiver before sending =
	- there won't be a pop in the case handling block"
	stack addLast: OtherwiseFlag. "set for next pop"
	statements addLast: (self blockForCaseTo: nextCase).
		
	stack last == OtherwiseFlag
		ifTrue: "Last case"
			["ensure jump is within block (in case thenExpr returns wierdly I guess)"
			stack removeLast. "get rid of CaseFlag"
			stmtStream := ReadStream on: (self popTo: stack removeLast).
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == CaseFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: {stmtStream next}).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			"try find the end of the case"
			myExits := caseExits removeLast: elements size.
			myExits := myExits reject: [ :e | e isNil or: [ e < 0 or: [ e > method endPC ] ] ].
			thenJump := myExits isEmpty
							ifTrue: [ nextCase ]
							ifFalse: [ myExits max ].
			
			otherBlock := self blockTo: thenJump.
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf:otherwise: code: #macro)
					arguments: {cases. otherBlock})].! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 28/Dec/2024 14:42:00'!
jump: dist if: condition

	| savePc sign elsePc elseStart end cond ifExpr thenBlock elseBlock
	  thenJump elseJump condHasValue isIfNil saveStack |
	lastJumpIfPcStack addLast: lastPc.
	stack last == CaseFlag ifTrue: [^ [self case: dist] ensure: [lastJumpIfPcStack removeLast]].
	elsePc := lastPc.
	elseStart := pc + dist.
	end := limit.
	"Check for bfp-jmp to invert condition.
	Don't be fooled by a loop with a null body."
	sign := condition.
	savePc := pc.
	self interpretJump ifNotNil:
		[:elseDist|
		 (elseDist >= 0 and: [elseStart = pc]) ifTrue:
			 [sign := sign not.  elseStart := pc + elseDist]].
	pc := savePc.
	ifExpr := stack removeLast.
	(isIfNil := stack size > 0 and: [stack last == IfNilFlag]) ifTrue:
		[stack removeLast].
	saveStack := stack.
	stack := OrderedCollection new.
	thenBlock := self blockTo: elseStart.
	condHasValue := hasValue or: [isIfNil].
	"ensure jump is within block (in case thenExpr returns)"
	thenJump := exit <= end ifTrue: [exit] ifFalse: [elseStart].
	"if jump goes back, then it's a loop"
	thenJump < elseStart
		ifTrue:
			[| blockBody blockArgs savedReadCounts blockBodyReadCounts selector |
			 "Must be a while loop...
			  thenJump will jump to the beginning of the while expr.  In the case of while's
			  with a block in the condition, the while expr should include more than just
			  the last expression: find all the statements needed by searching for the node
			  with the relevant pc."
			stack := saveStack.
			savedReadCounts := tempReadCounts copy.
			pc := thenJump.
			blockBody := self statementsTo: elsePc.
			blockBodyReadCounts := tempReadCounts.
			savedReadCounts keysAndValuesDo:
				[:temp :count|
				 blockBodyReadCounts at: temp put: (blockBodyReadCounts at: temp) - count].
			tempReadCounts := savedReadCounts.
			"discard unwanted statements from block"
			blockBody size - 1 timesRepeat: [statements removeLast].
			blockArgs := thenBlock statements = constructor codeEmptyBlock statements
							ifTrue: [#()]
							ifFalse: [{ thenBlock }].
			selector := blockArgs isEmpty
							ifTrue: [sign ifTrue: [#whileFalse] ifFalse: [#whileTrue]]
							ifFalse: [sign ifTrue: [#whileFalse:] ifFalse: [#whileTrue:]].
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor codeSelector: selector code: #macro)
					arguments: blockArgs).
			pc := elseStart.
			selector == #whileTrue: ifTrue:
				[self convertToDoLoop: blockBodyReadCounts]]
		ifFalse:
			["Must be a conditional..."
			elseBlock := self blockTo: thenJump.
			elseJump := exit.
			"if elseJump is backwards, it is not part of the elseExpr"
			elseJump < elsePc ifTrue:
				[pc := lastPc].
			cond := isIfNil
						ifTrue:
							[constructor
								codeMessage: ifExpr ifNilReceiver
								selector: (constructor
											codeSelector: (sign ifTrue: [#ifNotNil:] ifFalse: [#ifNil:])
											code: #macro)
								arguments: {thenBlock}]
						ifFalse:
							[| args |
							args := sign
										ifTrue: [{elseBlock. thenBlock}]
										ifFalse: [{thenBlock. elseBlock}].
							(constructor
								decodeIfNilWithReceiver: ifExpr
								selector: #ifTrue:ifFalse:
								arguments: args
								tempReadCounts: tempReadCounts) ifNil:
									[constructor
										codeMessage: ifExpr
										selector: (constructor codeSelector: #ifTrue:ifFalse: code: #macro)
										arguments: args]].
			stack := saveStack.
			condHasValue
				ifTrue: [stack addLast: cond]
				ifFalse: [statements addLast: cond]].
	lastJumpIfPcStack removeLast! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'nice 10/May/2020 12:35:00'!
send: selector super: superFlag numArgs: numArgs

	| args rcvr selNode msgNode messages |
	args := Array new: numArgs.
	(numArgs to: 1 by: -1) do:
		[:i | args at: i put: stack removeLast].
	rcvr := stack removeLast.
	superFlag ifTrue: [rcvr := constructor codeSuper].
	selNode := constructor codeAnySelector: selector.
	rcvr == CaseFlag
		ifTrue:
			[| cases stmtStream elements node b |
			selector == #= ifTrue:
					[" = signals a case statement..."
					statements addLast: args first.
					stack addLast: rcvr. "restore CaseFlag"
					^ self].
			selector = #caseError ifFalse: [self error: 'unexpected message send while decompiling a caseOf:'].
			stmtStream := ReadStream on: (self popTo: stack removeLast).
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == CaseFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: {stmtStream next}).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf: code: #macro)
					arguments: {cases}).
			^self].
	rcvr == CascadeFlag
		ifTrue:
			["May actually be a cascade or an ifNil: for value."
			self willJumpIfFalse
				ifTrue: "= generated by a case macro"
					[selector == #= ifTrue:
						[" = signals a case statement..."
						statements addLast: args first.
						stack removeLast; addLast: CaseFlag; addLast: CaseFlag.	"Properly mark the case statement"
						^ self].
					selector == #== ifTrue:
						[" == signals an ifNil: for value..."
						stack removeLast; removeLast.
						rcvr := stack removeLast.
						stack addLast: IfNilFlag;
							addLast: (constructor
								codeMessage: rcvr
								selector: selNode
								arguments: args).
						^ self]]
				ifFalse:
					[(self willJumpIfTrue and: [selector == #==]) ifTrue:
						[" == signals an ifNotNil: for value..."
						stack removeLast; removeLast.
						rcvr := stack removeLast.
						stack addLast: IfNilFlag;
							addLast: (constructor
								codeMessage: rcvr
								selector: selNode
								arguments: args).
						^ self]].
			msgNode := constructor
							codeCascadedMessage: selNode
							arguments: args.
			stack last == CascadeFlag ifFalse:
				["Last message of a cascade"
				statements addLast: msgNode.
				messages := self popTo: stack removeLast.  "Depth saved by first dup"
				msgNode := constructor
								codeCascade: stack removeLast
								messages: messages]]
		ifFalse:
			[msgNode := constructor
						codeMessage: rcvr
						selector: selNode
						arguments: args].
	stack addLast: msgNode! !

!Decompiler methodsFor: 'private' stamp: 'di 26/Dec/1998 21:29:00'!
quickMethod

	method isReturnSpecial
		ifTrue: [^ constructor codeBlock:
				{constTable at: method primitive - 255} returns: true].
	method isReturnField
		ifTrue: [^ constructor codeBlock:
				{constructor codeInst: method returnField} returns: true].
	self error: 'improper short method'! !


!Message class methodsFor: 'instance creation' stamp: 'ST-80 31/May/1983 09:10:35'!
selector: aSymbol argument: anObject 
	"Answer an instance of me whose selector is aSymbol and single argument
	is anObject."

	^self new setSelector: aSymbol arguments: {anObject}! !


!Browser methodsFor: 'message category list' stamp: 'jmv 29/Mar/2012 18:00:00'!
messageCategoryList
	"Answer the selected category of messages."
	^selectedClassName
		ifNil: [ #() ]
		ifNotNil: [ {ClassOrganizer allCategory}, self rawMessageCategoryList ]! !


!Debugger methodsFor: 'private' stamp: 'Squeak1.0 20/Sep/1996 10:50:05'!
newStack: stack
	| oldStack diff |
	oldStack := contextStack.
	contextStack := stack.
	(oldStack == nil or: [oldStack last ~~ stack last])
		ifTrue: [contextStackList := contextStack collect: [:ctx | ctx printString].
				^ self].
	"May be able to re-use some of previous list"
	diff := stack size - oldStack size.
	contextStackList := diff <= 0
		ifTrue: [contextStackList copyFrom: 1-diff to: oldStack size]
		ifFalse: [diff > 1
				ifTrue: [contextStack collect: [:ctx | ctx printString]]
				ifFalse: [{stack first printString} , contextStackList]]! !


!Inspector methodsFor: 'accessing' stamp: 'jmv 23/May/2020 19:59:02'!
baseFieldList
	"Answer an Array consisting of 'self'
	and the instance variable names of the inspected object."

	^ {'self'. 'all inst vars'}
			, self objectClass allInstVarNames! !


!ContextVariablesInspector methodsFor: 'accessing' stamp: 'jmv 1/Mar/2010 09:51:00'!
fieldList 
	"Refer to the comment in Inspector|fieldList."

	object
		ifNil: [^ Array with: 'thisContext'].
	^fieldList ifNil:[fieldList := {'thisContext'. 'stack top'. 'all temp vars'} , object tempNames]! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 22/Jun/2013 19:38:00'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	"try to write reference for me"
	^ DiskProxy 
		global: #ChangeSet
		selector: #existingOrNewChangeSetNamed:forBaseSystem:
		args: {self name. self isForBaseSystem}
"===
	refStrm replace: self with: nil.
	^ nil
==="
! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 2/May/2024 16:17:58'!
to: anEnd by: aDuration
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #dateAndTime).
	self assert: aDuration class == Duration.
	^ (Schedule starting: self ending: anEnd)
		schedule: {aDuration};
		yourself.
! !


!Association methodsFor: 'objects from disk' stamp: 'jmv 2/Mar/2010 09:24:00'!
objectForDataStream: refStrm
	| dp |
	"I am about to be written on an object file.  If I am a known global, write a proxy that will hook up with the same resource in the destination system."

	^ (Smalltalk associationAt: key ifAbsent: nil) == self 
		ifTrue: [dp := DiskProxy global: #Smalltalk selector: #associationOrUndeclaredAt: 
							args: {key}.
			refStrm replace: self with: dp.
			dp]
		ifFalse: [self]! !


!MessageSend class methodsFor: 'instance creation' stamp: 'sma 29/Feb/2000 20:44:00'!
receiver: anObject selector: aSymbol argument: aParameter
	^ self receiver: anObject selector: aSymbol arguments: {aParameter}! !


!WeakMessageSend class methodsFor: 'instance creation' stamp: 'nk 25/Apr/2002 09:37:00'!
receiver: anObject selector: aSymbol argument: aParameter
	^ self receiver: anObject selector: aSymbol arguments: {aParameter}
! !


!Point methodsFor: 'point functions' stamp: 'len 9/May/2022 18:03:53'!
eightNeighbors
	^ { self + `1@0`.
		self + `1@1`.
		self + `0@1`.
		self + `-1@1`.
		self + `-1@0`.
		self + `-1@ -1`.
		self + `0@ -1`.
		self + `1@ -1`}! !

!Point methodsFor: 'point functions' stamp: 'len 9/May/2022 18:03:57'!
fourNeighbors
	^ { self + `1@0`.
		self + `0@1`.
		self + `-1@0`.
		self + `0@ -1`}! !


!Morph methodsFor: 'events-alarms' stamp: 'ar 11/Sep/2000 16:35:00'!
addAlarm: aSelector with: arg1 after: delayTime
	"Add an alarm (that is an action to be executed once) with the given set of parameters"
	^self addAlarm: aSelector withArguments: {arg1} after: delayTime! !

!Morph methodsFor: 'private' stamp: 'jmv 30/Sep/2025 09:47:36'!
privateAddMorph: aMorph atIndex: index position: aPointOrNil

	| oldIndex myWorld itsWorld oldOwner |
	aMorph == self ifTrue: [^ self error: 'can not be own submorph'].
	((index >= 1) and: [index <= (submorphs size + 1)])
		ifFalse: [^ self error: 'index out of range'].
	myWorld := self world.
	oldOwner := aMorph owner.
	(oldOwner == self and: [(oldIndex := submorphs indexOf: aMorph) > 0]) ifTrue: [
		oldIndex = index ifTrue: [ ^self ].
		"aMorph's position changes within in the submorph chain"
		oldIndex < index ifTrue: [
			"moving aMorph to back"
			submorphs replaceFrom: oldIndex to: index-1 with: submorphs startingAt: oldIndex+1.
			submorphs at: index put: aMorph.
		] ifFalse: [
			"moving aMorph to front"
			oldIndex-1 to: index by: -1 do: [ :i |
				submorphs at: i+1 put: (submorphs at: i)].
			submorphs at: index put: aMorph.
		].
		aPointOrNil ifNotNil: [aMorph privatePosition: aPointOrNil].
	] ifFalse: [
		"adding a new morph"
		oldOwner ifNotNil: [
			itsWorld := aMorph world.
			itsWorld ifNotNil: [aMorph invalidateBounds].
			oldOwner privateRemove: aMorph.
			oldOwner removedMorph: aMorph.
		].
		aMorph privateOwner: self.
		submorphs := submorphs copyReplaceFrom: index to: index-1 with: {aMorph}.
		aPointOrNil ifNotNil: [aMorph privatePosition: aPointOrNil].
		(itsWorld == myWorld) ifFalse: [aMorph intoWorld: myWorld].
	].
	myWorld ifNotNil: [
		index = 1
			ifTrue: [aMorph redrawNeeded ]
			ifFalse: [aMorph invalidateBounds]].
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged.
	oldOwner == self ifFalse: [
		self addedMorph: aMorph.
		aMorph noteNewOwner: self ]! !


!MenuItemMorph methodsFor: 'select/deselect' stamp: 'ML 12/May/2026 18:17:32'!
select
	self isSelected: true.
	owner activeSubmenu: subMenu.
	subMenu ifNotNil: [
		self displayBounds ifNotNil: [:r |
			subMenu delete.
			subMenu
				popUpAdjacentTo: {r topRight + `10@0`. r topLeft}
				from: self.
			subMenu setProperty: #forgiveParent toValue: owner.
			self world activeHand ifNotNil: [:hand |
				owner recordForgiveApexAt: hand morphPosition]].
		subMenu selectItem: nil]! !


!PluggableButtonMorph class methodsFor: 'example' stamp: 'jmv 25/Jul/2025 09:28:20'!
example
	"
	PluggableButtonMorph example openInWorld
	"

	| s1 s2 s3 b1 b2 b3 row |
	s1 := Switch new.
	s2 := Switch new turnOn.
	s3 := Switch new.
	s1 onAction: [s3 turnOn].
	s2 onAction: [s3 turnOff].
	s3 onAction: [s2 turnOff].
	b1 := (PluggableButtonMorph model: s1 stateGetter: #isOn action: #switch) label: 'S1'.
	b2 := (PluggableButtonMorph model: s2 stateGetter: #isOn action: #turnOn) label: 'S2'.
	b3 := (PluggableButtonMorph model: s3 stateGetter: nil action: #switch) 
		enableSelector: #isOn;
		label: 'S3'.
	b1 color: `Color lightRed`.
	b2 color: `Color lightRed`.
	b3 color: `Color lightRed`.
	row := LinearLayoutMorph newRow
		addMorphs: {b1. b2. b3};
		morphExtent: `120@35`.
	^ row! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 3/Apr/2023 10:42:10'!
openSingleMessageBrowser
	| msgName mr |
	"Create and schedule a message list browser populated only by the currently selected message"

	(msgName := model selectedMessageName) ifNil: [^ self].

	mr := MethodReference
		class: model selectedClassOrMetaClass
		selector: msgName.

	Smalltalk
		browseMessageList: {mr}
		name: mr stringVersion! !


!InlineMethodImplementorsStepWindow methodsFor: 'actions' stamp: 'FB 21/Nov/2021 21:00:23'!
setImplementorInApplier

	selectedImplementor := selectedImplementor ifNil: [model selection].
	applier implementors: (self compiledMethodsFrom: {selectedImplementor}).
	! !


!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 26/Nov/2024 10:23:03'!
explainClass: symbol 
	"Is symbol a class variable or a pool variable?"
	| class reply classes |
	(class := self codeProvider selectedClassOrMetaClass) ifNil: [^ nil].
	"no class is selected"
	(class isKindOf: Metaclass)
		ifTrue: [class := class soleInstance].
	classes := {class}
				, class allSuperclasses.
	"class variables"
	reply := classes detect: [:each | (each classVarNames detect: [:name | symbol = name]
					ifNone: nil)
					notNil]
				ifNone: nil.
	reply ifNotNil: [
		^ String streamContents: [:str |
			str
				nextPutAll: symbol;
				nextPutAll: ' is a class variable, defined in class ';
				nextPutAll: reply printString, '\' withNewLines;
				nextPutAll: 'Smalltalk browseAllCallsOn: (';
				nextPutAll: reply printString;
				nextPutAll: ' classPool associationAt: #';
				nextPutAll: symbol;
				nextPutAll: ').']].
	"pool variables"
	classes do: [:each | (each sharedPools
			detect: [:pool | (pool includesKey: symbol)
					and: 
						[reply := pool.
						true]]
			ifNone: nil)
			notNil].
	reply
		ifNil: [(Undeclared includesKey: symbol)
				ifTrue: [
					^ String streamContents: [:str |
						str
							nextPutAll: symbol;
							nextPutAll: ' is an undeclared variable.';
							nextPutAll: 'Smalltalk browseAllCallsOn: (Undeclared associationAt: #';
							nextPutAll: symbol;
							nextPutAll: ').']]]
		ifNotNil: 
			[classes := Array streamContents: [ :strm |
				Smalltalk
					allBehaviorsDo: [:each | (each sharedPools
							detect: 
								[:pool | 
								pool == reply]
							ifNone: nil)
							notNil ifTrue: [strm nextPut: each]]].
			"Perhaps not print whole list of classes if too long. (unlikely)"
			^ String streamContents: [:str |
				str
					nextPutAll: symbol;
					nextPutAll: ' is a pool variable from the pool ';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asPlainString;
					nextPutAll: ', which is used by the following classes ';
					nextPutAll: classes printString , '\' withNewLines;
					nextPutAll: 'Smalltalk browseAllCallsOn: (';
					nextPutAll: (Smalltalk keyAtIdentityValue: reply) asPlainString;
					nextPutAll: ' bindingOf: #';
					nextPutAll: symbol;
					nextPutAll: ').']].
	^ nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 26/Nov/2024 10:24:17'!
explainInst: string 
	"Is string an instance variable of this class?"
	| classes cls |
	cls := self codeProvider selectedClassOrMetaClass.
	cls ifNil: [^ nil].	  "no class known"
	classes := {cls}
				, cls allSuperclasses.
	classes := classes detect: [:each | (each instVarNames
			detect: [:name | name = string] ifNone: nil)
			notNil] ifNone: [^nil].
	classes := classes printString.
	^ String streamContents: [:str |
		str
			nextPutAll: string;
			nextPutAll: ' is an instance variable of the receiver; defined in class ';
			nextPutAll: classes, '\' withNewLines;
			nextPutAll: 'Smalltalk browseAllAccessesTo: ''';
			nextPutAll: string;
			nextPutAll: ''' from: ';
			nextPutAll: classes;
			nextPut: $.]! !


!AutoCompleterSelectorsCollector methodsFor: 'collecting' stamp: 'HAW 15/Feb/2019 15:04:08'!
addSelectorsOf: aClass upTo: aSuperclassToExclude

	self addSelectorsOfAll: {aClass} upTo: aSuperclassToExclude ! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: ''!
fileInMethod: selector
	^self fileInMethods: {selector}! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 28/Nov/2024 17:00:14'!
fileOutMethod: selector

	DirectoryEntry fileOutsDirQuery: (name,'-', selector asFileName, '.st') writeStreamDo: [ :fileStream |
		self fileOutMethods: {selector} on: fileStream ].! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'hlsf 24/Jun/2022 09:42:07'!
startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skipBack])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines := allLabels size.
	lineArray ifNil: [lineArray := #()].
	nLinesPer := segmentHeight // (Preferences at: #standardMenuFont) lineSpacing - 5.
	from := 1.
	[ true ] whileTrue: [
		to := (from + nLinesPer) min: nLines.
		subset := (allLabels copyFrom: from to: to) asOrderedCollection.
		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])
			before: subset first.
		subLines := lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].
		subLines := {1} , subLines.
		index := (PopUpMenu labels: subset printStringWithNewline lines: subLines)
					startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
		index = 1
			ifTrue: [from := to + 1.
					from > nLines ifTrue: [ from := 1 ]]
			ifFalse: [index = 0 ifTrue: [^ 0].
					^ from + index - 2]]! !


!ExtractMethod class methodsFor: 'instance creation' stamp: 'pau 17/Jul/2023 13:22:16'!
fromInterval: anIntervalToExtract of: aMethodToExtractCodeFrom to: newMessage categorizedAs: aCategory withArgumentNames: renamedArguments 

	^self 
		newDefinition: (ExtractMethodNewMethod 
			fromInterval: anIntervalToExtract 
			of: aMethodToExtractCodeFrom 
			to: newMessage 
			categorizedAs: aCategory 
			withArgumentNames: renamedArguments ) 
		replacements: {ExtractMethodReplacement 
			fromInterval: anIntervalToExtract 
			of: aMethodToExtractCodeFrom 
			to: newMessage}! !


!InlineMethod methodsFor: 'source code building - private' stamp: 'HAW 27/Jul/2023 17:53:10'!
statementWithMessageSend: aMessageNodeReference usedIn: aParseNode inlinedWith: anExpression addingParentheses: addParentheses

	| sourceRangeOfUsage enclosingNodeSourceRange normalizedMessageSendRange expression|

	enclosingNodeSourceRange := (self findRangesOf: aParseNode in: aMessageNodeReference methodNode completeSourceRanges) first.
	sourceRangeOfUsage := aMessageNodeReference completeSourceRange.
	normalizedMessageSendRange := (sourceRangeOfUsage first - enclosingNodeSourceRange first + 1)
					to: (sourceRangeOfUsage last - enclosingNodeSourceRange first + 1).
	"If the expression we are replacing is not at the end of the enclosing node, and it ends with a dot, we must remove it in order for it to be a valid statement"
	expression := ((sourceRangeOfUsage last < enclosingNodeSourceRange last) and: [anExpression endsWith: '.'])
		ifTrue: [anExpression allButLast] ifFalse: [anExpression].
	expression := addParentheses ifTrue: [self addParenthesesIfNeededTo: expression] ifFalse: [expression].

	^(aMessageNodeReference compiledMethod sourceCode copyFrom: enclosingNodeSourceRange first to: enclosingNodeSourceRange last)
		copyReplacing: {normalizedMessageSendRange -> expression}! !

