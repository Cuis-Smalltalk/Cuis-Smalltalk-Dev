'From Cuis 4.2 of 25 July 2013 [latest update: #2330] on 20 May 2015 at 12:53:22.966846 pm'!

!CodeFileBrowser methodsFor: 'edit pane' stamp: 'jmv 5/20/2015 12:39'!
selectedMessage
	"Answer a copy of the source code for the selected message selector."

	| class selector answer |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	answer _ (class sourceCodeAt: selector).
	Preferences browseWithPrettyPrint ifTrue: [
		answer _ (class compilerClass new
					format: answer
					in: class
					notifying: nil)].
	self showingAnyKindOfDiffs ifTrue: [
		answer _ (self
			methodDiffFor: answer
			class: self selectedClass
			selector: self selectedMessageName
			meta: self metaClassIndicated)].
	^ answer asText makeSelectorBoldIn: class! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 5/20/2015 12:50'!
scanSpecificChangeRecordType
	"Scan anything that involves more than one chunk"

	| itemPosition item item2 tokens firstToken secondToken stamp anIndex def isMeta name record methodReference doItOnlyIfInBaseSystem feature |
	itemPosition _ file position.
	item _ file nextChunk.
	(((item includesSubString: 'commentStamp:')
	or: [item includesSubString: 'methodsFor:']
	or: [item includesSubString: 'classDefinition:']
	or: [item includesSubString: 'classRemoval:']
	or: [item includesSubString: 'methodRemoval:'])
	or: [item includesSubString: 'methodMoveToSomePackage:']
	or: [item includesSubString: 'classMoveToSomePackage:']
	or: [item includesSubString: 'provides']
	or: [item includesSubString: 'requires'])
		ifFalse: [
			"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
			^ self addItem: (ChangeRecord new file: file position: itemPosition type: #preamble)
				 text: ('preamble: ' , item contractTo: 160)].

	tokens _ Scanner new scanTokens: item.
	tokens size >= 2 ifTrue: [
		stamp _ ''.
		anIndex _ tokens indexOf: #stamp: ifAbsent: nil.
		anIndex ifNotNil: [stamp _ tokens at: (anIndex + 1)].
		firstToken _ tokens first.
		secondToken _ tokens second.

		firstToken == #classDefinition: ifTrue: [
			itemPosition _ file position.
			isMeta _ secondToken includesSubString: ' class'.
			name _ isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
			def _ file nextChunk.
			record _ ChangeRecord new file: file position: itemPosition type: #classDefinition
				class: name asSymbol category: tokens last meta: isMeta stamp: nil.
			self addItem: record text: 'classDefinition: ', def.
			^ self ].

		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #classMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				self 
					addItem: (ClassDeletionChangeRecord new
						clsName: secondToken;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['clase move to some package: '] ifFalse: ['class removal: ']), secondToken ].
			^ self ].

		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ]) ifTrue: [
			doItOnlyIfInBaseSystem _ firstToken == #methodMoveToSomePackage:.
			itemPosition _ file position.
			item2 _ file nextChunk.
			item2 size > 0 ifTrue: [
				isMeta _ tokens third == #class.
				isMeta ifTrue: [secondToken substrings first] ifFalse: [secondToken].
				methodReference _ (MethodReference new
					setClassSymbol: secondToken
					classIsMeta: isMeta
					methodSymbol: tokens last
					stringVersion: secondToken, ' ', (isMeta ifTrue: ['class '] ifFalse: ['']), tokens last).
				self
					addItem: (MethodDeletionChangeRecord new
						methodReference: methodReference;
						doItOnlyIfInBaseSystem: doItOnlyIfInBaseSystem)
					text: 
						(doItOnlyIfInBaseSystem ifTrue: ['method move to some package: '] ifFalse: ['method removal: ']), 
							methodReference stringVersion ].
			^ self ].
		
		secondToken == #methodsFor: ifTrue: [
			^ self scanCategory: tokens third class: firstToken meta: false stamp: stamp].

		tokens third == #methodsFor: ifTrue: [
			^ self scanCategory: tokens fourth class: firstToken meta: true stamp: stamp].

		secondToken == #commentStamp: ifTrue: [
			stamp _ tokens third.
			self addItem:
					(ChangeRecord new file: file position: file position type: #classComment
									class: firstToken category: nil meta: false stamp: stamp)
					text: 'class comment for ' , firstToken, 
						  (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
			file nextChunk.
			^ self ].

		firstToken == #provides: ifTrue: [
			feature _ FeatureChangeRecord new
					type: #provides
					feature: (Feature name: secondToken version: tokens third revision: tokens fourth).
			self
				addItem: feature
				text: feature string ].

		firstToken == #requires: ifTrue: [
			feature _ FeatureChangeRecord new
					type: #requires
					feature: (FeatureRequirement name: secondToken minVersion: tokens third minRevision: tokens fourth maxVersion: (tokens size > 4 ifTrue: [tokens fifth])).
			self
				addItem: feature
				text: feature string ].
	]! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 5/20/2015 12:52'!
scanVersionsOf: method class: class meta: meta category: category selector: selector
	| position prevPos prevFileIndex preamble tokens sourceFilesCopy stamp |
	selectorOfMethod _ selector.
	currentCompiledMethod _ method.
	classOfMethod _ meta ifTrue: [class class] ifFalse: [class].
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	self addedChangeRecord ifNotNil: [ :change |
		self addItem: change text: ('{1} (in {2})' format: { change stamp. change fileName }) ].
	listIndex _ 0.
	position _ method filePosition.
	sourceFilesCopy _ SourceFiles collect:
		[:x | x ifNotNil: [ x readOnlyCopy]].
	method fileIndex = 0 ifTrue: [^ nil].
	file _ sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil]
		whileTrue:
		[file position: (0 max: position-150).  "Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevPos _ nil.
		stamp _ ''.
		(preamble findString: 'methodsFor:' startingAt: 1) > 0
			ifTrue: [tokens _ Scanner new scanTokens: preamble]
			ifFalse: [tokens _ Array new  "ie cant be back ref"].
		((tokens size between: 7 and: 8)
			and: [(tokens at: tokens size-5) = #methodsFor:])
			ifTrue: [
				(tokens at: tokens size-3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp _ tokens at: tokens size-2.
						prevPos _ tokens last.
						prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: prevPos.
						prevPos _ sourceFilesCopy filePositionFromSourcePointer: prevPos]
				ifFalse: ["Old format gives no stamp; prior pointer in two parts"
						prevPos _ tokens at: tokens size-2.
						prevFileIndex _ tokens last].
				(prevPos = 0 or: [prevFileIndex = 0]) ifTrue: [prevPos _ nil]].
		((tokens size between: 5 and: 6)
			and: [(tokens at: tokens size-3) = #methodsFor:])
			ifTrue: [
				(tokens at: tokens size-1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp _ tokens at: tokens size]].
 		self addItem:
				(ChangeRecord new file: file position: position type: #method
						class: class name category: category meta: meta stamp: stamp)
			text: stamp , ' ' , class name , (meta ifTrue: [' class '] ifFalse: [' ']) , selector.
		position _ prevPos.
		prevPos notNil ifTrue: [
			file _ sourceFilesCopy at: prevFileIndex]].
	sourceFilesCopy do: [:x | x notNil ifTrue: [x close]].
	self clearSelections! !


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 5/20/2015 12:51'!
scanVersionsOf: class
	"Scan for all past versions of the class comment of the given class"

	| oldCommentRemoteStr sourceFilesCopy position prevPos stamp preamble tokens prevFileIndex |

	classOfMethod _ class.
	oldCommentRemoteStr _ class  organization commentRemoteStr.
	currentCompiledMethod _ oldCommentRemoteStr.
	selectorOfMethod _ #Comment.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	oldCommentRemoteStr ifNil:[^ nil] ifNotNil: [oldCommentRemoteStr sourcePointer].

	sourceFilesCopy _ SourceFiles collect: [ :x | x ifNotNil: [x readOnlyCopy]].
	position _ oldCommentRemoteStr position.
	file _ sourceFilesCopy at: oldCommentRemoteStr sourceFileNumber.
	[ position notNil & file notNil]  whileTrue: [
		file position: (0 max: position-150).  " Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [
				preamble _ file nextChunk.
				file skipSeparators			"Skip any padding"
				].

		prevPos _ nil.
		stamp _ ''.
		(preamble findString: 'commentStamp:' startingAt: 1) > 0
			ifTrue: [
				tokens _ Scanner new scanTokens: preamble.
				(tokens at: tokens size-3) = #commentStamp: ifTrue: [
					"New format gives change stamp and unified prior pointer"
					stamp _ tokens at: tokens size-2.
					prevPos _ tokens last.
					prevFileIndex _ sourceFilesCopy fileIndexFromSourcePointer: prevPos.
					prevPos _ sourceFilesCopy filePositionFromSourcePointer: prevPos]]
			ifFalse: [
				"The stamp get lost, maybe after a condenseChanges"
				stamp _ '<historical>'].
 		self addItem:
				(ChangeRecord new file: file position: position type: #classComment
						class: class name category: nil meta: class isMeta stamp: stamp)
			text: stamp , ' ' , class name , ' class comment'. 
		prevPos = 0 ifTrue: [ prevPos _ nil ].
		position _ prevPos.
		prevPos notNil ifTrue: [ file _ sourceFilesCopy at: prevFileIndex ]].
	sourceFilesCopy do: [ :x | x notNil ifTrue: [ x close ]].
	self clearSelections! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 5/20/2015 12:50'!
contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Create an error if the category of the selected message is unknown. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."
	| category selector class oldSelector |

	(class _ self selectedClassOrMetaClass) ifNil:
		[(myChangeSet preambleString == nil or: [aString size = 0]) ifTrue: [ ^ false].
		(aString count: [:char | char == $"]) odd 
			ifTrue: [self inform: 'unmatched double quotes in preamble']
			ifFalse: [(Scanner new scanTokens: aString) size > 0 ifTrue: [
				self inform: 'Part of the preamble is not within double-quotes.
To put a double-quote inside a comment, type two double-quotes in a row.
(Ignore this warning if you are including a doIt in the preamble.)']].
		myChangeSet preambleString: aString.
		self currentSelector: nil.  "forces update with no 'unsubmitted chgs' feedback"
		^ true].
	oldSelector _ self selectedMessageName.
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector ifNil: [^ false].
	(self messageList includes: selector)
		ifTrue: [self currentSelector: selector]
		ifFalse: [self currentSelector: oldSelector].
	self update.
	^ true! !


!Inspector methodsFor: 'selecting' stamp: 'jmv 5/20/2015 12:41'!
accept: aString

	| result |
	result _ self doItReceiver class compilerClass new
				evaluate: (ReadStream on: aString)
				in: self doItContext
				to: self doItReceiver
				notifying: nil	"fix this"
				ifFail:  [^ false].
	acceptedContentsCache _ result printString.
	self replaceSelectionValue: result.	"may put contents back"
	self acceptedContentsChanged.
	^ true! !


!Behavior methodsFor: 'accessing' stamp: 'jmv 5/20/2015 12:45'!
decompilerClass
	"Answer a decompiler class appropriate for compiled methods of this class."

	^self compilerClass decompilerClass! !

!Behavior methodsFor: 'private' stamp: 'jmv 5/20/2015 12:48'!
basicCompile: code notifying: requestor trailer: bytes ifFail: failBlock
	"Compile code without logging the source in the changes file"

	| methodNode |
	methodNode _ self compilerClass new
				compile: code
				in: self
				notifying: requestor
				ifFail: failBlock.
	methodNode encoder requestor: requestor.
	^ CompiledMethodWithNode generateMethodFromNode: methodNode trailer: bytes.! !


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 5/20/2015 12:51'!
checkForInstVarsOK: instVarString
	"Return true if instVarString does no include any names used in a subclass"
	| instVarArray |
	instVarArray _ Scanner new scanFieldNames: instVarString.
	self allSubclasses do:
		[:cl | cl instVarNames do:
			[:n | (instVarArray includes: n)
				ifTrue: [self error: n , ' is already used in ' , cl name.
						^ false]]].
	^ true! !


!Class methodsFor: 'initialization' stamp: 'jmv 5/20/2015 12:51'!
declare: varString 
	"Declare class variables common to all instances. Answer whether 
	recompilation is advisable."

	| newVars conflicts |
	newVars _ 
		(Scanner new scanFieldNames: varString)
			collect: [:x | x asSymbol].
	newVars do: [:var | 
		var first isLowercase
			ifTrue: [self error: var, ' class variable name should be capitalized; proceed to include anyway.']].
	conflicts _ false.
	classPool
		ifNotNil: [
			(classPool keys reject: [:x | newVars includes: x]) 
				do: [:var | self removeClassVarName: var]].
	(newVars reject: [:var | self classPool includesKey: var])
		do: [:var | "adding"
			"check if new vars defined elsewhere"
			(self bindingOf: var)
				ifNotNil: [ 
					self error: var , ' is defined elsewhere'.
					conflicts _ true]].
	newVars size > 0
		ifTrue: [
			classPool _ self classPool.
			"in case it was nil"
			newVars do: [:var | classPool declare: var from: Undeclared]].
	^conflicts! !

!Class methodsFor: 'initialization' stamp: 'jmv 5/20/2015 12:51'!
sharing: poolString 
	"Set up sharedPools. Answer whether recompilation is advisable."
	| oldPools |
	oldPools _ self sharedPools.
	sharedPools _ OrderedCollection new.
	(Scanner new scanFieldNames: poolString) do: 
		[:poolName | 
		sharedPools add: (Smalltalk at: poolName asSymbol ifAbsent:[
			(self confirm: 'The pool dictionary ', poolName,' does not exist.',
						'\Do you want it automatically created?' withNewLines)
				ifTrue:[Smalltalk at: poolName asSymbol put: Dictionary new]
				ifFalse:[^self error: poolName,' does not exist']])].
	sharedPools isEmpty ifTrue: [sharedPools _ nil].
	^oldPools anySatisfy: [ :pool |
		self sharedPools noneSatisfy: [ :p | p == pool ]]! !


!BlockClosure methodsFor: 'printing' stamp: 'jmv 5/20/2015 12:44'!
decompile
	^Decompiler new decompileBlock: self! !


!Categorizer methodsFor: 'accessing' stamp: 'jmv 5/20/2015 12:50'!
changeFromString: aString 
	"Parse the argument, aString, and make this be the receiver's structure."

	| categorySpecs |
	categorySpecs _ Scanner new scanTokens: aString.
	"If nothing was scanned and I had no elements before, then default me"
	(categorySpecs isEmpty and: [elementArray isEmpty])
		ifTrue: [^ self setDefaultList: Array new].

	^ self changeFromCategorySpecs: categorySpecs! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 5/20/2015 12:51'!
class: oldClass instanceVariableNames: instVarString unsafe: unsafe
	"This is the basic initialization message to change the definition of
	an existing Metaclass"
	| instVars newClass needNew copyOfOldClass |
	instVars _ Scanner new scanFieldNames: instVarString.
	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateInstvars: instVars from: oldClass forSuper: oldClass superclass) ifFalse:[^nil].
		(self validateSubclassFormat: oldClass typeOfClass from: oldClass forSuper: oldClass superclass extra: instVars size) ifFalse:[^nil]].
	"See if we need a new subclass or not"
	needNew _ self needsSubclassOf: oldClass superclass type: oldClass typeOfClass instanceVariables: instVars from: oldClass.
	needNew ifNil:[^nil]. "some error"
	needNew ifFalse:[^oldClass]. "no new class needed"

	"Create the new class"
	copyOfOldClass _ oldClass copy.
	newClass _ self 
		newSubclassOf: oldClass superclass 
		type: oldClass typeOfClass
		instanceVariables: instVars
		from: oldClass.
		
	newClass _ self recompile: false from: oldClass to: newClass mutate: false.
	self doneCompiling: newClass.
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	^newClass! !

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 5/20/2015 12:51'!
name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory |
	instVars _ Scanner new scanFieldNames: instVarString.
	classVars _ (Scanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass _ Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass _ nil]. "Already checked in #validateClassName:"
	copyOfOldClass _ oldClass copy.

	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].
		(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil]].

	"See if we need a new subclass"
	needNew _ self needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass.
	needNew ifNil: [^nil]. "some error"

	(needNew and:[unsafe not]) ifTrue:[
		"Make sure we don't redefine any dangerous classes"
		(self tooDangerousClasses includes: oldClass name) ifTrue:[
			self error: oldClass name, ' cannot be changed'.
		].
		"Check if the receiver should not be redefined"
		(oldClass notNil and:[oldClass shouldNotBeRedefined]) ifTrue:[
			self notify: oldClass name asText allBold, 
						' should not be redefined!! \Proceed to store over it.' withNewLines]].

	needNew ifTrue:[
		"Create the new class"
		newClass _ self 
			newSubclassOf: newSuper 
			type: type 
			instanceVariables: instVars
			from: oldClass.
		newClass ifNil: [ ^nil]. "Some error"
		newClass setName: className.
	] ifFalse:[
		"Reuse the old class"
		newClass _ oldClass.
	].

	"Install the class variables and pool dictionaries... "
	force _ (newClass declare: classVarString) | (newClass sharing: poolString).

	"... classify ..."
	newCategory _ category asSymbol.
	organization _ Smalltalk organization.
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	newClass _ self recompile: force from: oldClass to: newClass mutate: false.

	"... export if not yet done ..."
	(Smalltalk at: newClass name ifAbsent: nil) == newClass ifFalse:[
		[Smalltalk at: newClass name put: newClass]
			on: AttemptToWriteReadOnlyGlobal do:[:ex| ex resume: true].
		Smalltalk flushClassNameCache.
	].

	self doneCompiling: newClass.
	
	"... notify interested clients ..."
	oldClass ifNil: [
		SystemChangeNotifier uniqueInstance classAdded: newClass inCategory: newCategory.
		^ newClass].
	SystemChangeNotifier uniqueInstance classDefinitionChangedFrom: copyOfOldClass to: newClass.
	newCategory ~= oldCategory 
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: category].
	^newClass! !


!CodeFile methodsFor: 'private' stamp: 'jmv 5/20/2015 12:51'!
classDefinition: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	tokens size = 11 ifFalse:[^doIts add: chgRec].
	theClass := self getClass: (tokens at: 3).
	theClass definition: string.
	classOrder add: theClass.! !

!CodeFile methodsFor: 'private' stamp: 'jmv 5/20/2015 12:51'!
metaClassDefinition: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	theClass := self getClass: (tokens at: 1).
	theClass metaClass definition: string.
	classOrder add: theClass metaClass.! !

!CodeFile methodsFor: 'private' stamp: 'jmv 5/20/2015 12:51'!
msgClassComment: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	(tokens size = 3 and:[(tokens at: 3) class == String]) ifTrue:[
		theClass := self getClass: tokens first.
		^theClass classComment: tokens last].
	(tokens size = 4 and:[(tokens at: 3) asString = 'class' and:[(tokens at: 4) class == String]]) ifTrue:[
		theClass := self getClass: tokens first.
		theClass metaClass classComment: tokens last].
! !

!CodeFile methodsFor: 'private' stamp: 'jmv 5/20/2015 12:51'!
possibleSystemSource: chgRec
	| tokens |
	sourceSystem isEmpty ifTrue:[
		tokens := Scanner new scanTokens: chgRec string.
		(tokens size = 1 and:[tokens first class == String]) ifTrue:[
			sourceSystem := tokens first.
			^self]].
	doIts add: chgRec.! !

!CodeFile methodsFor: 'private' stamp: 'jmv 5/20/2015 12:51'!
removedMethod: string with: chgRec
	| class tokens firstToken secondToken thirdToken |
	tokens _ Scanner new scanTokens: string.
	tokens size >= 3 ifTrue: [
		firstToken _ tokens at: 1.
		secondToken _ tokens at: 2.
		thirdToken _ tokens at: 3.
		(tokens size = 3 and: [ secondToken == #removeSelector: or: [ secondToken == #removeSelectorIfInBaseSystem: ]]) ifTrue:[
			class _ self getClass: firstToken.
			^class perform: secondToken with: thirdToken.
		].
		(tokens size = 4 and: [ secondToken == #class and: [ thirdToken == #removeSelector: or: [ thirdToken == #removeSelectorIfInBaseSystem: ]]]) ifTrue:[
			class _ self getClass: firstToken.
			^class metaClass perform: thirdToken with: (tokens at: 4).
		].
	].
	doIts add: chgRec! !


!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 5/20/2015 12:51'!
possibleDescription: chgRec
	| tokens prefix token |
	description isEmpty ifTrue:[
		tokens _ Scanner new scanTokens: chgRec string.
		(tokens size = 1 and: [ (token _ tokens first) class == String ]) ifTrue: [
			prefix _ 'Description '.
			(token beginsWith: prefix) ifTrue: [
				description _ token copyFrom: prefix size + 1 to: token size.
				^self ]]].
	doIts add: chgRec.! !


!Symbol methodsFor: 'printing' stamp: 'jmv 5/20/2015 12:52'!
storeOn: aStream 

	aStream nextPut: $#.
	(Scanner isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 5/20/2015 12:52'!
readFrom: strm  "Symbol readFromString: '#abc'"

	strm peek = $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"! !


!InstructionStream methodsFor: 'testing' stamp: 'jmv 5/20/2015 12:49'!
willBlockReturn

	^ (self method at: pc) = Encoder blockReturnCode! !

!InstructionStream methodsFor: 'testing' stamp: 'jmv 5/20/2015 12:49'!
willJustPop

	^ (self method at: pc) = Encoder popCode! !


!Parser class methodsFor: 'services' stamp: 'jmv 5/20/2015 12:46'!
methodHeaderLengthFrom: methodSource

	| parser |
	parser _ self new.
	parser parseSelector: methodSource.
	^ parser endOfLastToken min: methodSource size! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 5/20/2015 12:52'!
isLegalInstVarName: aString
	"Answer whether aString is a legal instance variable name."

	^ ((Scanner isLiteralSymbol: aString) and: [(aString includes: $:) not]) and:
		[(self inviolateInstanceVariableNames includes:  aString) not]! !

!methodRemoval: SystemDictionary #actualCompiledMethodWithNodeClass!
SystemDictionary removeSelector: #actualCompiledMethodWithNodeClass!
!methodRemoval: SystemDictionary #actualDecompilerClass!
SystemDictionary removeSelector: #actualDecompilerClass!
!methodRemoval: SystemDictionary #actualEncoderClass!
SystemDictionary removeSelector: #actualEncoderClass!
!methodRemoval: SystemDictionary #actualParserClass!
SystemDictionary removeSelector: #actualParserClass!
!methodRemoval: SystemDictionary #actualScannerClass!
SystemDictionary removeSelector: #actualScannerClass!

!SystemDictionary reorganize!
('accessing' classes organization)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClass:as: renameClassNamed:as:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources fixSourceCodeLineEndings macroBenchmark1 macroBenchmark3 makeInternalRelease obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories removeTextCode testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass exitToDebugger extraVMMemory extraVMMemory: getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primVmFileName setMacFileInfoOn: unloadModule: vmFileName vmParameterAt: vmParameterAt:put: vmVersion)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjectsDo: allObjectsSelect: allPrimitiveMessages allPrimitiveMethodsInCategories: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: lastQuitLogPosition logSapshot:andQuit: okayToDiscardUnsavedCode okayToSave printStuffToCleanOnImageSave processShutDownList: processStartUpList: quitPrimitive quitPrimitive: removeFromShutDownList: removeFromStartUpList: saveAs saveAs:andQuit:clearAllClassState: saveAsEmbeddedImage saveAsNewVersion saveSession send:toClassesNamedIn:with: setGCParameters setPlatformPreferences snapshot:andQuit:clearAllClassState: snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotPrimitive unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles datedVersion endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray externalObjects hasSpecialSelector:ifTrueSetByte: primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize specialSelectors unregisterExternalObject:)
('toDeprecate' getFileNameFromUser)
('ui' beep confirmRemovalOf:on: globals inspectGlobals primitiveBeep)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnnededRedefinitions browseMessageList:name: browseMessageList:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do:)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit processHasThreadIdInstVar: processPreemptionYields processPreemptionYields:)
('query' hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: namesBeginningWith:do: namesBeginningWith:do:in:)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' processCommandLineArgument:storeStartUpScriptArgsOn: processCommandLineArguments readCommandLineArguments setStartupStamp startUpArguments startUpScriptArguments startup:)
!

