'From Cuis 4.2 of 25 July 2013 [latest update: #2907] on 20 August 2016 at 10:26:34 pm'!

!Object class methodsFor: 'instance creation' stamp: 'jmv 8/20/2016 22:14:48'!
readFrom: aStream
	"Create an object based on the contents of aStream."

	| object ok |
	ok _ (aStream is: #Stream) or: [ aStream isString ].
	(ok or: [ aStream is: #Text ]) ifFalse: [ ^ self error: 'expected String or Text' ].
	object _ Compiler evaluate: aStream.
	(object isKindOf: self) ifFalse: [ self error: self name, ' expected' ].
	^ object! !


!CodeProvider methodsFor: 'message list' stamp: 'jmv 8/20/2016 22:16:53'!
decompiledSource
	"Obtain a source string by decompiling the method's code.
	Also return the string."
	| class selector method answer |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	"Was method deleted while in another project?"
	method _ class
		compiledMethodAt: selector
		ifAbsent: [ ^ '' ].
	currentCompiledMethod _ method.
	"decompile without temp names "
"	answer _ (class decompilerClass new"
	"See comment at #useOldCompiler"
	answer _ (Decompiler new
		decompile: selector
		in: class
		method: method) decompileString.
	^ answer copy! !

!CodeProvider methodsFor: 'message list' stamp: 'jmv 8/20/2016 22:19:17'!
validateMessageSource: sourceString forSelector: aSelector inClass: theClass
	"Check whether there is evidence that method source is invalid"

"	(theClass parserClass selectorFrom: sourceString asString) = aSelector"
	"See comment at #useOldCompiler"
	(Parser selectorFrom: sourceString asString) = aSelector
		ifFalse: [
			self inform: 'There may be a problem with your sources file!!

The source code for every method should (usually) start with the
method selector but this is not the case with this method!! You may
proceed with caution but it is recommended that you get a new source file.'].! !


!Browser methodsFor: 'class functions' stamp: 'jmv 8/20/2016 22:13:35'!
copyClass
	| originalClass originalName copysName  newDefinition newMetaDefinition newClass |
	selectedClassName ifNil: [^ self].
	originalClass := self selectedClass.
	originalName := originalClass name.
	copysName _ self request: 'Please type new class name' initialAnswer: originalName.
	copysName = '' ifTrue: [^ self].  " Cancel returns '' "
	copysName _ copysName asSymbol.
	copysName = originalName ifTrue: [^ self].
	(Smalltalk includesKey: copysName)
		ifTrue: [^ self error: copysName , ' already exists'].
	newDefinition := originalClass definition
		copyReplaceAll: originalName printString
		with: copysName printString.
	newClass _ Compiler evaluate: newDefinition logged: true.
	newMetaDefinition := originalClass class definition
		copyReplaceAll: originalClass class name
		with: newClass class name.
	Compiler evaluate: newMetaDefinition logged: true.
	newClass copyAllCategoriesFrom: originalClass.
	newClass class copyAllCategoriesFrom: originalClass class.
	originalClass hasComment ifTrue: [
		newClass comment: originalClass comment ].			
	self classListIndex: 0.
	self changed: #classList! !

!Browser methodsFor: 'class functions' stamp: 'jmv 8/20/2016 22:13:42'!
defineClass: defString notifying: aRequestor  
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aRequestor."
	| oldClass class newClassName defTokens keywdIx |
	oldClass _ self selectedClassOrMetaClass.
	defTokens _ defString findTokens: Character separators.
	keywdIx _ defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx _ defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName _ (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [ oldClass theNonMetaClass name asString ~= newClassName ])
		and: [ Smalltalk includesKey: newClassName asSymbol ]) ifTrue: [
			"Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: (newClassName bold , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?'))
				ifFalse: [ ^ false ]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil: [ oldClass _ oldClass superclass ].
	class _ Compiler
				evaluate: defString
				notifying: aRequestor
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: [
				self changed: #systemCategoryList.
				self changed: #classList.
				self changed: #clearUserEdits.
				self setClass: class selector: nil.
				"self clearUserEditFlag; editClass."
				^ true ]
		ifFalse: [ ^ false ]! !

!Browser methodsFor: 'message functions' stamp: 'jmv 8/20/2016 22:19:04'!
defineMessageFrom: aString notifying: aRequestor
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList |
	selectedMessageName _ self selectedMessageName.
	oldMessageList _ self messageList.
"	selector _ self selectedClassOrMetaClass parserClass selectorFrom: aString."
	"See comment at #useOldCompiler"
	selector _ Parser selectorFrom: aString.
	(self metaClassIndicated
		and: [(self selectedClassOrMetaClass includesSelector: selector) not
		and: [Metaclass isScarySelector: selector]])
		ifTrue: ["A frist-time definition overlaps the protocol of Metaclasses"
				(self confirm: (selector bold, ' is used in the existing class system.
Overriding it could cause serious problems.
Is this really what you want to do?'))
				ifFalse: [^nil]].
	selector _ self selectedClassOrMetaClass
				compile: aString
				classified: (category _ self selectedMessageCategoryName)
				notifying: aRequestor.
	selector
		ifNil: [^ nil].
	selector ~~ selectedMessageName
		ifTrue: [
			category = ClassOrganizer nullCategory
				ifTrue: [self changed: #classSelectionChanged.
						self changed: #classList.
						self messageCategoryListIndex: 1].
			self setClassOrganizer.  "In case organization not cached"
			(oldMessageList includes: selector)
				ifFalse: [self changed: #messageList].
			self messageListIndex: (self messageList indexOf: selector)].
	^ selector! !


!MessageSet methodsFor: 'private' stamp: 'jmv 8/20/2016 22:14:43'!
contents: aString notifying: aRequestor 
	"Compile the code in aString. Notify aRequestor of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| category selector class oldSelector |
	self okayToAccept ifFalse: [^ false].
	selectedMessage ifNil: [^ false].
	class _ selectedMessage actualClass.
	oldSelector _ selectedMessage methodSymbol.
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue:
		[oldSelector == #Comment ifTrue:
			[class comment: aString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue: [
			Compiler
				evaluate: aString
				notifying: aRequestor
				logged: true.
			self changed: #clearUserEdits.
 			^ false].
		oldSelector == #Hierarchy ifTrue:
			[self inform: 'To change the hierarchy, edit the class definitions'. 
			^ false]].
	"Normal method accept"
	category _ class organization categoryOfElement: oldSelector.
	selector _ class compile: aString
				classified: category
				notifying: aRequestor.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	self triggerEvent: #annotationChanged.
	^ true! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/20/2016 22:19:09'!
scanCategory: category class: class meta: meta stamp: stamp
	| itemPosition method |
	[
		itemPosition _ file position.
		method _ file nextChunk.
		method size > 0 ]						"done when double terminators"
			whileTrue: [
				self
					addItem: (ChangeRecord new file: file position: itemPosition type: #method
							class: class category: category meta: meta stamp: stamp)
					text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
"						, ((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method)"
	"See comment at #useOldCompiler"
						, (Parser selectorFrom: method)
						, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/20/2016 22:20:23'!
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

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 8/20/2016 22:22:51'!
selectEquivalentMethods
	"Selects all method definitions for which there is already an equivalent method in the current image, 
	(meaning that the difference is cosmetic and not in behavior)"
	1 to: changeList size do: [ :i | 
		| change class |
		change _ changeList at: i.
		listSelections at: i put:
			((change changeType == #method and: [
				(class _ change changeClass) notNil]) and: [
					(class includesSelector: change methodSelector) and: [
						| cmWithNode |
						cmWithNode _ [class basicCompile: change string notifying: nil trailer: class defaultMethodTrailer ifFail: nil] 
							on: SyntaxErrorNotification do: [ :ex | ex return ].
						(cmWithNode notNil and: [
							| current inChange |
							current _ (class compiledMethodAt: change methodSelector) copyWithTrailerBytes: #(0).
							inChange _ cmWithNode method copyWithTrailerBytes: #(0).
							current = inChange or: [
								| currentCmWithNode |
"								currentCmWithNode _ [class basicCompile: (class decompilerClass new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ]."
								"See comment at #useOldCompiler"
								currentCmWithNode _ [class basicCompile: (Decompiler new decompile: change methodSelector in: class) decompileString
										notifying: nil trailer: class defaultMethodTrailer ifFail: nil] on: SyntaxErrorNotification do: [ :ex | ex return ].
								(currentCmWithNode notNil and: [
									current _ currentCmWithNode method copyWithTrailerBytes: #(0).
									current = inChange])
							]
						])
					]]
			)].
	self changed: #allSelections.
	self changed: #annotation! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 8/20/2016 22:22:23'!
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
		[:x | x ifNotNil: [ x name asFileEntry readStream ]].
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


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 8/20/2016 22:20:59'!
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

	sourceFilesCopy _ SourceFiles collect: [ :x | x ifNotNil: [x name asFileEntry readStream]].
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


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 8/20/2016 22:20:28'!
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


!Debugger methodsFor: 'accessing' stamp: 'jmv 8/20/2016 22:19:29'!
contents: aText notifying: aController
	"The retrieved information has changed and its source must now be updated.
	 In this case, the retrieved information is the method of the selected context."
	| result selector classOfMethod category h ctxt newMethod |
	contextStackIndex = 0 ifTrue:
		[^false].
	self selectedContext isExecutingBlock ifTrue:
		[h := self selectedContext activeHome.
		 h ifNil:
			[self inform: 'Method for block not found on stack, can''t edit and continue'.
			 ^false].
		 (self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withNewLines) ifFalse:
			[^false].
		self resetContext: h.
		(result := self contents: aText notifying: aController) ifTrue: [
			self acceptedContentsChanged].
		^result].

	classOfMethod := self selectedClass.
	category := self selectedMessageCategoryName.
"	selector :=self selectedClass parserClass selectorFrom: aText."
	"See comment at #useOldCompiler"
	selector :=Parser selectorFrom: aText.
	(selector == self selectedMessageName
	 or: [(self selectedMessageName beginsWith: 'DoIt')
		and: [selector numArgs = self selectedMessageName numArgs]]) ifFalse:
		[self inform: 'can''t change selector'.
		 ^false].
	selector := classOfMethod
				compile: aText
				classified: category
				notifying: aController.
	selector ifNil: [^false]. "compile cancelled"
	newMethod := classOfMethod compiledMethodAt: selector.
	newMethod isQuick ifTrue:
		[self down.
		 self selectedContext jump: (self selectedContext previousPc - self selectedContext pc)].
	ctxt := interruptedProcess popTo: self selectedContext.
	ctxt == self selectedContext
		ifFalse:
			[self inform: 'Method saved, but current context unchanged\because of unwind error. Click OK to see error' withNewLines]
		ifTrue:
			[newMethod isQuick ifFalse:
				[interruptedProcess
					restartTopWith: newMethod;
				 	stepToSendOrReturn].
			contextVariablesInspector object: nil].
	self resetContext: ctxt.
	^true! !

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 8/20/2016 22:14:27'!
returnValue: expression
	"Force a return of a given value to the previous context!!"

	| previous selectedContext value |
	contextStackIndex = 0 ifTrue: [^Smalltalk beep].
	selectedContext _ self selectedContext.
	value _ Compiler new 
				evaluate: expression
				in: selectedContext
				to: selectedContext receiver.
	previous _ selectedContext sender.
	self resetContext: previous.
	interruptedProcess popTo: previous value: value! !


!Behavior methodsFor: 'accessing' stamp: 'jmv 8/20/2016 22:13:08'!
compilerClass
	"Answer a compiler class appropriate for source methods of this class."

	^Compiler! !

!Behavior methodsFor: 'compiling' stamp: 'jmv 8/20/2016 22:16:22'!
decompile: selector 
	"Find the compiled code associated with the argument, selector, as a 
	message selector in the receiver's method dictionary and decompile it. 
	Answer the resulting source code as a string. Create an error notification 
	if the selector is not in the receiver's method dictionary."

"	^self decompilerClass new decompile: selector in: self"
	"See comment at #useOldCompiler"
	^Decompiler new decompile: selector in: self! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 8/20/2016 22:18:54'!
firstPrecodeCommentFor:  selector
	"If there is a comment in the source code at the given selector that preceeds the body of the method, return it here, else return nil"

	| parser source tree |
	"Behavior firstPrecodeCommentFor: #firstPrecodeCommentFor:"
	(MessageSet isPseudoSelector: selector)
		ifTrue:
			["Not really a selector"
			^ nil].
	source _ self sourceCodeAt: selector asSymbol ifAbsent: [^ nil].
"	parser _ self parserClass new."
	"See comment at #useOldCompiler"
	parser _ Parser new.
	tree _ 
		parser
			parse: (ReadStream on: source)
			class: self
			noPattern: false
			context: nil
			notifying: nil
			ifFail: [^ nil].
	^ (tree comment ifNil: [^ nil]) first! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 8/20/2016 22:18:59'!
methodHeaderFor: selector 
	"Answer the string corresponding to the method header for the given selector"

	| methodSource |
	methodSource _ self ultimateSourceCodeAt: selector ifAbsent: [self standardMethodHeaderFor: selector].
"	^methodSource copyFrom: 1 to: (self parserClass methodHeaderLengthFrom: methodSource)"
	"See comment at #useOldCompiler"
	^methodSource copyFrom: 1 to: (Parser methodHeaderLengthFrom: methodSource)

"
Behavior methodHeaderFor: #methodHeaderFor:
"
! !

!Behavior methodsFor: 'private' stamp: 'jmv 8/20/2016 22:12:43'!
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


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 8/20/2016 22:21:04'!
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


!Class methodsFor: 'initialization' stamp: 'jmv 8/20/2016 22:20:33'!
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

!Class methodsFor: 'initialization' stamp: 'jmv 8/20/2016 22:20:39'!
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


!Categorizer methodsFor: 'accessing' stamp: 'jmv 8/20/2016 22:20:16'!
changeFromString: aString 
	"Parse the argument, aString, and make this be the receiver's structure."

	| categorySpecs |
	categorySpecs _ Scanner new scanTokens: aString.
	"If nothing was scanned and I had no elements before, then default me"
	(categorySpecs isEmpty and: [elementArray isEmpty])
		ifTrue: [^ self setDefaultList: Array new].

	^ self changeFromCategorySpecs: categorySpecs! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 8/20/2016 22:20:44'!
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

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 8/20/2016 22:20:51'!
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


!BlockClosure methodsFor: 'printing' stamp: 'jmv 8/20/2016 22:16:27'!
decompile
	^Decompiler new decompileBlock: self! !

!BlockClosure methodsFor: 'testing' stamp: 'jmv 8/20/2016 22:13:25'!
isTrivialClosure
	"A trivial closure is one that doesn't really need the home context because:
		- It doesn't send messages to self or super
		- It doesn't access any instance variable
		- It doesn't access any outer temp
		- It doesn't do ^ return (return from method to caller)
	Therefore it doesn't close over a lexical scope, and in this sense they are trivial.
	They can also be called 'context free', 'clean' or 'simple block'.
	"

	| recreated source |
	source _ self decompile decompileString.

	"This catches any acess to outer context!!"
	recreated _ [ Compiler evaluate: source ] on: UndeclaredVariableWarning do: [ :ex | ^ false ].

	"Fail if returns from outer context, or uses self"
	Smalltalk
		eliotsClosureMeasurementsOn: recreated outerContext method
		over: [ :closuresCount :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
			anyClosureDoesMethodReturn ifTrue: [ ^ false ].
			anyClosureUsesSelf ifTrue: [ ^ false ]].

	"Ok."
	^true! !


!BlockContext methodsFor: 'accessing' stamp: 'jmv 8/20/2016 22:17:42'!
hasInstVarRef
	"Answer whether the receiver references an instance variable."

	| method scanner end printer |

	home ifNil: [^false].
	method := self method.
	end := self endPC.
	scanner := InstructionStream new method: method pc: startpc.
	printer := InstVarRefLocator new.

	[scanner pc <= end] whileTrue: [
		(printer interpretNextInstructionUsing: scanner) ifTrue: [^true].
	].
	^false! !

!BlockContext methodsFor: 'printing' stamp: 'jmv 8/20/2016 22:16:32'!
decompile
	"^ home method decompilerClass new decompileBlock: self"
	"See comment at #useOldCompiler"
	^ Decompiler new decompileBlock: self! !


!String methodsFor: 'converting' stamp: 'jmv 8/20/2016 22:21:59'!
findSelector
    "Revised to use scanner for better removal of extraneous stuff"

    | sel |

    sel := self withBlanksTrimmed.
    (sel includes: $:) ifTrue: [sel _ Scanner findSelectorIn: sel].
    sel isEmpty ifTrue: [^ nil].
    Symbol hasInterned: sel ifTrue:
        [:aSymbol | ^ aSymbol].
    ^ nil! !


!Symbol methodsFor: 'printing' stamp: 'jmv 8/20/2016 22:22:03'!
storeOn: aStream 

	aStream nextPut: $#.
	(Scanner isLiteralSymbol: self)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]! !


!Symbol class methodsFor: 'instance creation' stamp: 'jmv 8/20/2016 22:22:08'!
readFrom: strm
	"
	Symbol readFrom: '#abc' readStream
	"

	strm peek = $# ifFalse: [self error: 'Symbols must be introduced by #'].
	^ (Scanner new scan: strm) advance  "Just do what the code scanner does"! !


!Array methodsFor: 'converting' stamp: 'jmv 8/20/2016 22:13:00'!
evalStrings
	   "Allows you to construct literal arrays.
    #(true false nil '5@6' 'Set new' '''text string''') evalStrings
    gives an array with true, false, nil, a Point, a Set, and a String
    instead of just a bunch of Symbols"

    ^ self collect: [:each |  | it |
        it _ each.
        each == #true ifTrue: [it _ true].
		      each == #false ifTrue: [it _ false].
        each == #nil ifTrue: [it _ nil].
        each class == String ifTrue: [
			it _ Compiler evaluate: each].
        each class == Array ifTrue: [it _ it evalStrings].
        it]! !


!CompiledMethod methodsFor: 'printing' stamp: 'jmv 8/20/2016 22:18:23'!
longPrintOn: aStream indent: tabs
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue: 
		[self isReturnSpecial ifTrue:
			[^ aStream tab: tabs; nextPutAll: 'Quick return ' , 
				(#('self' 'true' 'false' 'nil' '-1' '0' '1' '2') at: self primitive - 255)].
		^ aStream nextPutAll: 'Quick return field ' , self returnField printString , ' (0-based)'].

	self primitive = 0 ifFalse: [
		aStream tab: tabs.
		self printPrimitiveOn: aStream.
	].
	(InstructionPrinter on: self) indent: tabs; printInstructionsOn: aStream.
! !

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 8/20/2016 22:20:01'!
longPrintRelativeOn: aStream indent: tabs
	"List of all the byte codes in a method with a short description of each" 

	self isQuick ifTrue: 
		[^self longPrintOn: aStream indent: tabs].
	self primitive = 0 ifFalse:
		[aStream tab: tabs. self printPrimitiveOn: aStream].
	(RelativeInstructionPrinter on: self)
		indent: tabs;
		printCode: false;
		printInstructionsOn: aStream.
! !

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 8/20/2016 22:18:28'!
symbolicLinesDo: aBlock
	"Evaluate aBlock with each of the lines in the symbolic output."

	| aStream pc firstLine |
	aStream := ReadWriteStream on: (String new: 64).
	self isQuick ifTrue:
		[self longPrintOn: aStream.
		 aBlock value: 0 value: aStream contents.
		 ^self].

	self primitive ~= 0 ifTrue:
		[self printPrimitiveOn: aStream.
		 aBlock value: 1 value: aStream contents.
		 aStream resetContents].

	pc := self initialPC.
	(InstructionPrinter on: self)
		indent: 0;
		printPC: false; "explorer provides pc anyway"
		printInstructionsOn: aStream
		do:	[:printer :scanner :stream| | line index |
			line := stream contents allButLast.
			firstLine _ line lines first.
			firstLine size < line size ifTrue: [
				line _ firstLine, '...'' (continues)'].
			(index := line indexOf: $>) > 0 ifTrue:
				[[(line at: index + 1) isSeparator] whileTrue: [index := index + 1].
				 line := ((line copyFrom: 1 to: index) copyReplaceAll: (String with: Character tab) with: (String new: 8 withAll: Character space)),
						(line copyFrom: index + 1 to: line size)].
			aBlock value: pc value: line.
			pc := scanner pc.
			stream resetContents]! !

!CompiledMethod methodsFor: 'scanning' stamp: 'jmv 8/20/2016 22:17:46'!
hasInstVarRef
	"Answer whether the method references an instance variable."

	| scanner end printer |

	scanner := InstructionStream on: self.
	printer := InstVarRefLocator new.
	end := self endPC.

	[scanner pc <= end] whileTrue: [
		(printer interpretNextInstructionUsing: scanner) ifTrue: [^true].
	].
	^false! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 8/20/2016 22:17:08'!
getSourceFor: selector in: class
	"Retrieve or reconstruct the source code for this method."
	| flagByte source |
	flagByte := self last.
	flagByte < 252 ifTrue:
		["No source pointer -- decompile without temp names"
"		^ (class decompilerClass new decompile: selector in: class method: self)"
	"See comment at #useOldCompiler"
		^ (Decompiler new decompile: selector in: class method: self)
			decompileString].

	"Situation normal; read the sourceCode from the file"
	source := [self getSourceFromFile]
				on: Error
		"An error can happen here if, for example, the changes file has been truncated by an aborted download.  The present solution is to ignore the error and fall back on the decompiler.  A more thorough solution should probably trigger a systematic invalidation of all source pointers past the end of the changes file.  Consider that, as time goes on, the changes file will eventually grow large enough to cover the lost code, and then instead of falling into this error case, random source code will get returned."
				do: [ :ex | ex return: nil].
		
	^source
		ifNil: [
			"Source code not available. Decompile blind (no temps)"
"			 (class decompilerClass new decompile: selector in: class method: self)"
	"See comment at #useOldCompiler"
			 (Decompiler new decompile: selector in: class method: self)
				decompileString]! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 8/20/2016 22:14:15'!
sourceClass
	"Get my receiver class (method class) from the preamble of my source.  Return nil if not found."

	^ [(Compiler evaluate: (self sourceFileStream backChunk "blank"; backChunk "preamble")) theClass] on: Error do: nil! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 8/20/2016 22:14:05'!
compilerClass
	^self methodClass 
		ifNil: [Compiler] 
		ifNotNil: [:class | class compilerClass].! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 8/20/2016 22:16:58'!
decompile
	"Return the decompiled parse tree that represents self"

	|  class selector |
	class := self methodClass ifNil: [Object].
	selector := self selector ifNil: [self defaultSelector].
"	^class decompilerClass new decompile: selector in: class method: self."
	"See comment at #useOldCompiler"
	^Decompiler new decompile: selector in: class method: self.! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 8/20/2016 22:19:23'!
methodNode
	"Return the parse tree that represents self"
	| aClass source |
	aClass := self methodClass.
	source := self
				getSourceFor: (self selector ifNil: [self defaultSelector])
				in: aClass.
"	^(aClass parserClass new"
	"See comment at #useOldCompiler"
	^(Parser new
		encoderClass: (self isBlueBookCompiled
						ifTrue: [EncoderForV3]
						ifFalse: [EncoderForV3PlusClosures]);
		parse: source class: aClass)
			sourceText: source;
			yourself! !

!CompiledMethod methodsFor: 'decompiling' stamp: 'jmv 8/20/2016 22:14:09'!
parserClass
	^self methodClass 
		ifNil: [Compiler parserClass] 
		ifNotNil: [:class | class parserClass].! !

!CompiledMethod methodsFor: 'debugger support' stamp: 'jmv 8/20/2016 22:17:59'!
abstractPCForConcretePC: concretePC
	"Answer the abstractPC matching concretePC."

	| abstractPC scanner client |
	self flag: 'belongs in DebuggerMethodMap?'.
	abstractPC := 1.
	scanner := InstructionStream on: self.
	client := InstructionClient new.
	[(scanner atEnd
	  or: [scanner pc >= concretePC]) ifTrue:
		[^abstractPC].
	 abstractPC := abstractPC + 1.
	 scanner interpretNextInstructionFor: client.
	 true] whileTrue! !

!CompiledMethod methodsFor: 'debugger support' stamp: 'jmv 8/20/2016 22:12:02'!
blockExtentsInto: aDictionary from: initialPC to: endPC scanner: scanner numberer: numbererBlock
	"Support routine for startpcsToBlockExtents"
	| extentStart blockSizeOrLocator |
	self flag: 'belongs in DebuggerMethodMap'.
	extentStart := numbererBlock value.
	[scanner pc <= endPC] whileTrue:
		[blockSizeOrLocator := scanner interpretNextInstructionFor: BlockStartLocator new.
		 blockSizeOrLocator isInteger ifTrue:
			[self
				blockExtentsInto: aDictionary
				from: scanner pc
				to: scanner pc + blockSizeOrLocator - 1
				scanner: scanner
				numberer: numbererBlock]].
	aDictionary at: initialPC put: (extentStart to: numbererBlock value).
	^aDictionary! !

!CompiledMethod methodsFor: 'debugger support' stamp: 'jmv 8/20/2016 22:18:04'!
pcPreviousTo: pc
	| scanner client prevPc |
	self flag: 'belongs in DebuggerMethodMap?'.
	pc > self endPC ifTrue: [^self endPC].
	scanner := InstructionStream on: self.
	client := InstructionClient new.
	[scanner pc < pc] whileTrue:
		[prevPc := scanner pc.
		 scanner interpretNextInstructionFor: client].
	^prevPc! !

!CompiledMethod methodsFor: 'closures' stamp: 'jmv 8/20/2016 22:12:31'!
embeddedBlockClosures
	"
	(CompiledMethod >> #embeddedBlockClosures)embeddedBlockClosures
	"
	| bms extractor scanner |
	bms := OrderedCollection new.
	scanner := self scanner.
	extractor := ClosureExtractor withAction: [ :c | bms add: c ] andScanner: scanner.
	[ scanner pc <= self endPC ] whileTrue: [ scanner interpretNextInstructionFor: extractor ].
	^ bms! !

!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 8/20/2016 22:21:49'!
timeStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	"(CompiledMethod compiledMethodAt: #timeStamp) timeStamp"

	| file preamble stamp tokens tokenCount |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file _ SourceFiles at: self fileIndex.
	file ifNil: [^ String new].  "sources file not available"
	"file does not exist happens in secure mode"
	file _ [file name asFileEntry readStream] on: FileDoesNotExistException do: [ :ex| nil ].
	file ifNil: [^ String new].
	preamble _ self getPreambleFrom: file at: (0 max: self filePosition).
	stamp _ String new.
	tokens _ (preamble findString: 'methodsFor:' startingAt: 1) > 0
		ifTrue: [Scanner new scanTokens: preamble]
		ifFalse: [Array new  "ie cant be back ref"].
	(((tokenCount _ tokens size) between: 7 and: 8) and: [(tokens at: tokenCount - 5) = #methodsFor:])
		ifTrue:
			[(tokens at: tokenCount - 3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp _ tokens at: tokenCount - 2]].
	((tokenCount between: 5 and: 6) and: [(tokens at: tokenCount - 3) = #methodsFor:])
		ifTrue:
			[(tokens at: tokenCount  - 1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
					stamp _ tokens at: tokenCount]].
	file close.
	^ stamp! !

!CompiledMethod methodsFor: 'inspecting' stamp: 'jmv 8/20/2016 22:21:44'!
explorerContents
	"(CompiledMethod compiledMethodAt: #explorerContents) explore"
	
	^Array streamContents:
		[:s| | tokens |
		tokens := Scanner new scanTokens: (self headerDescription readStream skipTo: $"; upTo: $").
		s nextPut: (ObjectExplorerWrapper
						with: ((0 to: tokens size by: 2) collect:
								[:i| i = 0 ifTrue: [self header] ifFalse: [{tokens at: i - 1. tokens at: i}]])
						name: 'header'
						model: self).
		(1 to: self numLiterals) do:
			[:key|
			s nextPut: (ObjectExplorerWrapper
							with: (self literalAt: key)
							name: ('literal', key printString contractTo: 32)
							model: self)].
		self isQuick
			ifTrue: [s nextPut: (ObjectExplorerWrapper
									with: self symbolic
									name: #symbolic
									model: self)]
			ifFalse:
				[self symbolicLinesDo:
					[:pc :line|
					pc <= 1
						ifTrue:
							[s nextPut: (ObjectExplorerWrapper
											with: line
											name: 'pragma'
											model: self)]
						ifFalse:
							[s nextPut: (ObjectExplorerWrapper
											with: line
											name: pc printString
											model: self)]]].
				"should be self numLiterals + 1 * Smalltalk wordSize + 1"
		self endPC + 1
			to: self basicSize
			do: [:key|
				s nextPut: (ObjectExplorerWrapper
								with: (self basicAt: key)
								name: key printString
								model: self)]]! !


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/20/2016 22:18:41'!
browseEqEqSentToSmallIntegerConstants
	"
	Smalltalk browseEqEqSentToSmallIntegerConstants
	"
	| hasMatch visitor |
	hasMatch _ false.
	visitor _ ParseNodeEnumerator
		ofBlock: [ :node |
			(node isMessageNode and: [
				(#(#== #~~ ) identityIncludes: node selector key) and: [
					node receiver isConstantNumber or: [ node arguments first isConstantNumber ]]]) ifTrue: [
						hasMatch _ true ]]
		select: [ :node |
			hasMatch not ].
	Smalltalk browseAllSelect: [ :method |
		hasMatch _ false.
		method decompile accept: visitor.
		hasMatch ].! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/20/2016 22:17:17'!
macroBenchmark1    "Smalltalk macroBenchmark1"
	"Decompiles and prettyPrints the source for every method in the system (or less depending on the *FILTER*, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same.  Because it never installs the new method, it should not cause any flusing of the method cache."
	 | methodNode oldMethod newMethod badOnes oldCodeString n classes |
	classes _ Smalltalk allClasses select: [:c | c name < 'B3'].
	badOnes _ OrderedCollection new.
'Decompiling and recompiling...'
displayProgressAt: Sensor mousePoint
from: 0 to: (classes detectSum: [:c | c selectors size])
during: [:bar | n _ 0.
	classes do:
		[:cls | 
		"Transcript cr; show: cls name."
		cls selectors do:
			[:selector | bar value: (n _ n+1).
			oldMethod _ cls compiledMethodAt: selector.
"			oldCodeString _ (cls decompilerClass new"
	"See comment at #useOldCompiler"
			oldCodeString _ (Decompiler new
								decompile: selector in: cls method: oldMethod)
							decompileString.
			methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls notifying: nil ifFail: nil.
			newMethod _ methodNode generate: #(0 0 0 0).
"			oldCodeString = (cls decompilerClass new"
	"See comment at #useOldCompiler"
			oldCodeString = (Decompiler new
								decompile: selector in: cls method: newMethod)
							decompileString ifFalse: [badOnes add: cls name , ' ' , selector]]].
].
	^ badOnes size! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 8/20/2016 22:17:27'!
testDecompiler
	"
	Smalltalk testDecompiler
	"
	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."
	| methodNode oldMethod newMethod badOnes oldCodeString n |
	badOnes _ OrderedCollection new.
	'Decompiling all classes...'
		displayProgressAt: Sensor mousePoint
		from: 0
		to: CompiledMethod instanceCount
		during: [ :bar |
			n _ 0.
			Smalltalk allBehaviorsDo: [ :cls |
				"Transcript cr; show: cls name."
				cls selectors do: [ :selector |
					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].
					oldMethod _ cls compiledMethodAt: selector.
"					oldCodeString _ (cls decompilerClass new"
	"See comment at #useOldCompiler"
					oldCodeString _ (Decompiler new
						decompile: selector
						in: cls
						method: oldMethod) decompileString.
					methodNode _ cls compilerClass new
						compile: oldCodeString
						in: cls
						notifying: nil
						ifFail: nil.
					newMethod _ methodNode generate: #(0 0 0 0 ).
					oldCodeString =
"						(cls decompilerClass new"
	"See comment at #useOldCompiler"
						(Decompiler new
							decompile: selector
							in: cls
							method: newMethod) decompileString ifFalse: [
						Transcript
							 newLine;
							 show: '***' , cls name , ' ' , selector.
						badOnes add: (MethodReference class: cls selector: selector) ]]]].
	Smalltalk
		browseMessageList: badOnes asArray sort
		name: 'Decompiler Discrepancies'.! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 8/20/2016 22:22:14'!
printStuffToCleanOnImageSave
	"So far, to serve experiments.
	Some day, when this is empty, we'll be closer to bootstrap from sources."
	| n nonNilVars m classInstVars v|
	n _ 0.
	""
	'--------' print.
	'Proceeses: ' print.
	((Process allSubInstances reject: [:each | each isTerminated ])
		sort: [ :a :b | a priority >= b priority ]) do: [ :p | p print ].
	'======' print.
	'Globals: ' print.
	Smalltalk globals associationsDo: [ :g | " {g key. g value class }" g  print ].
	'======' print.
	'ClassVars (notNil):  ' print.
	n _ 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		"Cleared, but come again for lazy init during this report.
		therefore, not required for bootstrap from sources"
		({ Scanner } includes: cls) ifFalse: [
			nonNilVars _ cls classPool select: [ :var | var notNil ].
			nonNilVars notEmpty ifTrue: [
				n _ n + 1.
				{ cls. nonNilVars keys} print ]]].
	'======' print.
	'InstClassVars (notNil): ' print.
	m _ 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		classInstVars _ Dictionary new.
		cls class allRegularInstVarNames do: [ :nam |
			v _ cls instVarNamed: nam.
			v ifNotNil: [
				classInstVars at: nam put: v ]].
		classInstVars notEmpty ifTrue: [
			m _ m + 1.
			{ cls class. classInstVars keys }  print ]].
	'======' print.
	
	{ n. m}  print! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 8/20/2016 22:12:14'!
eliotsClosureMeasurements
	"
	Smalltalk eliotsClosureMeasurements
	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	"
      | numMethods numMethodsWithClosure numMethodsWithIndirectTemps
         numClosures numClosuresWithCopiedValues numCopiedValuesForClosure
         numRemoteTemps numScopesWithRemoteTemps
         methodReturnsInClosure closureUsesSelfs methodReturnAndUsesSelfs numClean |

        numMethods := numMethodsWithClosure := numMethodsWithIndirectTemps :=
        numClosures := numClosuresWithCopiedValues := numCopiedValuesForClosure :=
        numRemoteTemps := numScopesWithRemoteTemps :=
        methodReturnsInClosure := closureUsesSelfs := methodReturnAndUsesSelfs := numClean := 0.
        self allSelect:
                 [:m| | s hasClosure hasIndirectTemps blkPc blkSz doesMethodReturn usesSelf hasCopied sel|
			sel _ false.
                 hasClosure := hasIndirectTemps := false.
                 s := InstructionStream on: m.
                 s scanFor:
                          [:b|
                          b = 143 "closure creation" ifTrue:
                                   [hasClosure := true.
                                   numClosures := numClosures + 1.
                                   s followingByte >= 16 ifTrue:
                                            [numClosuresWithCopiedValues := numClosuresWithCopiedValues + 1.
                                             numCopiedValuesForClosure := numCopiedValuesForClosure + (s followingByte >> 4)]].
                          (b = 138 "indirect temp vector creation"
                           and: [s followingByte <= 127]) ifTrue:
                                   [hasIndirectTemps := true.
                                    numScopesWithRemoteTemps := numScopesWithRemoteTemps + 1.
                                    numRemoteTemps := numRemoteTemps + s followingByte].
                          false].
                 numMethods := numMethods + 1.
                 hasClosure ifTrue:
                          [numMethodsWithClosure := numMethodsWithClosure + 1.
                           s pc: m initialPC; scanFor: [:b| b = 143].

"jmv-This looks like the correct place to do this"
                           hasCopied := s followingByte >= 16.

                           blkSz := s interpretNextInstructionFor: BlockStartLocator new.
                           blkPc := s pc.
                           doesMethodReturn := usesSelf := false.

"jmv-Doing this here looks like a bug. See the other comment"
                           hasCopied := s followingByte >= 16.

"jmv-Another bug. This only considers the first closure (and any nested closure in it), but not later ones"

                           s scanFor:
                                   [:b|
                                   s pc >= (blkPc + blkSz)
                                            ifTrue: [true]
                                            ifFalse:
                                                     [doesMethodReturn := doesMethodReturn or: [s willReturn and: [s willBlockReturn not]].
                                                      usesSelf := usesSelf or: [b = 112 "pushSelf"
                                                                                                  or: [b < 16 "pushInstVar"
                                                                                                  or: [(b = 128 and: [s followingByte <= 63]) "pushInstVar"
                                                                                                  or: [(b between: 96 and: 96 + 7) "storePopInstVar"
                                                                                                  or: [(b = 130 and: [s followingByte <= 63]) "storePopInstVar"
                                                                                                  or: [(b = 129 and: [s followingByte <= 63]) "storeInstVar"
                                                                                                  or: [b = 132 and: [s followingByte = 160]]]]]]]].
                                                     false]].
                           doesMethodReturn ifTrue:
                                   [methodReturnsInClosure := methodReturnsInClosure + 1].
                           usesSelf ifTrue:
                                   [closureUsesSelfs := closureUsesSelfs + 1].
                           (doesMethodReturn and: [usesSelf]) ifTrue:
                                   [methodReturnAndUsesSelfs := methodReturnAndUsesSelfs + 1].
                           (doesMethodReturn or: [usesSelf or: [hasCopied]]) ifFalse:
                                   [numClean := numClean + 1]].
                 hasIndirectTemps ifTrue: [numMethodsWithIndirectTemps := numMethodsWithIndirectTemps + 1].
                 sel].
^        { {'Methods'. numMethods}. {'MethodsWithClosure'. numMethodsWithClosure}. {'MethodsWithIndirectTemps'. numMethodsWithIndirectTemps}.
         {'Closures'. numClosures}. {'CopiedValuesForClosures'. numCopiedValuesForClosure}. {'ClosuresWithCopiedValues'. numClosuresWithCopiedValues}.
         {'RemoteTemps'. numRemoteTemps}. {'ScopesWithRemoteTemps'. numScopesWithRemoteTemps}.
         {'MethodsWithMethodReturnsInClosures'. methodReturnsInClosure}. {'MethodsWithReferencesToSelfInClosures'. closureUsesSelfs}. {'Both'. methodReturnAndUsesSelfs}.
         {'MethodsWithOnlyCleanClosures'. numClean} }! !

!SystemDictionary methodsFor: 'Closure measurements' stamp: 'jmv 8/20/2016 22:12:20'!
eliotsClosureMeasurementsOn: m over: aFiveArgBlock
	"
	See senders.
	Or try something like:
		Smalltalk
			eliotsClosureMeasurementsOn: FileList >> #defaultContents
			over: [ :closuresCount :hasCopiedValuesForClosure :hasIndirectTemps :anyClosureHasCopied :anyClosureDoesMethodReturn :anyClosureUsesSelf |
				(Array with: closuresCount with: hasCopiedValuesForClosure with: hasIndirectTemps with: anyClosureHasCopied with: anyClosureDoesMethodReturn with: anyClosureUsesSelf)]

	From http://www.mirandabanda.org/cogblog/2008/11/14/mechanised-modifications-and-miscellaneous-measurements/
	by Eliot Miranda
	
	Note: This could perhaps be refactored to use the newer #embeddedBlockClosures and testing methods on the closures themselves.
	"
	| s nextScanStart thisClosureHasCopied closuresCount hasIndirectTemps blkPc blkSz anyClosureHasCopied anyClosureDoesMethodReturn anyClosureUsesSelf analyzedClosures |
	closuresCount := 0.
	hasIndirectTemps := false.
	anyClosureHasCopied :=  anyClosureDoesMethodReturn := anyClosureUsesSelf := false.
	s := InstructionStream on: m.
	s scanFor: [ :b |
		b = 16r8F "16r8F = 143 closure creation" ifTrue: [
			closuresCount := closuresCount + 1].
		(b = 16r8A "16r8A = 138indirect temp vector creation" and: [ s followingByte <= 127]) ifTrue: [
				hasIndirectTemps := true].
		false].
	nextScanStart := m initialPC.
	analyzedClosures := 0.
	[ analyzedClosures < closuresCount ] whileTrue: [
		s pc: nextScanStart; scanFor: [ :b | b = 16r8F ].	"16r8F = 143 Search for first closure"
		analyzedClosures := analyzedClosures + 1.
		thisClosureHasCopied := s followingByte >= 16r10.
		anyClosureHasCopied := anyClosureHasCopied | thisClosureHasCopied.
		blkSz := s interpretNextInstructionFor: BlockStartLocator new.		"Findout size of first closure"
		blkPc := s pc.
		s scanFor: [ :b |
			s pc >= (blkPc + blkSz)
				ifTrue: [
					nextScanStart := s pc.
					true]
				ifFalse: [
					b = 16r8F ifTrue: [			
						thisClosureHasCopied := s followingByte >= 16r10.
						anyClosureHasCopied := anyClosureHasCopied | thisClosureHasCopied.
						analyzedClosures := analyzedClosures + 1 ].
					anyClosureDoesMethodReturn := anyClosureDoesMethodReturn or: [s willReturn and: [s willBlockReturn not]].
					anyClosureUsesSelf := anyClosureUsesSelf or: [b = 16r70 "pushSelf"
										or: [b < 16r10 "pushInstVar"
										or: [(b = 16r80 and: [s followingByte <= 16r3F]) "pushInstVar"
										or: [(b between: 16r60 and: 16r60 + 7) "storePopInstVar"
										or: [(b = 16r82 and: [s followingByte <= 63]) "storePopInstVar"
										or: [(b = 16r81 and: [s followingByte <= 63]) "storeInstVar"
										or: [b = 16r84 and: [s followingByte = 160]]]]]]]].
					false]]].
	^aFiveArgBlock valueWithArguments: (Array
			with: closuresCount
			with: hasIndirectTemps
			with: anyClosureHasCopied
			with: anyClosureDoesMethodReturn
			with: anyClosureUsesSelf)! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 8/20/2016 22:15:58'!
processCommandLineArgument: rawArgStream storeStartUpScriptArgsOn: startUpScriptArgs
	"
	Smalltalk processCommandLineArguments
	
	A possible example (duplicated single quotes: '' should be double quotes, but not allowed in a Smalltalk comment):
	Squeak.exe Cuis4.2-2211x.image -r RequiredFeature1 -rRequiredFeature2 -d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine'' -s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3''
	"
	| p data entry |
	p _ rawArgStream next.

	(p first = $- and: [ p size > 1 ]) ifTrue: [
		"If the command is not included in p, it is next argument"
		p size = 2
			ifTrue: [
				"as in 		-r RequiredFeature1"
				data _ rawArgStream next ]
			ifFalse: [
				"as in 		-rRequiredFeature2"
				data _ p copyFrom: 3 to: p size ].
		p second caseOf: {
			[ $r ] -> [		"as in 		-rRequiredFeature2"
				{ 'Feature require: '. data } print.
				[ Feature require: data ] on: Error do: [] ].
			[ $d ] -> [		"as in 		-d ''Transcript show: 'popo1'; newLine'' -d''Transcript show: 'popo2'; newLine''        (duplicated singleQuotes should read doubleQuote)"
				{ 'Compiler evaluate: '. data } print.
				[ Compiler evaluate: data ] on: Error do: [] ].
			[ $s ] -> [		"as in 		-s smalltalkScript.st paramAlScript1 paramAlSCript2 ''parametro al script ->>>--// 3'' 			(duplicated singleQuotes should read doubleQuote)"
				[ rawArgStream atEnd ] whileFalse: [
					startUpScriptArgs nextPut: rawArgStream next ].
				"Can use 'Smalltalk startUpScriptArguments' inside the startUp script
				{ 'Compiler evaluate contents of file named: '. data. ' arguments: '. Smalltalk startUpScriptArguments } print."
				entry _ data asFileEntry.
				entry exists ifTrue: [
					entry readStreamDo: [ :stream |
						[ Compiler evaluate: stream contentsOfEntireFile ] on: Error do: []]].
				"Maybe we decide to clear them after startup script execution
				startUpScriptArguments _ nil" ]
		}
		otherwise: []
	]! !


!ContentPack methodsFor: 'importing' stamp: 'jmv 8/20/2016 22:14:22'!
decodeContentFrom: associationList

	| associations | 
	
	associations _ associationList collect: [ :assoc |
		(assoc at: 2) class = Array
			ifTrue: [ (assoc at: 1) -> (self decodeContentFrom: (assoc at: 2)) ]
			ifFalse: [ 
				(assoc at: 1) -> (Compiler 
					evaluate: (self 
						perform: 
							('object' , (assoc at: 2) 
								asString) asSymbol) base64Decoded)
			]
	].

	^ associations as: Dictionary! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 8/20/2016 22:14:57'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Answer the result of compilation.  Put up a progress report with
     the given announcement as the title."

	| val chunk |
	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :bar | 
			[ self atEnd ] whileFalse: [
					bar value: self position.
					self skipSeparators.
					
					[
						val := (self peekFor: $!!) 
								ifTrue: [
									chunk := self nextChunk.
									"These are the ones that should do nothing, 
									because next line is a doit that does the stuff
									(or because it is handled elsewhere)"
									(chunk beginsWith: 'description:  ')
									| (chunk beginsWith: 'provides: ')
									| (chunk beginsWith: 'requires: ')
									| (chunk beginsWith: 'classDefinition: ')
									| (chunk beginsWith: 'classRemoval: ')
									| (chunk beginsWith: 'methodRemoval: ')
									| (chunk beginsWith: 'classMoveToSomePackage: ')
									| (chunk beginsWith: 'methodMoveToSomePackage: ')
										ifFalse: [(Compiler evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									[ Compiler evaluate: chunk logged: true ]
										on: Error
										do: [ :ex |
											ex print.
											('while evaluating: ', chunk) print.
											ex pass ]
										]] 
							on: InMidstOfFileinNotification
							do: [ :ex | ex resume: true ] ].
			].
	"Note:  The main purpose of this banner is to flush the changes file."
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !


!Transcripter methodsFor: 'command line' stamp: 'jmv 8/20/2016 22:16:07'!
readEvalPrint
	| line |
	[ #('quit' 'exit' 'done' ) includes: (line _ self request: '>') ] whileFalse: [
		self
			newLine;
			show:
				([ Compiler evaluate: line ] ifError: [ :err :ex |
					err ]) ]! !


!MethodNode methodsFor: 'source mapping' stamp: 'jmv 8/20/2016 22:19:38'!
rawSourceRangesAndMethodDo: aBinaryBlock
	"Evaluate aBinaryBlock with the rawSourceRanges and method generated from the receiver."

	| methNode method |
"	methNode := encoder classEncoding parserClass new"
	"See comment at #useOldCompiler"
	methNode := Parser new
					encoderClass: encoder class;
					parse: (sourceText "If no source, use decompile string as source to map from"
							ifNil: [self decompileString]
							ifNotNil: [sourceText])
					class: self methodClass.
	method := methNode generate.  "set bytecodes to map to"
	^aBinaryBlock
		value: methNode encoder rawSourceRanges
		value: method! !

!MethodNode methodsFor: 'debugger support' stamp: 'jmv 8/20/2016 22:19:33'!
blockExtentsToTempsMap
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."

	^encoder blockExtentsToTempsMap ifNil:
		[| methNode |
"		methNode := encoder classEncoding parserClass new"
	"See comment at #useOldCompiler"
		methNode := Parser new
						encoderClass: encoder class;
						parse: (sourceText ifNil: [self decompileString])
						class: self methodClass.
		"As a side effect generate: creates data needed for the map."
		methNode generate.
		methNode encoder blockExtentsToTempsMap]! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 8/20/2016 22:15:41'!
compileSelectionFor: anObject in: evalContext
	| methodNode |
	methodNode _ [
	Compiler new
		sourceStreamGetter: #selectionAsStream;		"Cuis specific. Do not remove!!"
		
		compileNoPattern: self selectionAsStream
		in: anObject class
		context: evalContext
		notifying: self
		ifFail: [ ^ nil ]]
		on: OutOfScopeNotification
		do: [ :ex |
			ex resume: true ].
	^ methodNode generate! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 8/20/2016 22:15:48'!
evaluateSelectionAndDo: aBlock ifFail: failBlock
	"Treat the current selection as an expression; evaluate it and return the result
	3 +4
	"
	| provider result rcvr ctxt |
	self lineSelectAndEmptyCheck: [^ ''].

	provider _ self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
				rcvr _ provider doItReceiver.
				ctxt _ provider doItContext]
		ifFalse: [rcvr _ ctxt _ nil].
	result _ [
		Compiler new
			sourceStreamGetter: #selectionAsStream;		"Cuis specific. Do not remove!!"
			evaluate: self selectionAsStream
			in: ctxt
			to: rcvr
			notifying: self
			ifFail: [^ failBlock value]
			logged: true.
	] 
		on: OutOfScopeNotification 
		do: [ :ex | ex resume: true].
	morph formatAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
	^ aBlock value: result! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 8/20/2016 22:19:48'!
explainTemp: string 
	"Is string the name of a temporary variable (method or block argument or temporary)?"

	| provider selectedClass methodNode tempNode |
	provider _ self codeProvider.
	(provider respondsTo: #selectedMessageName) ifFalse: [^ nil].
	provider selectedMessageName ifNil: [^nil].	"not in a method"
	selectedClass := provider selectedClassOrMetaClass.
"	methodNode := selectedClass parserClass new parse: provider selectedMessage class: selectedClass."
	"See comment at #useOldCompiler"
	methodNode := Parser new parse: provider selectedMessage class: selectedClass.
	tempNode := methodNode encoder tempNodes detect: [ :n | n name = string ] ifNone: [^nil].
	^(tempNode isArg
		ifTrue: [string, ' is an argument to this ']
		ifFalse: [string, ' is a temporary variable in this ']),
	   (tempNode isDeclaredAtMethodLevel
		ifTrue: ['method ']
		ifFalse: ['block '])! !


!TextDoIt methodsFor: 'as yet unclassified' stamp: 'jmv 8/20/2016 22:16:03'!
actOnClickFor: anObject
	"Note: evalString gets evaluated IN THE CONTEXT OF anObject
	 -- meaning that self and all instVars are accessible"
	Compiler evaluate: evalString for: anObject logged: false.
	^ true ! !


!RemoteString methodsFor: 'accessing' stamp: 'jmv 8/20/2016 22:15:19'!
compilerClass
	"Answer a compiler class appropriate for source methods of this class."

	^Compiler! !


!CodePackage methodsFor: 'testing' stamp: 'jmv 8/20/2016 22:21:34'!
changeRecordForOverriddenMethod: aMethodReference
	| sourceFilesCopy method position |
	method := aMethodReference actualClass compiledMethodAt: aMethodReference methodSymbol.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x isNil ifTrue: [ nil ]
				ifFalse: [x name asFileEntry readStream]].
	[ | file prevPos prevFileIndex chunk stamp methodCategory tokens |
	method fileIndex = 0 ifTrue: [^ nil].
	file := sourceFilesCopy at: method fileIndex.
	[position notNil & file notNil] whileTrue: [
		file position: (0 max: position-150).  "Skip back to before the preamble"
		[file position < (position-1)]  "then pick it up from the front"
			whileTrue: [ chunk _ file nextChunk ].

		"Preamble is likely a linked method preamble, if we're in
			a changes file (not the sources file).  Try to parse it
			for prior source position and file index"
		prevPos := nil.
		stamp := ''.
		(chunk findString: 'methodsFor:' startingAt: 1) > 0
			ifTrue: [tokens := Scanner new scanTokens: chunk]
			ifFalse: [tokens := #()  "ie cant be back ref"].
		((tokens size between: 7 and: 8)
			and: [(tokens at: tokens size-5) = #methodsFor:])
			ifTrue:
				[(tokens at: tokens size-3) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp := tokens at: tokens size-2.
						prevPos := tokens last.
						prevFileIndex := sourceFilesCopy fileIndexFromSourcePointer: prevPos.
						prevPos := sourceFilesCopy filePositionFromSourcePointer: prevPos]
				ifFalse: ["Old format gives no stamp; prior pointer in two parts"
						prevPos := tokens at: tokens size-2.
						prevFileIndex := tokens last].
				(prevPos = 0 or: [prevFileIndex = 0]) ifTrue: [prevPos := nil]].
		((tokens size between: 5 and: 6)
			and: [(tokens at: tokens size-3) = #methodsFor:])
			ifTrue:
				[(tokens at: tokens size-1) = #stamp:
				ifTrue: ["New format gives change stamp and unified prior pointer"
						stamp := tokens at: tokens size]].
		methodCategory := (tokens after: #methodsFor:) ifNil: ['as yet unclassifed'].
		(self includesMethodCategory: methodCategory ofClass: aMethodReference actualClass) ifTrue:
			[methodCategory = (Smalltalk at: #Categorizer ifAbsent: [Smalltalk at: #ClassOrganizer]) default ifTrue: [methodCategory := methodCategory, ' '].
			^ ChangeRecord new file: file position: position type: #method
						class: aMethodReference classSymbol category: methodCategory meta: aMethodReference classIsMeta stamp: stamp].
		position := prevPos.
		prevPos notNil ifTrue: [
			file _ sourceFilesCopy at: prevFileIndex]].
		^ nil]
			ensure: [sourceFilesCopy do: [:x | x notNil ifTrue: [x close]]]
	! !


!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 8/20/2016 22:22:18'!
isLegalInstVarName: aString
	"Answer whether aString is a legal instance variable name."

	^ ((Scanner isLiteralSymbol: aString) and: [(aString includes: $:) not]) and:
		[(self inviolateInstanceVariableNames includes:  aString) not]! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 8/20/2016 22:14:33'!
addEntry

	| newKey string |
	string _ FillInTheBlankMorph request:
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'.
	newKey _ Compiler evaluate: string.
	model addEntry: newKey! !

!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 8/20/2016 22:14:38'!
renameEntry
	| string newKey |

	string _ FillInTheBlankMorph request: 
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'
		 initialAnswer: model selectedKey printString.

	string = '' ifTrue: [
		^self ].

	newKey _ Compiler evaluate: string.

	model renameEntryTo: newKey! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 8/20/2016 22:13:50'!
compilerClass
	^ (Smalltalk at: class ifAbsent: [^ Compiler]) compilerClass! !

!ChangeRecord methodsFor: 'access' stamp: 'jmv 8/20/2016 22:19:13'!
methodSelector
	^type == #method ifTrue:
"		[(Smalltalk at: class ifAbsent: [Object]) parserClass new parseSelector: self string]"
	"See comment at #useOldCompiler"
		[Parser new parseSelector: self string]! !

!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 8/20/2016 22:13:59'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	| s |
	type == #method ifTrue: [
		self changeClass ifNotNil: [ :methodClass |
			methodClass
				compile: self text
				classified: category
				withStamp: stamp
				notifying: nil ]].
	type == #doIt ifTrue: [
		((s _ self string) beginsWith: '----') ifFalse: [ Compiler evaluate: s ]].
	type == #classDefinition ifTrue: [ Compiler evaluate: self string ].
	type == #classComment ifTrue: [
		(Smalltalk at: class asSymbol)
			comment: self text
			stamp: stamp ]! !


!DebuggerMethodMap methodsFor: 'source mapping' stamp: 'jmv 8/20/2016 22:23:14'!
abstractSourceMap
	"Answer with a Dictionary of abstractPC <Integer> to sourceRange <Interval>."
	| theMethodToScan rawSourceRanges concreteSourceRanges abstractPC scanner client |
	abstractSourceRanges ifNotNil: [
		^abstractSourceRanges].
	"If the methodNode hasn't had a method generated it doesn't have pcs set in its
	 nodes so we must generate a new method and might as well use it for scanning."
	[
		methodNode rawSourceRangesAndMethodDo: [ :ranges :method |
			rawSourceRanges := ranges.
		 	theMethodToScan := method ]]
		on: UndeclaredVariableWarning
		do: [ :ex | ex resume ].
	concreteSourceRanges := Dictionary new.
	rawSourceRanges keysAndValuesDo: [ :node :range |
		node pc ~= 0 ifTrue: [
			concreteSourceRanges at: node pc put: range ]].
	abstractPC := 1.
	abstractSourceRanges := Dictionary new.
	scanner := InstructionStream on: theMethodToScan.
	client := InstructionClient new.
	[
		(concreteSourceRanges includesKey: scanner pc) ifTrue: [
			abstractSourceRanges at: abstractPC put: (concreteSourceRanges at: scanner pc)].
		 abstractPC := abstractPC + 1.
		 scanner interpretNextInstructionFor: client.
		 scanner atEnd ] whileFalse.
	^abstractSourceRanges! !


!DebuggerMethodMap class methodsFor: 'instance creation' stamp: 'jmv 8/20/2016 22:23:18'!
forMethod: aMethod "<CompiledMethod>"
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod.
	 Answer an existing instance from the cache if it exists, cacheing a new one if required."
	
	| n |
	^self protected: [ 
		MapCache
			at: aMethod
			ifAbsent: [
				[ n _ aMethod methodNode ] 
					on: UndeclaredVariableWarning
					do: [ :ex | ex resume ].
				self
						cacheDebugMap:
							(self
								forMethod: aMethod
								methodNode: n)
						forMethod: aMethod] ]! !


!CodeFile methodsFor: 'private' stamp: 'jmv 8/20/2016 22:21:08'!
classDefinition: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	tokens size = 11 ifFalse:[^doIts add: chgRec].
	theClass := self getClass: (tokens at: 3).
	theClass definition: string.
	classOrder add: theClass.! !

!CodeFile methodsFor: 'private' stamp: 'jmv 8/20/2016 22:21:14'!
metaClassDefinition: string with: chgRec
	| tokens theClass |
	tokens := Scanner new scanTokens: string.
	theClass := self getClass: (tokens at: 1).
	theClass metaClass definition: string.
	classOrder add: theClass metaClass.! !

!CodeFile methodsFor: 'private' stamp: 'jmv 8/20/2016 22:21:20'!
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

!CodeFile methodsFor: 'private' stamp: 'jmv 8/20/2016 22:21:24'!
possibleSystemSource: chgRec
	| tokens |
	sourceSystem isEmpty ifTrue:[
		tokens := Scanner new scanTokens: chgRec string.
		(tokens size = 1 and:[tokens first class == String]) ifTrue:[
			sourceSystem := tokens first.
			^self]].
	doIts add: chgRec.! !

!CodeFile methodsFor: 'private' stamp: 'jmv 8/20/2016 22:21:29'!
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


!CodePackageFile methodsFor: 'change record types' stamp: 'jmv 8/20/2016 22:21:40'!
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


!PseudoClass methodsFor: 'accessing' stamp: 'jmv 8/20/2016 22:15:03'!
compilerClass
	^ (Smalltalk at: name ifAbsent: [^ Compiler]) compilerClass! !

!PseudoClass methodsFor: 'private' stamp: 'jmv 8/20/2016 22:15:08'!
evaluate: aString
	^Compiler evaluate: aString for: nil logged: true! !

!PseudoClass methodsFor: 'private' stamp: 'jmv 8/20/2016 22:15:14'!
parserClass

	^ Compiler parserClass! !

!PseudoClass methodsFor: 'methods' stamp: 'jmv 8/20/2016 22:19:42'!
addMethodChange: aChangeRecord
	| selector |
"	selector _ self parserClass selectorFrom: aChangeRecord string."
	"See comment at #useOldCompiler"
	selector _ Parser selectorFrom: aChangeRecord string.
	self organization classify: selector under: aChangeRecord category.
	self sourceCodeAt: selector put: aChangeRecord! !


!SmalltalkCompleter methodsFor: 'entries' stamp: 'jmv 8/20/2016 22:15:35'!
computeEntries

	| allSource contextClass id p last3Ranges range prevRange receiverClass prevPrevRange |
	allSource _ model actualContents string.
	p _ (model is: #hasTextProvider)
		ifTrue: [ model textProvider ]
		ifFalse: [ model ].
	contextClass _ (p respondsTo: #selectedClassOrMetaClass) ifTrue: [
		p selectedClassOrMetaClass ].

	"Instead of creating a new string, maybe we could pass the last position to parse to Shout..."
	parser _ SHParserST80 new.
	parser
		workspace: ((model isMemberOf: Workspace) ifTrue: [ model ]);
		classOrMetaClass: contextClass;
		source: (allSource copyFrom: 1 to: position).
	parser parse.
	last3Ranges _ parser last3Ranges.
	range _ last3Ranges third.
	range ifNil: [ ^entries _ #() ].

	"If parsing breaks before position, then we don't know what to suggest, therefore don't open Completion"
	range end = position ifFalse: [ ^entries _ #() ].

	prefix _ allSource copyFrom: range start to: range end.
	
	(parser isMessage: range rangeType) ifTrue: [
		"If previous range is a constant or a well known identifier, we might filter messages"
		prevRange _ last3Ranges second.
		prevPrevRange _ last3Ranges first.
		receiverClass _ nil.
		"3 if -> ifNil: but not ifTrue:
		3=4 -> ifNil: or ifTrue:"
		(prevRange notNil and: [ prevPrevRange isNil or: [ (#(binary keyword) includes: prevPrevRange rangeType) not]]) ifTrue: [
			id _ (allSource copyFrom: prevRange start to: prevRange end).
			receiverClass _ prevRange rangeType caseOf: {
				[ #globalVar ] -> [ (Smalltalk at: id asSymbol) class ].
				[ #self ] -> [ contextClass ].
				[ #super ] -> [ contextClass superclass ].
				[ #true ] -> [ True ].
				[ #false ] -> [ False ].
				[ #nil ] -> [ UndefinedObject ].
				[ #character ] -> [ id first class ].
				[ #number ] -> [ (Compiler evaluate: id) class ].
				[ #string ] -> [ (Compiler evaluate: id) class ].
				[ #symbol ] -> [ (Compiler evaluate: id) class ].
				[ #stringSymbol ] -> [ (Compiler evaluate: id) class ].
				"thisContext could mean ContextPart or BlockClosure..."
				"[ #thisContext ] -> [ ContextPart ]"
			} otherwise: [ nil ]
		].
		^self computeMessageEntries: receiverClass ].

	(parser isPartialOrFullIdentifier: range rangeType) ifTrue: [
		^self computeIdentifierEntries ].
	
	"If we don't know what to do, do nothing"
	entries _ #()! !


!SHParserST80 methodsFor: 'character testing' stamp: 'jmv 8/20/2016 22:21:54'!
isSelectorCharacter: aCharacter
	"Actually meaning 'isBinarySelectorCharacter'. i.e. can aCharacter be part of a binary selector? "
	aCharacter isValidInIdentifiers ifTrue: [^false].
	aCharacter isSeparator ifTrue: [^false].
	"$- is specified here as NOT being a selector char, but it can appear as the 
	first char in a binary selector. That case is handled specially elsewhere"
	('"#$'':().;[]{}^_-'  includes: aCharacter) 
		ifTrue:[^false].
	aCharacter asciiValue = Scanner doItCharacterValue ifTrue: [^false "the doIt char"].
	aCharacter asciiValue = 0 ifTrue: [^false].
	"Any other char is ok as a binary selector char."
	^true
! !
