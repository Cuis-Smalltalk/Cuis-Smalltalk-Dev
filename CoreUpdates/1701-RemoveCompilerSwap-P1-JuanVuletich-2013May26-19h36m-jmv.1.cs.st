'From Cuis 4.1 of 12 December 2012 [latest update: #1700] on 26 May 2013 at 7:38:11.056031 pm'!

!Array methodsFor: 'converting' stamp: 'jmv 5/26/2013 15:36'!
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


!Behavior methodsFor: 'accessing' stamp: 'jmv 5/26/2013 15:40'!
compilerClass
	"Answer a compiler class appropriate for source methods of this class."

	^Compiler! !


!Browser methodsFor: 'class functions' stamp: 'jmv 5/26/2013 19:09'!
copyClass
	| originalClass originalName copysName |
	selectedClassName ifNil: [^ self].
	originalClass := self selectedClass.
	originalName := originalClass name.
	copysName _ self request: 'Please type new class name' initialAnswer: originalName.
	copysName = '' ifTrue: [^ self].  " Cancel returns '' "
	copysName _ copysName asSymbol.
	copysName = originalName ifTrue: [^ self].
	(Smalltalk includesKey: copysName)
		ifTrue: [^ self error: copysName , ' already exists'].
	Cursor wait 
		showWhile: [
			| newDefinition newMetaDefinition newClass |
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
				newClass comment: originalClass comment ] ].
	self classListIndex: 0.
	self changed: #classList! !

!Browser methodsFor: 'class functions' stamp: 'jmv 5/26/2013 15:38'!
defineClass: defString notifying: aController  
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aController."
	| oldClass class newClassName defTokens keywdIx envt |
	oldClass _ self selectedClassOrMetaClass.
	defTokens _ defString findTokens: Character separators.
	keywdIx _ defTokens findFirst: [:x | x beginsWith: 'category'].
	envt _ Smalltalk environmentForCategory: ((defTokens at: keywdIx+1) copyWithout: $').
	keywdIx _ defTokens findFirst: [:x | '*subclass*' match: x].
	newClassName _ (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [oldClass theNonMetaClass name asString ~= newClassName])
		and: [envt includesKeyOrAbove: newClassName asSymbol]) ifTrue:
			["Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: ((newClassName , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: newClassName size))
				ifFalse: [^ false]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil:[oldClass _ oldClass superclass].
	class _ Compiler
				evaluate: defString
				notifying: aController
				logged: true.
	(class isKindOf: Behavior)
		ifTrue: [self changed: #systemCategoryList.
				self changed: #classList.
				self changed: #clearUserEdits.
				self setClass: class selector: nil.
				"self clearUserEditFlag; editClass."
				^ true]
		ifFalse: [^ false]! !


!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 5/26/2013 19:09'!
fileIn
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	Cursor read showWhile: [ | s |
		type == #method ifTrue: [
			self changeClass ifNotNil: [ :methodClass |
				methodClass
					compile: self text
					classified: category
					withStamp: stamp
					notifying: nil.
				self methodSelector ifNotNil: [ :aSelector |
					Utilities
						noteMethodSubmission: aSelector
						forClass: methodClass ]]].
		type == #doIt ifTrue: [
			((s _ self string) beginsWith: '----') ifFalse: [ Compiler evaluate: s ]].
		type == #classDefinition ifTrue: [ Compiler evaluate: self string ].
		type == #classComment ifTrue: [ | cls |
			(cls _ Smalltalk at: class asSymbol)
				comment: self text
				stamp: stamp.
			Utilities
				noteMethodSubmission: #Comment
				forClass: cls ]].! !


!ClosureCompilerTest methodsFor: 'tests' stamp: 'jmv 5/26/2013 15:37'!
testBlockDoitDecompilation
	"Tests that decompile of a doit block with remote vars executes correcly"
	"Tests that decompilation of a Block, when 'method' of block is equivalent to that compiled by a DoIt, preserves the temp names "
	
	| blockSourceStream methodNode compiledMethod block decompiledBlock method |
	blockSourceStream := '|x y| [:a :b | x _ a. y _ b. x + y]' readStream.
	methodNode := Compiler new 
						from: blockSourceStream class: nil class context: nil notifying: nil;
						translate: blockSourceStream noPattern: true ifFail: [nil].
	method _ methodNode generate: #(0 0 0 0).
	compiledMethod _ method copyWithTempsFromMethodNode: methodNode.
	block := nil withArgs: #() executeMethod: compiledMethod.
	
	self shouldnt: [decompiledBlock := block decompile] raise: Error.
	self assert: 
'{[ :a :b |
	x _ a.
	y _ b.
	x + y. ]}' equals: decompiledBlock printString
! !


!CodeFileBrowser methodsFor: 'edit pane' stamp: 'jmv 5/26/2013 15:41'!
selectedMessage
	"Answer a copy of the source code for the selected message selector."

	| class selector answer |
	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	answer _ (class sourceCodeAt: selector).
	Preferences browseWithPrettyPrint ifTrue: [
		answer _ (Compiler new
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


!Inspector methodsFor: 'selecting' stamp: 'jmv 5/26/2013 15:37'!
accept: aString

	| result |
	result _ Compiler new
				evaluate: (ReadStream on: aString)
				in: self doItContext
				to: self doItReceiver
				notifying: nil	"fix this"
				ifFail:  [^ false].
	acceptedContentsCache _ result printString.
	self replaceSelectionValue: result.	"may put contents back"
	self acceptedContentsChanged.
	^ true! !


!InspectorWindow methodsFor: 'menu commands' stamp: 'jmv 5/26/2013 15:41'!
addEntry

	| newKey string |
	string _ FillInTheBlankMorph request:
'Enter new key, then type RETURN.
(Expression will be evaluated for value.)
Examples:  #Fred    ''a string''   3+4'.
	newKey _ Compiler evaluate: string.
	model addEntry: newKey! !


!MessageSet methodsFor: 'private' stamp: 'jmv 5/26/2013 15:39'!
contents: aString notifying: aController 
	"Compile the code in aString. Notify aController of any syntax errors. 
	Answer false if the compilation fails. Otherwise, if the compilation 
	created a new method, deselect the current selection. Then answer true."

	| category selector class oldSelector |
	self okayToAccept ifFalse: [^ false].
	self setClassAndSelectorIn: [:c :os | class _ c.  oldSelector _ os].
	class ifNil: [^ false].
	(oldSelector notNil and: [oldSelector first isUppercase]) ifTrue:
		[oldSelector == #Comment ifTrue:
			[class comment: aString stamp: Utilities changeStamp.
			self triggerEvent: #annotationChanged.
 			self changed: #clearUserEdits.
			^ false].
		oldSelector == #Definition ifTrue:
			["self defineClass: aString notifying: aController."
			Compiler
				evaluate: aString
				notifying: aController
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
				notifying: aController.
	selector
		ifNil: [^ false].
	selector == oldSelector ifFalse: [
		self reformulateListNoting: selector].
	self triggerEvent: #annotationChanged.
	^ true! !


!Object class methodsFor: 'instance creation' stamp: 'jmv 5/26/2013 15:41'!
readFrom: aStream
	"Create an object based on the contents of aStream."

	| object ok |
	ok _ (aStream isKindOf: Stream) or: [aStream isKindOf: String].
	(ok or: [aStream isKindOf: Text]) ifFalse: [^ self error: 'expected String or Text'].
	object _ Compiler evaluate: aStream.
	(object isKindOf: self) ifFalse: [self error: self name, ' expected'].
	^object! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 5/26/2013 15:42'!
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
									"These are the ones that should do nothing, because next line is a doit that does the stuff"
									(chunk beginsWith: 'classDefinition: ')
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
											ex resume: true ]
										]] 
							on: InMidstOfFileinNotification
							do: [ :ex | ex resume: true] ].
			self close ].
	"Note:  The main purpose of this banner is to flush the changes file."
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	^val! !


!PseudoClass methodsFor: 'accessing' stamp: 'jmv 5/26/2013 15:42'!
compilerClass
	^ (Smalltalk at: name ifAbsent: [^ Compiler]) compilerClass! !

!PseudoClass methodsFor: 'private' stamp: 'jmv 5/26/2013 15:42'!
evaluate: aString
	^Compiler evaluate: aString for: nil logged: true! !

!PseudoClass methodsFor: 'private' stamp: 'jmv 5/26/2013 15:42'!
parserClass

	^ Compiler parserClass! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 5/26/2013 15:37'!
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


!Text methodsFor: 'emphasis' stamp: 'jmv 5/26/2013 15:42'!
makeSelectorBold
	"For formatting Smalltalk source code, set the emphasis of that portion of 
	the receiver's string that parses as a message selector to be bold."

	| parser i |
	string size = 0 ifTrue: [^ self].
	i _ 0.
	[(string at: (i _ i + 1)) isSeparator] whileTrue.
	(string at: i) = $[ ifTrue: [^ self].  "block, no selector"
	(parser _ Compiler parserClass new) parseSelector: string.
	self makeBoldFrom: 1 to: (parser endOfLastToken min: string size)! !


!TextDoIt methodsFor: 'as yet unclassified' stamp: 'jmv 5/26/2013 15:43'!
actOnClickFor: anObject
	"Note: evalString gets evaluated IN THE CONTEXT OF anObject
	 -- meaning that self and all instVars are accessible"
	Compiler evaluate: evalString for: anObject logged: false.
	^ true ! !


!Transcripter methodsFor: 'command line' stamp: 'jmv 5/26/2013 15:43'!
readEvalPrint
	| line okToRevert |
	okToRevert _ true.
	[#('quit' 'exit' 'done' ) includes: (line _ self request: '>')] whileFalse: [
		line = 'revert' ifTrue: [
			okToRevert
				ifTrue: [
					Utilities revertLastMethodSubmission.
					self newLine; show: 'reverted: ' , Utilities mostRecentlySubmittedMessage.
					okToRevert _ false]
				ifFalse: [self newLine; show: 'Only one level of revert currently supported']]
		ifFalse: [self newLine; show: ([Compiler evaluate: line] ifError: [:err :ex | err])]]! !


!Utilities class methodsFor: 'common requests' stamp: 'jmv 5/26/2013 15:43'!
eval: aString
	"Evaluate the string in a neutral context, and under certain circumstances print the 
	result in the transcript"

	| result |
	result _ Compiler new evaluate: aString in: nil to: nil.
	(result isKindOf: Number) | (result isKindOf: String)
		ifTrue: [
			Transcript newLine; nextPutAll: result printString]! !

