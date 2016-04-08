'From Cuis 4.2 of 25 July 2013 [latest update: #2723] on 8 April 2016 at 12:42:14.652614 pm'!

!TextAttribute commentStamp: 'jmv 4/8/2016 12:24' prior: 0!
Tells a piece of text to be a certain way.
!


!String methodsFor: 'text conversion helpers' stamp: 'jmv 4/8/2016 10:06'!
bold
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  …' sub, '(1 / z)' ) edit
	"
	^self asText bold! !

!String methodsFor: 'text conversion helpers' stamp: 'jmv 4/8/2016 10:06'!
italic
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  …' sub, '(1 / z)' ) edit
	"
	^self asText italic! !

!String methodsFor: 'text conversion helpers' stamp: 'jmv 4/8/2016 10:06'!
sub
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  …' sub, '(1 / z)' ) edit
	"
	^self asText sub! !

!String methodsFor: 'text conversion helpers' stamp: 'jmv 4/8/2016 10:06'!
super
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  …' sub, '(1 / z)' ) edit
	"
	^self asText super! !

!String methodsFor: 'text conversion helpers' stamp: 'jmv 4/8/2016 10:06'!
under
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  …' sub, '(1 / z)' ) edit
	"
	^self asText under! !


!Text methodsFor: 'adding emphasis' stamp: 'jmv 4/8/2016 10:05'!
bold
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  …' sub, '(1 / z)' ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis bold from: 1 to: string size! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 4/8/2016 10:05'!
italic
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  …' sub, '(1 / z)' ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis italic from: 1 to: string size! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 4/8/2016 10:05'!
sub
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  …' sub, '(1 / z)' ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis subscript from: 1 to: string size! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 4/8/2016 10:05'!
super
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  …' sub, '(1 / z)' ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis superscript from: 1 to: string size! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 4/8/2016 10:05'!
under
	"Stuff like
	('X' italic, '2' super, ' + ', 'H' bold, 'ij' sub, ' + ', 'lim' italic under, 'z  …' sub, '(1 / z)' ) edit
	"
	string size = 0 ifTrue: [ ^self ].
	self addAttribute: TextEmphasis underlined from: 1 to: string size! !


!CodeProvider methodsFor: 'contents' stamp: 'jmv 4/8/2016 12:37'!
commentContents
	"documentation for the selected method"

	| poss aClass aSelector |
	^ (poss _ (aClass _ self selectedClassOrMetaClass)
						ifNil:
							['----']
						ifNotNil:
							[(aSelector _ self selectedMessageName)
								ifNil:
									['---']
								ifNotNil:
									[(aClass precodeCommentOrInheritedCommentFor: aSelector)", String crString, String crString, self timeStamp"
"which however misses comments that are between the temps  declaration and the body of the method; those are picked up by [aClass commentOrInheritedCommentFor: aSelector] but that method will get false positives from comments *anywhere* in the method source"]])
		isEmptyOrNil
			ifTrue:
				[aSelector
					ifNotNil:
						[((aClass methodHeaderFor: aSelector), '

Has no comment') ]
					ifNil:
						['Hamna']]
			ifFalse:	[aSelector
				ifNotNil: [((aClass methodHeaderFor: aSelector), '

', poss) ]
				ifNil: [poss]]! !

!CodeProvider methodsFor: 'message list' stamp: 'jmv 4/8/2016 12:38'!
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
	answer _ (class decompilerClass new
		decompile: selector
		in: class
		method: method) decompileString.
	^ answer copy! !

!CodeProvider methodsFor: 'message list' stamp: 'jmv 4/8/2016 12:38'!
selectedMessage
	"Answer a copy of the source code for the selected message.  This generic version is probably actually never reached, since every subclass probably reimplements and does not send to super.  In time, ideally, most, or all, reimplementors would vanish and all would defer instead to a universal version right here.  Everything in good time."

	| class selector method |

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	class _ self selectedClassOrMetaClass.
	(class isNil or: [(selector _ self selectedMessageName) isNil]) ifTrue: [^ ''].
	method _ class compiledMethodAt: selector ifAbsent: [^ ''].	"method deleted while in another project"
	currentCompiledMethod _ method.

	^ (self showComment
		ifFalse: [self sourceStringPrettifiedAndDiffed]
		ifTrue:	[ self commentContents])
			copy! !


!Browser methodsFor: 'class functions' stamp: 'jmv 4/8/2016 12:40'!
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

!Browser methodsFor: 'message functions' stamp: 'jmv 4/8/2016 12:40'!
defineMessageFrom: aString notifying: aRequestor
	"Compile the expressions in aString. Notify aRequestor if a syntax error occurs. Install the compiled method in the selected class classified under  the currently selected message category name. Answer the selector obtained if compilation succeeds, nil otherwise."
	| selectedMessageName selector category oldMessageList |
	selectedMessageName _ self selectedMessageName.
	oldMessageList _ self messageList.
	selector _ self selectedClassOrMetaClass parserClass selectorFrom: aString.
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

!Browser methodsFor: 'message list' stamp: 'jmv 4/8/2016 12:36'!
selectedMessage
	"Answer a copy of the source code for the selected message."

	| class selector method |
	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	class _ self selectedClassOrMetaClass.
	selector _ self selectedMessageName.
	method _ class compiledMethodAt: selector ifAbsent: [^ ''].	"method deleted while in another project"
	currentCompiledMethod _ method.

	^ (self showingDocumentation
		ifFalse: [ self sourceStringPrettifiedAndDiffed ]
		ifTrue: [ self commentContents ])
			copy! !


!CodeFileBrowser methodsFor: 'edit pane' stamp: 'jmv 4/8/2016 12:37'!
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
	^ answer! !


!MessageSet methodsFor: 'contents' stamp: 'jmv 4/8/2016 12:35'!
selectedMessage
	"Answer the source method for the currently selected message."

	| class selector |
	selectedMessage ifNil: [^ 'Class vanished'].
	
	class _ selectedMessage actualClass.
	selector _ selectedMessage methodSymbol.
	selector ifNil: [ ^'prims'].

	selector first isUppercase ifTrue: [
		selector == #Comment ifTrue: [
			currentCompiledMethod _ class organization commentRemoteStr.
			^ class comment].
		selector == #Definition ifTrue: [
			^ class definition].
		selector == #Hierarchy ifTrue: [^ class printHierarchy]].
	(class includesSelector: selector) ifFalse: [
		currentCompiledMethod _ nil.
		^ 'Missing'].

	self showingDecompile ifTrue: [
		^ self decompiledSource ].

	currentCompiledMethod _ class compiledMethodAt: selector ifAbsent: nil.
	self showingDocumentation ifTrue: [
		^ self commentContents ].

	^self sourceStringPrettifiedAndDiffed! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 4/8/2016 12:36'!
acceptedStringOrText
	"return the source code that shows in the bottom pane"

	| sel class strm changeType answer |
	self changed: #clearUserEdits.
	currentClassName ifNil: [^ myChangeSet preambleString ifNil: ['']].
	class _ self selectedClassOrMetaClass.
	(sel _ currentSelector)
		ifNotNil: [
			changeType _ (myChangeSet atSelector: (sel _ sel asSymbol) class: class).
			changeType == #remove
				ifTrue: [^'Method has been removed (see versions)'].
			changeType == #addedThenRemoved
				ifTrue: [^'Added then removed (see versions)'].
			changeType == #movedToOtherPackage
				ifTrue: [^'Method was moved to some other package'].
			class ifNil: [^'Method was added, but cannot be found!!'].
			(class includesSelector: sel)
				ifFalse: [^'Method was added, but cannot be found!!'].
			answer _  (class sourceCodeAt: sel).
			(#(prettyPrint prettyLineDiffs prettyWordDiffs) includes: self contentsSymbol) ifTrue: [
				answer _ (class compilerClass new
						format: answer
						in: class 
						notifying: nil)].
			self showingAnyKindOfDiffs
				ifTrue: [ answer _ (self diffFromPriorSourceFor: answer) ].
			^ answer ]
		ifNil: [
			strm _ WriteStream on: (String new: 100).
			(myChangeSet classChangeAt: currentClassName) do: [ :each |
				each == #remove ifTrue: [strm nextPutAll: 'Entire class was removed.'; newLine].
				each == #addedThenRemoved ifTrue: [strm nextPutAll: 'Class was added then removed.'; newLine].
				each == #rename ifTrue: [strm nextPutAll: 'Class name was changed.'; newLine].
				each == #add ifTrue: [strm nextPutAll: 'Class definition was added.'; newLine].
				each == #change ifTrue: [strm nextPutAll: 'Class definition was changed.'; newLine].
				each == #reorganize ifTrue: [strm nextPutAll: 'Class organization was changed.'; newLine].
				each == #comment ifTrue: [strm nextPutAll: 'New class comment.'; newLine].
				each == #movedToOtherPackage ifTrue: [strm nextPutAll: 'Class was moved to some other package.'; newLine].
			].
			^ strm contents].! !


!Debugger methodsFor: 'context stack (message list)' stamp: 'jmv 4/8/2016 12:39'!
selectedMessage
	"Answer the source code of the currently selected context."
	^self selectedContext debuggerMap sourceText! !


!ClassDescription methodsFor: 'copying' stamp: 'jmv 4/8/2016 12:31'!
copy: sel from: class classified: cat 
	"Install the method associated with the first arugment, sel, a message 
	selector, found in the method dictionary of the second argument, class, 
	as one of the receiver's methods. Classify the message under the third 
	argument, cat."

	| code category |
	"Useful when modifying an existing class"
	code _ class sourceCodeAt: sel.
	code ifNotNil: [
			category _ cat
				ifNil: [ class organization categoryOfElement: sel].
			(self methodDict includesKey: sel)
				ifTrue: [code = (self sourceCodeAt: sel) 
							ifFalse: [self error: self name 
										, ' ' 
										, sel 
										, ' will be redefined if you proceed.']].
			self compile: code classified: category]! !


!Text methodsFor: 'printing' stamp: 'jmv 4/8/2016 09:58'!
printOn: aStream

	"Add the identity of the receiver to a stream"
	(aStream isKindOf: TextStream)
		ifTrue: [
			aStream nextPutAll: self.
			^ self].
	
	self printNameOn: aStream.
	aStream nextPutAll: ' for '; print: string! !

!Text methodsFor: 'adding emphasis' stamp: 'jmv 4/8/2016 09:52'!
allBold
	"prefer shorter selector"
	self bold! !

!methodRemoval: Text #makeBoldFrom:to:!
Text removeSelector: #makeBoldFrom:to:!
!methodRemoval: Text #makeSelectorBold!
Text removeSelector: #makeSelectorBold!
!methodRemoval: Text #makeSelectorBoldIn:!
Text removeSelector: #makeSelectorBoldIn:!
!methodRemoval: Behavior #sourceMethodAt:!
Behavior removeSelector: #sourceMethodAt:!
!methodRemoval: Behavior #sourceMethodAt:ifAbsent:!
Behavior removeSelector: #sourceMethodAt:ifAbsent:!

!Text reorganize!
('accessing' append: at: at:put: commandForReplaceFrom:to:with: customizeExplorerContents embeddedMorphs embeddedMorphsFrom:to: findString:startingAt: findString:startingAt:caseSensitive: lineCount prepend: rangeOf:startingAt: replaceFrom:to:with: size string)
('comparing' = hash is:)
('copying' copyFrom:to: copyReplaceFrom:to:with: copyReplaceTokens:with: postCopy)
('converting' asNumber asString asText displayStringOrText replaceFrom:to:with:startingAt: replaceFrom:to:withString:attributes:startingAt: reversed withCuisLineEndings)
('emphasis' addAttribute: addAttribute:from:to: alignmentAt: attributesAt: attributesFrom:to:do: characterStyleOrNilAt: characterStyleOrNilIfApplying: commandForAddAttribute:from:to: commandForRemoveAttributes:from:to: emphasisAt: find: font: fontAt: fontIfApplying: initialFont: paragraphStyleOrNilAt: paragraphStyleOrNilIfApplying: removeAttributes:from:to: runLengthFor: withAttributeValues:do:)
('printing' printOn: storeOn:)
('private' basicReplaceAttributesFrom:to:with: fixParagraphAttributesFrom:to: privateSetParagraphAttributes:from:to: privateSetRuns: runs setString:setRuns:)
('attributes' canJoin:and: hasAnyAttribute hasAnyParagraphAttribute unembellished)
('user interface' edit editLabel:)
('paragraph support' encompassLine: encompassParagraph:)
('as yet unclassified' paragraphStyleChunksDo:)
('adding emphasis' allBold bold italic sub super under)
!

