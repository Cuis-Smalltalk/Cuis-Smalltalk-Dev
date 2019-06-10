'From Cuis 5.0 of 7 November 2016 [latest update: #3786] on 8 June 2019 at 6:28:48 pm'!

!String methodsFor: 'converting' stamp: 'HAW 6/8/2019 18:20:03'!
withoutSeparators
	
	^self reject: [ :aCharacter | aCharacter isSeparator ]! !


!TextEditor methodsFor: 'accessing-selection' stamp: 'HAW 6/8/2019 18:16:53'!
selectedString

	^self selection string! !


!TextEditor methodsFor: 'editing keys' stamp: 'HAW 6/8/2019 18:23:09'!
changeLineEndsToLf: aKeyboardEvent
	"Replace all CRs and CrLfs by LFs.
	Triggered by Cmd-U -- useful when getting code from FTP sites"
	"This is a user command, and generates undo"
	
	self replaceSelectionWith: self selectedString withCuisLineEndings.
	^ true! !

!TextEditor methodsFor: 'editing keys' stamp: 'HAW 6/8/2019 18:23:30'!
hiddenInfo
	"In TextLinks, TextDoits, TextColor, and TextURLs, there is hidden info.  Return the entire string that was used by Cmd-6 to create this text attribute.  Usually enclosed in < >."

	| attrList |
	attrList _ model actualContents attributesAt: (self pointIndex + self markIndex)//2.
	attrList do: [:attr |
		attr forTextActionInfoDo: [ :info |
			^ self selectedString, '<', info, '>']].
	"If none of the above"
	attrList do: [:attr |
		attr forTextColorDo: [ :color |
			^ self selectedString, '<', color printString, '>']].
	^ self selectedString, '[No hidden info]'! !

!TextEditor methodsFor: 'editing keys' stamp: 'HAW 6/8/2019 18:23:38'!
makeCapitalized: aKeyboardEvent
	"Force the current selection to uppercase.  Triggered by Cmd-X."
	"This is a user command, and generates undo"

	| prev |
	prev _ $-.  "not a letter"
	self replaceSelectionWith: 
		(self selectedString collect:
			[:c | prev _ prev isLetter ifTrue: [c asLowercase] ifFalse: [c asUppercase]]).
	^ true! !

!TextEditor methodsFor: 'editing keys' stamp: 'HAW 6/8/2019 18:23:45'!
makeLowercase: aKeyboardEvent
	"Force the current selection to lowercase.  Triggered by Cmd-X."
	"This is a user command, and generates undo"

	self replaceSelectionWith: self selectedString asLowercase.
	^ true! !

!TextEditor methodsFor: 'editing keys' stamp: 'HAW 6/8/2019 18:23:53'!
makeUppercase: aKeyboardEvent
	"Force the current selection to uppercase.  Triggered by Cmd-Y."
	"This is a user command, and generates undo"

	self replaceSelectionWith: self selectedString asUppercase.
	^ true! !

!TextEditor methodsFor: 'menu messages' stamp: 'HAW 6/8/2019 18:24:07'!
setSearchString
	"Make the current selection, if any, be the current search string."
	self hasSelection ifFalse: [morph flash. ^ self].
	self setSearch: self selectedString! !

!TextEditor methodsFor: 'nonediting/nontyping keys' stamp: 'HAW 6/8/2019 18:24:13'!
setSearchString: aKeyboardEvent
	"Establish the current selection as the current search string."

	| aString |
	self lineSelectAndEmptyCheck: [^ true].
	aString _  self selectedString.
	aString size = 0
		ifTrue: [ self flash ]
		ifFalse: [ self setSearch: aString ].
	^ true! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 6/8/2019 18:21:46'!
browseClassFromIt
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."

	| aClass |
	self wordSelectAndEmptyCheck: [^ self].

	aClass _ Utilities
		classFromPattern: self selectedString withBlanksCondensed
		withCaption: 'choose a class to browse...'.
	aClass ifNil: [^ morph flash].

	HierarchyBrowserWindow
		onClass: aClass
		selector: nil! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 6/8/2019 18:21:52'!
classCommentsContainingIt
	"Open a browser class comments which contain the current selection somewhere in them."

	self lineSelectAndEmptyCheck: [^ self].
	Smalltalk browseClassCommentsWithString: self selectedString! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 6/8/2019 18:22:28'!
explain
	"Try to shed some light on what kind of entity the current selection is. 
	The selection must be a single token or construct. Insert the answer after 
	the selection. Send private messages whose names begin with 'explain' 
	that return a string if they recognize the selection, else nil."

	| string tiVars cgVars selectors delimitors numbers sorry reply symbol provider |
	sorry _ 'Sorry, I can''t explain that.  Please select a single
token, construct, or special character.'.
	(string _ self selectedString) isEmpty
		ifTrue: [reply _ '']
		ifFalse: [
			string _ string withBlanksTrimmed.
			"Remove space, tab, cr"
			"Temps and Instance vars need only test strings that are all letters"
			(string detect: [:char | char isValidInIdentifiers not]
				ifNone: nil) ifNil: [
					tiVars _ (self explainTemp: string)
						ifNil: [self explainInst: string]].
					
			provider _ self codeProvider.
			(tiVars == nil and: [provider respondsTo: #explainSpecial:])
				ifTrue: [tiVars _ provider explainSpecial: string].
			tiVars _ tiVars
				ifNil: [ '']
				ifNotNil: [ tiVars , '\' withNewLines].
			"Context, Class, Pool, and Global vars, and Selectors need 
			only test symbols"
			(Symbol hasInterned: string ifTrue: [:s | symbol _ s])
				ifTrue: [
					cgVars _ (self explainCtxt: symbol) 
						ifNil: [ (self explainClass: symbol)
							ifNil: [ self explainGlobal: symbol]].
					"See if it is a Selector (sent here or not)"
					selectors _ (self explainMySel: symbol)
						ifNil: [(self explainPartSel: string)
							ifNil: [ self explainAnySel: symbol]]]
				ifFalse: [selectors _ self explainPartSel: string].
			cgVars _ cgVars
				ifNil: [ '']
				ifNotNil: [cgVars , '\' withNewLines].
			selectors _ selectors
				ifNil: [ '']
				ifNotNil: [ selectors , '\' withNewLines].
			delimitors _ string size = 1
				ifTrue: ["single special characters"
					self explainChar: string]
				ifFalse: ["matched delimitors"
					self explainDelimitor: string].
			numbers _ self explainNumber: string.
			numbers ifNil: [numbers _ ''].
			delimitors ifNil: [delimitors _ ''].
			reply _ tiVars , cgVars , selectors , delimitors , numbers].
	reply size = 0 ifTrue: [reply _ sorry].

	morph showBalloon: reply.
	self runningWorld ifNotNil: [ :w | w findATranscript ].
	reply print! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 6/8/2019 18:22:40'!
methodSourceContainingIt
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source).  Slow!!"

	self lineSelectAndEmptyCheck: [^ self].
	Smalltalk browseMethodsWithSourceString: self selectedString! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 6/8/2019 18:22:47'!
methodStringsContainingit
	"Open a browser on methods which contain the current selection as part of a string constant."

	self lineSelectAndEmptyCheck: [^ self].
	Smalltalk browseMethodsWithString: self selectedString! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 6/8/2019 18:25:03'!
referencesToIt
	"Open a references browser on the selected symbol: a variable name or class name"

	| selectedString provider environment |

	"look for exactly a whole word"
	self selectWord.	
	selectedString _ self selectedString withoutSeparators.

	"convenient access to class variables, including those in SharedPools"
	provider _ self codeProvider.
	environment _ (provider respondsTo: #selectedClassOrMetaClass) ifTrue: [ provider selectedClassOrMetaClass ].
	environment _ environment ifNil: [ Smalltalk ].

	(environment bindingOf: selectedString)
		ifNotNil: [ :reference | Smalltalk browseAllCallsOn: reference ]
		ifNil: [ (environment ~= Smalltalk and: [ environment definesInstanceVariableNamedInHierarchy: selectedString ])
			ifTrue: [ Smalltalk browseAllAccessesTo: selectedString from: environment ]
			ifFalse: [ morph flash ]]
			

	! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 6/8/2019 18:22:55'!
selectedSelector
	"Try to make a selector out of the current text selection"

	^ self selectedString findSelector! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 6/8/2019 18:20:31'!
selectedSymbol
	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"

	| aString |
	self hasSelection ifFalse: [^ nil].
	aString _ self selectedString withoutSeparators.
	aString size = 0 ifTrue: [^ nil].
	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].

	^ nil! !


!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: lineCount lineIndicesDo: lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string)
('comparing' < <= = > >= alike: beginsWith: caseInsensitiveLessOrEqual: caseSensitiveLessOrEqual: commonPartWith:startAt:stopAt:applying: commonPrefixWith: compare: compare:caseSensitive: crc16 endsWith: hash is:substringAt: match: sameAs: startingAt:match:startingAt:)
('copying' , append: appendToString: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('converting' aOrAnPrefix adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: afterBlanksEndsWith: asCamelCase asDate asFileName asHex asIdentifier: asInteger asLowercase asMonth asNumber asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUppercase asUtf8 asUtf8: asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findSelector firstNoBlankIndex initialIntegerOrNil keywords lastNoBlankIndex prefixAndSuffix: prefixedWithAOrAn romanNumber squeezedTo: substrings surroundedBySingleQuotes translateFrom:to:table: translateToLowercase translateToUppercase translateWith: truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withLineEndings: withNewLines withoutEnclosing: withoutLeadingBlanks withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('printing' isLiteral print printOn: storeOn:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom: replaceFrom:to:with:startingAt:)
('system primitives' compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' hasContentsInExplorer isAlphaNumeric isRemote isString)
('paragraph support' encompassLine: encompassParagraph: endOfParagraphBefore: indentationIfBlank:)
('arithmetic' * + - / // \\)
('user interface' edit editLabel:)
('formatting' format:)
('uCompletion' separateKeywords)
('fileman-converting' asAbsolutePathName asDirectoryEntry asDriveName asFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-actions' fileContents fileContents:)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('text conversion helpers' bold italic struck sub super under)
!

