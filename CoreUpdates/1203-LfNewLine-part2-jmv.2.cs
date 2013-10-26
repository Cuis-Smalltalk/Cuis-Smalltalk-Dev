'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 14 March 2012 at 12:02:56 pm'!

!Object methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:40'!
      longPrintOn: aStream	"Append to the argument, aStream, the names and values of all 	of the receiver's instance variables."	self class allInstVarNames doWithIndex: [ :title :index |		aStream nextPutAll: title;		 nextPut: $:;		 space;		 tab;		 print: (self instVarAt: index);		 newLine]! !

!Object methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:40'!
                        longPrintOn: aStream limitedTo: sizeLimit indent: indent	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  Limit is the length limit for each inst var."	self class allInstVarNames doWithIndex: [ :title :index |		indent timesRepeat: [aStream tab].		aStream nextPutAll: title;		 nextPut: $:;		 space;		 tab;		 nextPutAll: 			((self instVarAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1));		 newLine ]! !

!Object methodsFor: 'printing' stamp: 'jmv 3/13/2012 16:56'!
                               longPrintString	"Answer a String whose characters are a description of the receiver."		| str |	str _ String streamContents: [:aStream | self longPrintOn: aStream].	"Objects without inst vars should return something"	^ str isEmpty ifTrue: [self printString, String newLineString ] ifFalse: [str]! !

!Object methodsFor: 'printing' stamp: 'jmv 3/13/2012 16:56'!
                  longPrintStringLimitedTo: aLimitValue	"Answer a String whose characters are a description of the receiver."		| str |	str := String streamContents: [:aStream | self longPrintOn: aStream limitedTo: aLimitValue indent: 0].	"Objects without inst vars should return something"	^ str isEmpty ifTrue: [self printString, String newLineString ] ifFalse: [str]! !

!Object methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:40'!
                          print	Transcript show: self printString; newLine! !

!Object methodsFor: 'private' stamp: 'jmv 3/13/2012 12:42'!
              primitiveError: aString 	"This method is called when the error handling results in a recursion in 	calling on error: or halt or halt:."	| context emergencyEvaluator lines r |	r _ 10@10 extent: (Display extent -20 min: 700@1000).	lines _ r height // StrikeFont default height.	emergencyEvaluator _ Transcripter newInFrame: r.	emergencyEvaluator		nextPutAll: '***System error handling failed***'; newLine;		nextPutAll: aString; newLine;		nextPutAll: '-------------------------------'; newLine.	context _ thisContext sender sender.	(30 min: lines - 10) timesRepeat: [context ifNotNil: [emergencyEvaluator print: (context _ context sender); newLine]].	emergencyEvaluator		nextPutAll: '-------------------------------'; newLine;		nextPutAll: 'Type ''revert'' to revert your last method change.'; newLine;		nextPutAll: 'Type ''exit'' to exit the emergency evaluator.'; newLine.	emergencyEvaluator readEvalPrint.	World install "init hands and redisplay"! !


!AutoCompleter methodsFor: 'keyboard' stamp: 'jmv 3/13/2012 10:19'!
                 handleKeystrokeBefore: kbEvent	"I return a boolean. true when I have handled the event and no futher processing is needed by the caller."	| currentPos currentCharIsAlphaNumeric keyValue ctrl cmd tab colon alphanum backspace esc space return keyChar  |	currentPos _ textMorph editor startIndex-1.	currentCharIsAlphaNumeric _ currentPos > 0 and: [ model textSize >= currentPos and: [			(model actualContents at: currentPos) isAlphaNumeric ]].	keyValue _ kbEvent keyValue.	keyChar _ kbEvent keyCharacter.	ctrl _ kbEvent controlKeyPressed.	cmd _ kbEvent commandAltKeyPressed.	tab _ keyChar = Character tab.	colon _keyChar = $:.	alphanum _ kbEvent keyCharacter isAlphaNumeric.	backspace _ keyValue = 8.	esc _ keyValue = 27.	space _ #(0 32 160) includes: keyValue.	return _ kbEvent isReturnKey.	"Stuff to do if the menu is not open"	menuMorph ifNil: [		"Ctrl-Space or Tab for open"		"Mac specific note: Using option-space (actually option+160) effectively disables the non-breaking space character 160"		(space & (ctrl | kbEvent rawMacOptionKeyPressed) or: [			(self opensWithTab and: [tab]) and: [ currentCharIsAlphaNumeric ]])				ifTrue: [ self openCompletionMenu. ^ true].		"Auto-open - currently deactivated""		(ctrl not & cmd not & alphanum) 			ifTrue: [ self openCompletionMenu ]."		^ false].	"Starting here, stuff to do if the menu is open"	menuMorph stillActive.	"Escape"	esc ifTrue: [ self closeMenu. ^ true].	"Backspace"	backspace ifTrue: [		currentCharIsAlphaNumeric ifFalse: [ self closeMenu ].		^ false].	"Home"	keyValue = 1 ifTrue: [ menuMorph home. ^ true ].	"End"	keyValue = 4 ifTrue: [ menuMorph end. ^ true].	"?"	keyChar = $? ifTrue: [ menuMorph help. ^true].	"Arrow up"	keyValue = 30 ifTrue: [ menuMorph moveUp. ^ true].	"Arrow down"	keyValue = 31 ifTrue: [ menuMorph moveDown. ^ true].	"Page up"	keyValue = 11 ifTrue: [ menuMorph pageUp. ^ true].	"Page down"	keyValue = 12 ifTrue: [ menuMorph pageDown. ^ true].	"Return, Tab or Ctrl-Space"	(return or: [ space & (ctrl | kbEvent rawMacOptionKeyPressed) or: [ tab]]) ifTrue: [		self insertSelected			ifTrue: [^ true]].	"All keys but the alphanumeric chars (without command and control ) 	and the backspace key do close the menu"	(ctrl not & cmd not and: [ alphanum | colon])		ifFalse: [ self closeMenu ].	^false! !


!Base64MimeConverter methodsFor: 'conversion' stamp: 'jmv 3/13/2012 12:14'!
                 mimeEncode	"Convert from data to 6 bit characters."	| phase1 phase2 raw nib lineLength |	phase1 _ phase2 _ false.	lineLength := 0.	[dataStream atEnd] whileFalse: [		lineLength >= 70 ifTrue: [ mimeStream newLine.  lineLength := 0. ].		data _ raw _ dataStream next asInteger.		nib _ (data bitAnd: 16rFC) bitShift: -2.		mimeStream nextPut: (ToCharTable at: nib+1).		(raw _ dataStream next) ifNil: [raw _ 0. phase1 _ true].		data _ ((data bitAnd: 3) bitShift: 8) + raw asInteger.		nib _ (data bitAnd: 16r3F0) bitShift: -4.		mimeStream nextPut: (ToCharTable at: nib+1).		(raw _ dataStream next) ifNil: [raw _ 0. phase2 _ true].		data _ ((data bitAnd: 16rF) bitShift: 8) + (raw asInteger).		nib _ (data bitAnd: 16rFC0) bitShift: -6.		mimeStream nextPut: (ToCharTable at: nib+1).		nib _ (data bitAnd: 16r3F).		mimeStream nextPut: (ToCharTable at: nib+1).		lineLength := lineLength + 4.].	phase1 ifTrue: [mimeStream skip: -2; nextPut: $=; nextPut: $=.			^ mimeStream].	phase2 ifTrue: [mimeStream skip: -1; nextPut: $=.			^ mimeStream].! !


!Behavior methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:14'!
                            longPrintOn: aStream	"Append to the argument, aStream, the names and values of all of the receiver's instance variables.  But, not useful for a class with a method dictionary."	aStream nextPutAll: '<<too complex to show>>'; newLine! !

!Behavior methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:29'!
                  printHierarchy	"Answer a description containing the names and instance variable names 	of all of the subclasses and superclasses of the receiver."	| aStream index |	index _ 0.	aStream _ WriteStream on: (String new: 16).	self allSuperclasses reverseDo: [ :aClass | 		aStream newLineTab: index.		index _ index + 1.		aStream nextPutAll: aClass name.		aStream space.		aStream print: aClass instVarNames].	aStream newLine.	self printSubclassesOn: aStream level: index.	^aStream contents! !

!Behavior methodsFor: 'compiling' stamp: 'jmv 3/13/2012 12:14'!
           compile: code notifying: requestor 	"Compile the argument, code, as source code in the context of the 	receiver and insEtall the result in the receiver's method dictionary. The 	second argument, requestor, is to be notified if an error occurs. The 	argument code is either a string or an object that converts to a string or 	a PositionableStream. This method also saves the source code."		| methodAndNode |	methodAndNode _ self		basicCompile: code "a Text"		notifying: requestor		trailer: self defaultMethodTrailer		ifFail: [^nil].	methodAndNode method putSource: code fromParseNode: methodAndNode node inFile: 2			withPreamble: [:f | f newLine; nextPut: $!!; nextChunkPut: 'Behavior method'; newLine].	self addSelector: methodAndNode selector withMethod: methodAndNode method notifying: requestor.	^ methodAndNode selector! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 3/13/2012 22:09'!
  standardMethodHeaderFor: aSelector
	| args |
	args _ (1 to: aSelector numArgs)	collect:[:i| 'arg', i printString].
	args size = 0 ifTrue:[^aSelector asString].
	args size = 1 ifTrue:[^aSelector,' arg1'].
	^String streamContents:[:s|
		(aSelector findTokens: ':') with: args do: [ :tok :arg |
			s nextPutAll: tok; nextPutAll:': '; nextPutAll: arg; nextPutAll:' '.
		].
	].
! !

!Behavior methodsFor: 'user interface' stamp: 'jmv 3/13/2012 21:53'!
 crossReference
	"Answer an Array of arrays of size 2 whose first element is a message selector in the receiver's method dictionary and whose second element is a set of all message selectors in the method dictionary whose methods send a message with that selector. Subclasses are not included."

	^self selectors asArray sort collect: [ :x |
		Array 
			with: String newLineString, x 
			with: (self whichSelectorsReferTo: x) ]

	"
	Point crossReference.
	"! !


!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 3/13/2012 23:02'!
             valueSupplyingAnswers: aListOfPairs
	"evaluate the block using a list of questions / answers that might be called upon to
	automatically respond to Object>>confirm: or FillInTheBlank requests"

	^self
		on: ProvideAnswerNotification
		do: [ :notification |
			| caption |
			caption _ notification messageText withBlanksCondensed. "to remove new lines"
			aListOfPairs
				detect:  [ :each |
					caption = each first
						or: [ (caption includesSubstring: each first caseSensitive: false)
						or: [ each first match: caption ] ] ]
				ifFound: [ :answer | notification resume: answer second ]
				ifNone: [
					(ProvideAnswerNotification signal: notification messageText)
						ifNil: [ notification resume ]
						ifNotNil: [ :outerAnswer | notification resume: outerAnswer ] ] ]! !

!BlockClosure methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:16'!
       fullPrintOn: aStream	aStream print: self; newLine.	(self decompile ifNil: ['--source missing--']) printOn: aStream indent: 0! !


!BlockLocalTempCounter methodsFor: 'initialize-release' stamp: 'jmv 3/13/2012 12:16'!
      testTempCountForBlockAt: startPc in: method	"Compute the number of local temporaries in a block.	 If the block begins with a sequence of push: nil bytecodes then some of	 These could be initializing local temps.  We can only reliably disambuguate	 them from other uses of nil by parsing the stack and seeing what the offset	 of the stack pointer is at the end of the block.There are short-cuts.  The only	 one we take here is		- if there is no sequence of push nils there can be no local temps"	| symbolicLines line prior thePc |	symbolicLines := Dictionary new.	method symbolicLinesDo:		[:pc :lineForPC| symbolicLines at: pc put: lineForPC].	stackPointer := 0.	scanner := InstructionStream new method: method pc: startPc.	scanner interpretNextInstructionFor: self.	blockEnd ifNil:		[self error: 'pc is not that of a block'].	scanner nextByte = Encoder pushNilCode ifTrue:		[joinOffsets := Dictionary new.		 [scanner pc < blockEnd] whileTrue:			[line := symbolicLines at: scanner pc.			 prior := stackPointer.			 thePc := scanner pc.			 scanner interpretNextInstructionFor: self.			 Transcript newLine; print: prior; nextPutAll: '->'; print: stackPointer;  tab; print: thePc; tab; nextPutAll: line]].	^stackPointer! !


!Categorizer methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:17'!
printOn: aStream 	"Refer to the comment in Object|printOn:."	| elementIndex |	elementIndex _ 1.	1 to: categoryArray size do: [ :i | 		aStream nextPut: $(.		(categoryArray at: i) asString printOn: aStream.		[elementIndex <= (categoryStops at: i)]			whileTrue: 				[aStream space; nextPutAll: (elementArray at: elementIndex).				elementIndex _ elementIndex + 1].		aStream nextPut: $); newLine]! !


!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:14'!
                           fileOutCommentOn: aFileStream moveSource: moveSource toFile: fileIndex	"Copy the class comment to aFileStream.  If moveSource is true (as in compressChanges or compressSources, then update classComment to point to the new file."	| fileComment |	classComment ifNotNil: [			aFileStream newLine.			fileComment _ RemoteString newString: classComment text							onFileNumber: fileIndex toFile: aFileStream.			moveSource ifTrue: [classComment _ fileComment]]! !

!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:14'!
    putCommentOnFile: aFileStream numbered: sourceIndex moveSource: moveSource forClass: aClass	"Store the comment about the class onto file, aFileStream."	| header |	classComment ifNotNil: [		aFileStream newLine; nextPut: $!!.		header _ String streamContents: [:strm | 				strm nextPutAll: aClass name;				nextPutAll: ' commentStamp: '.				commentStamp ifNil: [commentStamp _ '<historical>'].				commentStamp storeOn: strm.				strm nextPutAll: ' prior: '; nextPutAll: '0'].		aFileStream nextChunkPut: header.		aClass organization fileOutCommentOn: aFileStream				moveSource: moveSource toFile: sourceIndex.		aFileStream newLine]! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 3/13/2012 12:30'!
                          fileOutOn: aFileStream	"File the receiver out on the given file stream"	| aString |	type == #method		ifTrue:			[aFileStream newLine; nextPut: $!!.			aString _  class asString							, (meta ifTrue: [' class methodsFor: ']									ifFalse: [' methodsFor: '])							, category asString printString.			stamp ifNotNil:				[aString _ aString, ' stamp: ''', stamp, ''''].			aFileStream nextChunkPut: aString.			aFileStream newLine].	type == #preamble ifTrue: [aFileStream nextPut: $!!].	type == #classComment		ifTrue:			[aFileStream nextPut: $!!.			aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.			aFileStream newLine].			type == #classDefinition ifTrue: [		aFileStream nextPut: $!!.		aFileStream nextChunkPut: 			'classDefinition: ', 			(self isMetaClassChange ifTrue: [self methodClassName, ' class'] ifFalse: [self methodClassName]) printString,			' category: ', self category printString.		aFileStream newLine		].	aFileStream nextChunkPut: self string.		type == #method ifTrue: [aFileStream nextChunkPut: ' '; newLine].	type == #classComment ifTrue: [aFileStream newLine].	aFileStream newLine! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 23:13'!
                 checkForConversionMethods
	"See if any conversion methods are needed"
	| tell choice list smart restore renamed listAdd listDrop msgSet |

	Preferences conversionMethodsAtFileOut ifFalse: [^ self].	"Check preference"
	structures ifNil: [^ self].

	list _ OrderedCollection new.
	renamed _ OrderedCollection new.
	self changedClasses do: [ :class | | oldStruct newStruct need sel rec |
		need _ (self atClass: class includes: #new) not.
		need ifTrue: ["Renamed classes."
			(self atClass: class includes: #rename) ifTrue: [
				rec _ changeRecords at: class name.
				rec priorName ifNotNil: [
					(structures includesKey: rec priorName) ifTrue: [
						renamed add: class.  need _ false]]]].
		need ifTrue: [need _ (self atClass: class includes: #change)].
		need ifTrue: [oldStruct _ structures at: class name 
									ifAbsent: [need _ false.  #()]].
		need ifTrue: [
			newStruct _ (Array with: class classVersion), (class allInstVarNames).
			need _ (oldStruct ~= newStruct)].
		need ifTrue: [sel _ #convertToCurrentVersion:refStream:.
			(#(add change) includes: (self atSelector: sel class: class)) ifFalse: [
				list add: class]].
		].

	list isEmpty & renamed isEmpty ifTrue: [^ self].
	"Ask user if want to do this"
	tell _ 'If there might be instances of ', (list asArray, renamed asArray) printString,
		'\in a project (.pr file) on someone''s disk, \please ask to write a conversion method.\' withNewLines,
		'After you edit the conversion method, you''ll need to fileOut again.\' withNewLines,
		'The preference conversionMethodsAtFileOut in category "fileout" controls this feature.'.
	choice _ (PopUpMenu labels: 
'Write a conversion method by editing a prototype
These classes are not used in any object file.  fileOut my changes now.
I''m too busy.  fileOut my changes now.
Don''t ever ask again.  fileOut my changes now.') startUpWithCaption: tell. 
	choice = 4 ifTrue: [Preferences disable: #conversionMethodsAtFileOut].
	choice = 2 ifTrue: ["Don't consider this class again in the changeSet"
			list do: [:cls | structures removeKey: cls name ifAbsent: nil].
			renamed do: [:cls | | nn |
				nn _ (changeRecords at: cls name) priorName.
				structures removeKey: nn ifAbsent: nil]].
	choice ~= 1 ifTrue: [^ self].	"exit if choice 2,3,4"

	listAdd _ self askAddedInstVars: list.	"Go through each inst var that was added"
	listDrop _ self askRemovedInstVars: list.	"Go through each inst var that was removed"
	list _ (listAdd, listDrop) asSet asArray.

	smart _ SmartRefStream on: (RWBinaryOrTextStream on: '12345').
	smart structures: structures.
	smart superclasses: superclasses.
	(restore _ self class current) == self ifFalse: [
		self class  newChanges: self].	"if not current one"
	msgSet _ smart conversionMethodsFor: list.
		"each new method is added to self (a changeSet).  Then filed out with the rest."
	self askRenames: renamed addTo: msgSet using: smart.	"renamed classes, add 2 methods"
	restore == self ifFalse: [self class newChanges: restore].
	msgSet messageList isEmpty ifTrue: [^ self].
	self inform: 'Remember to fileOut again after modifying these methods.'.
	MessageSetWindow open: msgSet label: 'Conversion methods for ', self name.! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:18'!
             fileOutChangesFor: class on: stream 	"Write out all the method changes for this class."	| changes |	changes _ Set new.	(self methodChangesAtClass: class name) associationsDo: 		[:mAssoc | (mAssoc value == #remove or: [mAssoc value == #addedThenRemoved])			ifFalse: [changes add: mAssoc key]].	changes isEmpty ifFalse: [		class fileOutChangedMessages: changes on: stream.		stream newLine ]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:18'!
           fileOutMethodRemovalsFor: class on: stream 	"Write out removals and initialization for this class."	| dict classRecord |	classRecord _ changeRecords at: class name ifAbsent: [^ self].	dict _ classRecord methodChangeTypes.	dict keysSortedSafely do: [ :key | | changeType |		changeType _ dict at: key.		(#(remove addedThenRemoved) includes: changeType)			ifTrue: [				stream nextPut: $!!; nextChunkPut: 'methodRemoval: ', class name, ' ', key storeString; newLine.				stream nextChunkPut: class name, ' removeSelector: ', key storeString; newLine]].! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:18'!
           fileOutOn: stream 	"Write out all the changes the receiver knows about"	| classList |	(self isEmpty and: [stream isKindOf: FileStream])		ifTrue: [self inform: 'Warning: no changes to file out'].	classList _ ChangeSet superclassOrder: self changedClasses asOrderedCollection.	"First put out rename, max classDef and comment changes."	classList do: [:aClass | self fileOutClassDefinition: aClass on: stream].	"Then put out all the method changes"	classList do: [:aClass | self fileOutChangesFor: aClass on: stream].	"Finally put out removals, final class defs and reorganization if any"	classList reverseDo: [:aClass |		self fileOutMethodRemovalsFor: aClass on: stream.		self fileOutPSFor: aClass on: stream ].	self classRemoves sort do: [ :aClassName |		stream nextPut: $!!; nextChunkPut: ('classRemoval: #', aClassName); newLine.		stream nextChunkPut: 'Smalltalk removeClassNamed: #', aClassName; newLine]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:19'!
                           fileOutPSFor: class on: stream 	"Write out removals and initialization for this class."	| dict classRecord currentDef |	classRecord _ changeRecords at: class name ifAbsent: [^ self].	dict _ classRecord methodChangeTypes.	((dict includesKey:  #initialize) and: [ class isMeta ]) ifTrue: [		stream nextChunkPut: class soleInstance name, ' initialize'; newLine].	((classRecord includesChangeType: #change)			and: [(currentDef _ class definition) ~= (self fatDefForClass: class)]) ifTrue: [		stream			nextPut: $!!;			nextChunkPut: class definitionPreamble; newLine;			nextChunkPut: currentDef; newLine].	(classRecord includesChangeType: #reorganize) ifTrue: [		class fileOutOrganizationOn: stream.		stream newLine]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:19'!
   fileOutPostscriptOn: stream 	"If the receiver has a postscript, put it out onto the stream.  "	| aString |	aString _ self postscriptString.	(aString notNil and: [ aString size > 0])		ifTrue: [			stream nextChunkPut: aString "surroundedBySingleQuotes".			stream newLine; newLine]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:20'!
                           fileOutPreambleOn: stream 	"If the receiver has a preamble, put it out onto the stream.  "	| aString |	aString _ self preambleString.	(aString notNil and: [aString size > 0])		ifTrue: [			stream nextChunkPut: aString "surroundedBySingleQuotes".			stream newLine; newLine]! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:20'!
  preambleTemplate	"Answer a string that will form the default contents for a change set's preamble.	Just a first stab at what the content should be."	^ String streamContents: [:strm |		strm nextPutAll: '"Change Set:'.  "NOTE: fileIn recognizes preambles by this string."		strm tab;tab; nextPutAll: self name.		strm newLine; nextPutAll: 'Date:'; tab; tab; tab; nextPutAll: Date today printString.		strm newLine; nextPutAll: 'Author:'; tab; tab; tab; nextPutAll: Preferences defaultAuthorName.		strm newLine; newLine; nextPutAll: '<your descriptive text goes here>"']"ChangeSet current preambleTemplate"! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 22:50'!
                       summaryStringDelta: delta
	"Answer the string summarizing this changeSet"
	^ String streamContents: [ :s | | intName ps |
		intName _ self name splitInteger.
		intName first isNumber
			ifTrue: [s nextPutAll: (intName first + delta) printString , intName last]
			ifFalse: [s nextPutAll: intName first  "weird convention of splitInteger"].
		(ps _ self preambleString)
			ifNil: [s newLine]
			ifNotNil: [ | s2 date author |
				s2 _ ReadStream on: ps.
				s2 match: 'Date:'; skipSeparators.  date _ s2 crLfNextLine.
				s2 match: 'Author:'; skipSeparators.  author _ s2 crLfNextLine.
				s nextPutAll: ' -- '; nextPutAll: author; nextPutAll: ' -- '; nextPutAll: date; newLine.
				[s2 atEnd] whileFalse: [ | line |
					line _ s2 crLfNextLine.
					(line isEmpty or: [line = '"']) ifFalse: [s nextPutAll: line; newLine]]]].
! !

!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:20'!
verboseFileOut	"File out the receiver, to a file whose name is a function of the change-set name and either of the date & time or chosen to have a unique numeric tag, depending on the preference 'changeSetVersionNumbers'"	ChangeSet current fileOut.	Transcript newLine; show: 'Changes filed out ', Date dateAndTimeNow printString! !

!ChangeSet methodsFor: 'private' stamp: 'jmv 3/13/2012 12:18'!
                fileOutClassDefinition: class on: stream 	"Write out class definition for the given class on the given stream, if the class definition was added or changed."	(self atClass: class includes: #rename) ifTrue: [		stream nextChunkPut: 'Smalltalk renameClassNamed: #', (self oldNameFor: class), ' as: #', class name; newLine].	(self atClass: class includes: #change) ifTrue: [ "fat definition only needed for changes"		stream			nextPut: $!!; nextChunkPut: class definitionPreamble; newLine;			nextChunkPut: (self fatDefForClass: class); newLine.	] ifFalse: [		(self atClass: class includes: #add) ifTrue: [ "use current definition for add"			stream				nextPut: $!!; nextChunkPut: class definitionPreamble; newLine;				nextChunkPut: class definition; newLine.		].	].	(self atClass: class includes: #comment) ifTrue: [		class theNonMetaClass organization putCommentOnFile: stream numbered: 0 moveSource: false forClass: class theNonMetaClass.		stream newLine].! !


!ChangeSetPackageExporter methodsFor: 'services' stamp: 'jmv 3/13/2012 12:21'!
                             fileOutDefinitionForClass: aClass	stream		nextPut: $!!; nextChunkPut: aClass definitionPreamble; newLine;		nextChunkPut: aClass definition; newLine;		nextPut: $!!; nextChunkPut: aClass class definitionPreamble; newLine;		nextChunkPut: aClass class definition; newLine;		newLine! !

!ChangeSetPackageExporter methodsFor: 'services' stamp: 'jmv 3/13/2012 12:21'!
               fileOutInitializerForClass: aClass	stream nextChunkPut: aClass name, ' initialize'; newLine! !

!ChangeSetPackageExporter methodsFor: 'services' stamp: 'jmv 3/13/2012 12:21'!
                fileOutSystemCategories	package systemCategories do: [ :cat | self fileOutSystemCategory: cat ].	stream newLine; newLine! !

!ChangeSetPackageExporter methodsFor: 'services' stamp: 'jmv 3/13/2012 12:21'!
                  fileOutSystemCategory: categoryName	stream		nextChunkPut: 'SystemOrganization addCategory: ', categoryName printString;		newLine! !


!CharacterBlock methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:30'!
                 printOn: aStream	aStream nextPutAll: 'a CharacterBlock with index '.	stringIndex printOn: aStream.	(text notNil and: [text size> 0 and: [stringIndex between: 1 and: text size]])		ifTrue: [			aStream nextPutAll: ' and character '.			(text at: stringIndex) printOn: aStream].	aStream nextPutAll: ' and rectangle '.	super printOn: aStream.	textLine ifNotNil: [		aStream newLine; nextPutAll: ' in '.		textLine printOn: aStream].! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 3/13/2012 23:13'!
                          name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory |
	instVars _ Smalltalk actualScannerClass new scanFieldNames: instVarString.
	classVars _ (Smalltalk actualScannerClass new scanFieldNames: classVarString) collect: [:x | x asSymbol].

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
		ifTrue: [SystemChangeNotifier uniqueInstance class: newClass recategorizedFrom: oldCategory to: category].
	^newClass! !

!ClassBuilder methodsFor: 'validation' stamp: 'jmv 3/13/2012 23:13'!
                     validateClassName: aString
	"Validate the new class name"
	aString first isUppercase ifFalse:[
		self error: 'Class names must be capitalized'.
		^false].
	Smalltalk at: aString ifPresent:[:old|
		(old isKindOf: Behavior) ifFalse:[
			self notify: aString asText allBold, 
						' already exists!!\Proceed will store over it.' withNewLines]].
	^true! !


!ClassDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/13/2012 12:23'!
       fileOutOn: stream	"File the receiver out on the given file stream"		stream nextPut: $!!; nextChunkPut: ('classRemoval: #', clsName); newLine.	stream nextChunkPut: 'Smalltalk removeClassNamed: #', clsName; newLine! !


!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 3/13/2012 12:23'!
                 browseClassVarRefs 	"Put up a menu offering all class variable names; if the user chooses one, open up a message-list browser on all methods that refer to the selected class variable"	| lines labelStream allVars index owningClasses |	lines _ OrderedCollection new.	allVars _ OrderedCollection new.	owningClasses _ OrderedCollection new.	labelStream _ WriteStream on: (String new: 200).	self withAllSuperclasses reverseDo: [ :class | | vars |		vars _ class classVarNames asArray sort.		vars do: [ :var |			labelStream nextPutAll: var; newLine.			allVars add: var.			owningClasses add: class].		vars isEmpty ifFalse: [ lines add: allVars size ]].	labelStream contents isEmpty ifTrue: [^Beeper beep]. "handle nil superclass better"	labelStream skip: -1 "cut last CR".	index _ (PopUpMenu labels: labelStream contents lines: lines) startUp.	index = 0 ifTrue: [^ self].	Smalltalk browseAllCallsOn:		((owningClasses at: index) classPool associationAt: (allVars at: index))! !

!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 3/13/2012 12:24'!
                 chooseClassVarName 	"Present the user with a list of class variable names and answer the one selected, or nil if none"	| lines labelStream  allVars index |	lines _ OrderedCollection new.	allVars _ OrderedCollection new.	labelStream _ WriteStream on: (String new: 200).	self withAllSuperclasses reverseDo: [ :class | | vars |		vars _ class classVarNames asArray sort.		vars do: [ :var |			labelStream nextPutAll: var; newLine.			allVars add: var].		vars isEmpty ifFalse: [lines add: allVars size]].	labelStream contents isEmpty ifTrue: [^Beeper beep]. "handle nil superclass better"	labelStream skip: -1 "cut last CR".	index _ (PopUpMenu labels: labelStream contents lines: lines) startUp.	index = 0 ifTrue: [^ nil].	^ allVars at: index! !

!ClassDescription methodsFor: 'instance variables' stamp: 'jmv 3/13/2012 23:13'!
                          chooseInstVarThenDo: aBlock
	"Put up a menu of all the instance variables in the receiver, and when
the user chooses one, evaluate aBlock with the chosen variable as its
parameter.  If the list is 6 or larger, then offer an alphabetical
formulation as an alternative. triggered by a 'show alphabetically' item
at the top of the list."

	| lines labelStream allVars index count offerAlpha |
	(count _ self allInstVarNames size) = 0 ifTrue: 
		[ ^ self inform: 'There are no\instance variables.' withNewLines ].

	allVars _ OrderedCollection new.
	lines _ OrderedCollection new.
	labelStream _ WriteStream on: (String new: 200).

	(offerAlpha _ count > 5)
		ifTrue:
			[lines add: 1.
			allVars add: 'show alphabetically'.
			labelStream nextPutAll: allVars first; newLine].
	self withAllSuperclasses reverseDo: [ :class | | vars |
		vars _ class instVarNames.
		vars do: [ :var |
			labelStream nextPutAll: var; newLine.
			allVars add: var].
		vars isEmpty ifFalse: [lines add: allVars size]].
	labelStream skip: -1 "cut last CR".
	(lines size > 0 and: [ lines last = allVars size ]) ifTrue:
		[ lines removeLast ].  "dispense with inelegant line beneath last item"
	index _ (PopUpMenu labels: labelStream contents lines: lines)
startUpWithCaption: 'Instance variables in
', self name.
	index = 0 ifTrue: [^ self].
	(index = 1 and: [offerAlpha]) ifTrue: [^ self
chooseInstVarAlphabeticallyThenDo: aBlock].
	aBlock value: (allVars at: index)! !

!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 3/13/2012 12:26'!
 recoverFromMDFaultWithTrace	"This method handles emthodDict faults to support, eg, discoverActiveClasses (qv)."	self recoverFromMDFault.	Smalltalk at: #MDFaultDict ifPresent:		[:faultDict | faultDict at: self name put:			(String streamContents:				[:strm | (thisContext stackOfSize: 20) do: [:item | strm print: item; newLine]])]"Execute the following statement to induce MD fault tracing.  This means that, not only will all active classes be recorded but, after a test run, MDFaultDict will contain, for every class used, a stack trace showing how it came to be used.  This statement should be executed just prior to any such text, in order to clear the traces.	Smalltalk at: #MDFaultDict put: Dictionary new."! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:24'!
                              classComment: aString stamp: aStamp	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."	| ptr header file oldCommentRemoteStr |	(aString isKindOf: RemoteString) ifTrue:		[SystemChangeNotifier uniqueInstance classCommented: self.		^ self organization classComment: aString stamp: aStamp].	oldCommentRemoteStr _ self organization commentRemoteStr.	(aString size = 0) & (oldCommentRemoteStr == nil) ifTrue: [^ self organization classComment: nil].		"never had a class comment, no need to write empty string out"	ptr _ oldCommentRemoteStr ifNil: [0] ifNotNil: [oldCommentRemoteStr sourcePointer].	SourceFiles ifNotNil: [(file _ SourceFiles at: 2) ifNotNil:		[file setToEnd; newLine; nextPut: $!!.	"directly"		"Should be saying (file command: 'H3') for HTML, but ignoring it here"		header _ String streamContents: [:strm | strm nextPutAll: self name;			nextPutAll: ' commentStamp: '.			aStamp storeOn: strm.			strm nextPutAll: ' prior: '; nextPutAll: ptr printString].		file nextChunkPut: header]].	self organization classComment: (RemoteString newString: aString onFileNumber: 2) stamp: aStamp.	SystemChangeNotifier uniqueInstance classCommented: self.! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:25'!
                  definition	"Answer a String that defines the receiver."	| aStream path |	aStream _ WriteStream on: (String new: 300).	superclass		ifNil: [aStream nextPutAll: 'ProtoObject']		ifNotNil: [			path _ ''.				Smalltalk 					scopeFor: superclass name 					from: nil					envtAndPathIfFound: [:envt :remotePath | path _ remotePath].				aStream nextPutAll: path , superclass name].	aStream		nextPutAll: self kindOfSubclass;		store: self name.	aStream		newLine;		tab;		nextPutAll: 'instanceVariableNames: ';		store: self instanceVariablesString.	aStream		newLine;		tab;		nextPutAll: 'classVariableNames: ';		store: self classVariablesString.	aStream		newLine;		tab;		nextPutAll: 'poolDictionaries: ';		store: self sharedPoolsString.	aStream		newLine;		tab;		nextPutAll: 'category: ';		store: (SystemOrganization categoryOfElement: self name) asString.	superclass ifNil: [ 		aStream nextPutAll: '.'; newLine.		aStream nextPutAll: self name.		aStream space; nextPutAll: 'superclass: nil'. ].	^ aStream contents! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:25'!
   fileOutCategory: aSymbol on: aFileStream moveSource: moveSource toFile: fileIndex 	"File a description of the receiver's category, aString, onto aFileStream. If 	moveSource, is true, then set the method source pointer to the new file position.	Note when this method is called with moveSource=true, it is condensing the	.sources file, and should only write one preamble per method category."	| selectors |	aFileStream newLine.	selectors _ (aSymbol == ClassOrganizer allCategory)				ifTrue: [ self organization allMethodSelectors ]				ifFalse: [ self organization listAtCategoryNamed: aSymbol ].	"Overridden to preserve author stamps in sources file regardless"	selectors do: [:sel |		self printMethodChunk: sel 			withPreamble: true			on: aFileStream 			moveSource: moveSource 			toFile: fileIndex].	^ self! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:25'!
                         fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex	"File a description of the receiver on aFileStream. If the boolean 	argument, moveSource, is true, then set the trailing bytes to the position 	of aFileStream and to fileIndex in order to indicate where to find the 	source code."	aFileStream nextPut: $!!; nextChunkPut: self definitionPreamble; newLine.	aFileStream nextChunkPut: self definition.	self organization		putCommentOnFile: aFileStream		numbered: fileIndex		moveSource: moveSource		forClass: self.	self organization categories do: [ :heading |		self fileOutCategory: heading			on: aFileStream			moveSource: moveSource			toFile: fileIndex]! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:25'!
          fileOutOrganizationOn: aFileStream	"File a description of the receiver's organization on aFileStream."	aFileStream newLine; nextPut: $!!.	aFileStream nextChunkPut: self name, ' reorganize'; newLine.	aFileStream nextChunkPut: self organization printString; newLine! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:25'!
      printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod 	"Print a method category preamble.  This must have a category name.	It may have an author/date stamp, and it may have a prior source link.	If it has a prior source link, it MUST have a stamp, even if it is empty.""The current design is that changeStamps and prior source links are preserved in the changes file.  All fileOuts include changeStamps.  Condensing sources, however, eliminates all stamps (and links, natch)."	aFileStream newLine; nextPut: $!!.	aFileStream nextChunkPut: (String streamContents: [ :strm |		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.		(changeStamp notNil and: [			changeStamp size > 0 or: [priorMethod notNil]]) ifTrue: [			strm nextPutAll: ' stamp: '; print: changeStamp].		priorMethod notNil ifTrue: [			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer]]).! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:31'!
 printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex	"Copy the source code for the method associated with selector onto the fileStream.  If moveSource true, then also set the source code pointer of the method."	| preamble method oldPos newPos sourceFile endPos |	doPreamble 		ifTrue: [preamble _ self name , ' methodsFor: ' ,					(self organization categoryOfElement: selector) asString printString]		ifFalse: [preamble _ ''].	method _ self methodDict at: selector ifAbsent: [		outStream nextPutAll: selector; newLine.		outStream tab; nextPutAll: '** ERROR!!  THIS SCRIPT IS MISSING ** '; newLine; newLine.		outStream nextPutAll: '  '.		^ outStream].	((method fileIndex = 0		or: [(SourceFiles at: method fileIndex) == nil])		or: [(oldPos _ method filePosition) = 0])	ifTrue: [		"The source code is not accessible.  We must decompile..."		preamble size > 0 ifTrue: [ outStream newLine; nextPut: $!!; nextChunkPut: preamble; newLine].		outStream nextChunkPut: method decompileString]	ifFalse: [		sourceFile _ SourceFiles at: method fileIndex.		preamble size > 0			ifTrue:    "Copy the preamble"				[outStream copyPreamble: preamble from: sourceFile at: oldPos]			ifFalse:				[sourceFile position: oldPos].		"Copy the method chunk"		outStream padTo: SourceFiles pointerScaleForWriting put: $ .		newPos _ outStream position.		outStream copyMethodChunkFrom: sourceFile.		sourceFile skipSeparators.      "The following chunk may have ]style["		sourceFile peek == $] ifTrue: [			outStream newLine; copyMethodChunkFrom: sourceFile].		moveSource ifTrue: [    "Set the new method source pointer"			endPos _ outStream position.			method checkOKToAdd: endPos - newPos at: newPos in: method fileIndex.			method setSourcePosition: newPos inFile: fileIndex]].	preamble size > 0 ifTrue: [outStream nextChunkPut: ' '].	^ outStream newLine! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:26'!
                        putClassCommentToCondensedChangesFile: aFileStream	"Called when condensing changes.  If the receiver has a class comment, and if that class comment does not reside in the .sources file, then write it to the given filestream, with the resulting RemoteString being reachable from the source file #2.  Note that any existing backpointer into the .sources file is lost by this process -- a situation that maybe should be fixed someday."	| header aCommentRemoteStr aStamp |	self isMeta ifTrue: [^ self].  "bulletproofing only"	((aCommentRemoteStr _ self organization commentRemoteStr) isNil or:		[aCommentRemoteStr sourceFileNumber = 1]) ifTrue: [^ self].	aFileStream newLine; nextPut: $!!.	header _ String streamContents: [:strm | 		strm 			nextPutAll: self name;			nextPutAll: ' commentStamp: '.		(aStamp _ self organization commentStamp ifNil: ['<historical>']) storeOn: strm.		strm nextPutAll: ' prior: 0'].	aFileStream nextChunkPut: header.	aFileStream newLine.	self organization classComment: (RemoteString newString: self organization classComment onFileNumber: 2 toFile: aFileStream) stamp: aStamp! !

!ClassDescription methodsFor: 'private' stamp: 'jmv 3/13/2012 22:51'!
                        linesOfCode

"
Object linesOfCode 1217
"
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."

	| lines |
	lines _ 0.
	self selectorsDo: [ :sel | | code strm |
		code _ self sourceCodeAt: sel.
		strm _ ReadStream on: code.
		[strm atEnd] whileFalse: [ | line |
			line _ strm crLfNextLine.
			line isEmpty ifFalse: [lines _ lines+1]]].
	^self isMeta
		ifTrue: [ lines]
		ifFalse: [ lines + self class linesOfCode]
"
(SystemOrganization categories select: [:c | 'Fabrik*' match: c]) detectSum:
		[:c | (SystemOrganization superclassOrder: c) detectSum: [:cl | cl linesOfCode]] 24878
"! !

!ClassDescription methodsFor: 'accessing class hierarchy' stamp: 'jmv 3/13/2012 15:31'!
                       printSubclassesOn: aStream level: level	"As part of the algorithm for printing a description of the receiver, print the	subclass on the file stream, aStream, indenting level times."	| subclassNames |	aStream newLineTab: level.	aStream nextPutAll: self name.	aStream		 space;		 print: self instVarNames.	self == Class ifTrue: [		aStream			 newLineTab: level + 1;			 nextPutAll: '[ ... all the Metaclasses ... ]'.		^ self ].	subclassNames _ self subclasses asArray sort: [ :c1 :c2 |		c1 name <= c2 name ].	"Print subclasses in alphabetical order"	subclassNames do: [ :subclass |		subclass			printSubclassesOn: aStream			level: level + 1 ].! !


!Class methodsFor: 'initialize-release' stamp: 'jmv 3/13/2012 23:13'!
   sharing: poolString 
	"Set up sharedPools. Answer whether recompilation is advisable."
	| oldPools |
	oldPools _ self sharedPools.
	sharedPools _ OrderedCollection new.
	(Smalltalk actualScannerClass new scanFieldNames: poolString) do: 
		[:poolName | 
		sharedPools add: (Smalltalk at: poolName asSymbol ifAbsent:[
			(self confirm: 'The pool dictionary ', poolName,' does not exist.',
						'\Do you want it automatically created?' withNewLines)
				ifTrue:[Smalltalk at: poolName asSymbol put: Dictionary new]
				ifFalse:[^self error: poolName,' does not exist']])].
	sharedPools isEmpty ifTrue: [sharedPools _ nil].
	^oldPools anySatisfy: [ :pool |
		self sharedPools noneSatisfy: [ :p | p == pool ]]! !

!Class methodsFor: 'class variables' stamp: 'jmv 3/13/2012 23:13'!
                       removeClassVarName: aString 
	"Remove the class variable whose name is the argument, aString, from 
	the names defined in the receiver, a class. Create an error notification if 
	aString is not a class variable or if it is still being used in the code of 
	the class."

	| aSymbol |
	aSymbol _ aString asSymbol.
	(classPool includesKey: aSymbol)
		ifFalse: [ ^self error: aString, ' is not a class variable'].
	self withAllSubclasses do:[:subclass |
		(Array with: subclass with: subclass class) do: [ :classOrMeta |
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
	classPool isEmpty ifTrue: [ classPool _ nil ]! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:22'!
    fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool	"File a description of the receiver on aFileStream. If the boolean argument,	moveSource, is true, then set the trailing bytes to the position of aFileStream and	to fileIndex in order to indicate where to find the source code."	Transcript newLine; show: name.	super		fileOutOn: aFileStream		moveSource: moveSource		toFile: fileIndex.	self class nonTrivial		ifTrue: [			aFileStream newLine; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!!'; newLine; newLine.			self class				fileOutOn: aFileStream				moveSource: moveSource				toFile: fileIndex				initializing: aBool]! !

!Class methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:30'!
          fileOutPool: aPool onFileStream: aFileStream 	| aPoolName |	(aPool  isKindOf: SharedPool class) ifTrue:[^self notify: 'we do not fileout SharedPool type shared pools for now'].	aPoolName _ Smalltalk keyAtIdentityValue: aPool.	Transcript newLine; show: aPoolName.	aFileStream nextPutAll: 'Transcript show: ''' , aPoolName , '''; cr!!'; newLine.	aFileStream nextPutAll: 'Smalltalk at: #' , aPoolName , ' put: Dictionary new!!'; newLine.	aPool keys sort do: [ :aKey | | aValue |		aValue _ aPool at: aKey.		aFileStream nextPutAll: aPoolName , ' at: #''' , aKey asString , '''', ' put:  '.		(aValue isKindOf: Number)			ifTrue: [aValue printOn: aFileStream]			ifFalse: [aFileStream nextPutAll: '('.					aValue printOn: aFileStream.					aFileStream nextPutAll: ')'].		aFileStream nextPutAll: '!!'; newLine].	aFileStream newLine! !


!AbstractSound class methodsFor: 'utilities' stamp: 'jmv 3/13/2012 12:13'!
          pitchTable	"AbstractSound pitchTable"	| out i |	out := WriteStream on: (String new: 1000).	i := 12.	0 to: 8 do: [:octave |		#(c 'c#' d eb e f fs g 'g#' a bf b) do: [:noteName | | note |			note := noteName, octave printString.			out nextPutAll: note; tab.			out nextPutAll: i printString; tab.			out nextPutAll: (AbstractSound pitchForName: note) printString; newLine.			i := i + 1]].	^ out contents! !


!BreakpointManager class methodsFor: 'examples' stamp: 'jmv 3/13/2012 12:16'!
                testBreakpoint	"In the menu of the methodList, click on -toggle break on entry-	and evaluate the following:"	"BreakpointManager testBreakpoint"	Transcript newLine; show: 'Breakpoint test'! !


!CharacterScanner class methodsFor: 'class initialization' stamp: 'jmv 3/14/2012 09:13'!
initialize
	"
	CharacterScanner initialize
	"
	| stopConditions |
	stopConditions _ Array new: 258.
	stopConditions atAllPut: nil.
	stopConditions at: Character space asciiValue + 1 put: nil.
	stopConditions at: Character tab asciiValue + 1 put: #tab.

	"This line makes lf be shown as a newLine"
	stopConditions at: Character lfCharacter asciiValue + 1 put: #doNewLine.

	"This line makes cr be shown as a newLine"
	stopConditions at: Character crCharacter asciiValue + 1 put: #doNewLine.

	stopConditions at: CharacterScanner endOfRunCode put: #endOfRun.
	stopConditions at: CharacterScanner crossedXCode put: #crossedX.
	DefaultStopConditions _ stopConditions.! !


!ClassBuilder class methodsFor: 'cleanup obsolete classes' stamp: 'jmv 3/13/2012 12:23'!
          checkClassHierarchyConsistency: informer	"Check the consistency of the class hierarchy. The class hierarchy is consistent if the following	two logical equivalences hold for classes A and B:	- B is obsolete and 'B superclass' yields A  <-->  'A obsoleteSubclasses' contains B	- B is not obsolete and 'B superclass' yields A  <-->  'A subclasses' contains B"	| classes |	Transcript newLine; show: 'Start checking the class hierarchy...'.	Smalltalk garbageCollect.	classes := Metaclass allInstances.	classes keysAndValuesDo: [:index :meta |		informer value:'Validating class hierarchy ', (index * 100 // classes size) printString,'%'.		meta allInstances do: [:each | self checkClassHierarchyConsistencyFor: each].		self checkClassHierarchyConsistencyFor: meta.	].	Transcript show: 'OK'.! !

!ClassBuilder class methodsFor: 'cleanup obsolete classes' stamp: 'jmv 3/13/2012 12:23'!
     cleanupAndCheckClassHierarchy: informer	"Makes the class hierarchy consistent and removes obsolete classes from the SystemDictionary.	Afterwards it checks whether the hierarchy is really consistent."	Transcript newLine; show: '*** Before cleaning up ***'.	self countReallyObsoleteClassesAndMetaclasses.	self cleanupClassHierarchy: informer.	self checkClassHierarchyConsistency: informer.	Transcript newLine; newLine; show: '*** After cleaning up ***'.	self countReallyObsoleteClassesAndMetaclasses.! !

!ClassBuilder class methodsFor: 'cleanup obsolete classes' stamp: 'jmv 3/13/2012 12:23'!
       cleanupClassHierarchy: informer	"Makes the class hierarchy consistent and removes obsolete classes from the SystemDictionary."	| classes |	Transcript newLine; show: 'Start fixing the class hierarchy and cleaning up...'.	Smalltalk garbageCollect.	classes := Metaclass allInstances.	classes keysAndValuesDo: [:index :meta |		informer value:'Fixing  class hierarchy ', (index * 100 // classes size) printString,'%'.		"Check classes before metaclasses (because Metaclass>>isObsolete		checks whether the related class is obsolete)"		meta allInstances do: [:each | self cleanupClassHierarchyFor: each].		self cleanupClassHierarchyFor: meta.	].	Transcript show: 'DONE'.! !

!ClassBuilder class methodsFor: 'cleanup obsolete classes' stamp: 'jmv 3/13/2012 12:23'!
                               countReallyObsoleteClassesAndMetaclasses	"Counting really obsolete classes and metaclasses"	| metaSize classSize |	Smalltalk garbageCollect.	metaSize _ self reallyObsoleteMetaclasses size.	Transcript newLine; show: 'Really obsolete metaclasses: ', metaSize printString.	classSize _ self reallyObsoleteClasses size.	Transcript newLine; show: 'Really obsolete classes: ', classSize printString; newLine.	"Metaclasses must correspond to classes!!"	metaSize ~= classSize 		ifTrue: [self error: 'Serious metalevel inconsistency!!!!'].! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 3/13/2012 21:55'!
                             chooseRecentClipping
	"
	Clipboard chooseRecentClipping
	"
	"Choose by menu from among the recent clippings"
	recent isEmpty ifTrue: [ ^ nil ].
	^ (SelectionMenu
		labelList:
			(recent collect: [ :txt |
				((txt asString contractTo: 50)
					withLineEndings: '\') withBlanksCondensed ])
		selections: recent) startUp! !

!Clipboard methodsFor: 'accessing' stamp: 'jmv 3/13/2012 11:54'!
                            storeObject: anObject	"Set new contents on the clipboard.  Also export to OS.	anObject can be a:		String		Text		Form		Morph		Object.	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."	| primitiveFormat id |	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.	Another copy must be made again when pasting, as the same object could be pasted many times."	contents _ anObject copyForClipboard.	self noteRecentClipping: contents.	"Store on OS clipboard using ExtendedClipboardInterface if present"	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface | | interface |		interface _ clipboardInterface current.		interface canStore ifTrue: [			id _ self idFor: contents.			contents isString				ifTrue: [ ^interface storeString: contents id: id ].			(contents is: #Text)				ifTrue: [ ^interface storeText: contents id: id ].			(contents is: #Form)				ifTrue: [ ^interface storeForm: contents id: id ].			(contents is: #Morph)				ifTrue: [ ^interface storeForm: (contents imageForm: 32) id: id ].			^interface storeString: contents asString id: id ]].	"Otherwise use the clipboard primitives in the VM"	"The VM uses UTF-8 for clipboard"	primitiveFormat _ ((self stringOrIdFor: contents) withLineEndings: String lfString) iso8859s15ToUtf8.	self primitiveClipboardString: primitiveFormat! !

!Clipboard methodsFor: 'private' stamp: 'jmv 3/13/2012 16:22'!
                         retrieveIdOrStringFromOS	"Use a specific content type if ExtendedClipboard is active.	Otherwise, use regular clipboard primitives"	| primitiveFormat interface |	Smalltalk at: #ExtendedClipboardInterface ifPresent: [ :clipboardInterface |		interface _ clipboardInterface current.		interface canStore ifTrue: [			"Answer nil if no id was stored"			^ clipboardInterface current retrieveId ]].			primitiveFormat _ self primitiveClipboardString.	"Clipboard primitives answer an empty string if there is no string in OS clipboard.	We prefer nil"	primitiveFormat isEmpty ifTrue: [ ^nil ].	"The VM uses UTF-8 for clipboard"	^primitiveFormat utf8ToISO8859s15 withCuisLineEndings! !


!CodeFile methodsFor: 'accessing' stamp: 'jmv 3/13/2012 12:26'!
           description	^String streamContents:[:s|		s nextPutAll: 'CodeFile: '.		s nextPutAll: self fullName; newLine; newLine.		sourceSystem isEmpty ifFalse:[			s nextPutAll: sourceSystem; newLine; newLine ].		doIts isEmpty ifFalse:[			s nextPutAll: 'Unresolvable doIts:'; newLine; newLine.			doIts do: [ :chgRec |				s					nextPut:$!!;					nextPutAll: chgRec string;					nextPut: $!!;					newLine ]]]! !

!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/13/2012 12:26'!
  fileIn	| doitsMark |	doitsMark := 1.	doIts isEmpty ifFalse:[doitsMark := self askForDoits].	doitsMark = 4 ifTrue: [^nil].	doitsMark = 2 ifTrue:[self fileInDoits].	classOrder do:[:cls|		cls fileInDefinition.	].	classes do:[:cls|		Transcript newLine; show:'Filing in ', cls name.		cls fileInMethods.		cls hasMetaclass ifTrue:[cls metaClass fileInMethods].	].	doitsMark = 3 ifTrue: [ self fileInDoits ]! !

!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/13/2012 12:31'!
                         fileOut	| fileName stream |	fileName := FillInTheBlank request: 'Enter the file name' initialAnswer:''.	stream := FileStream newFileNamed: fileName.	sourceSystem isEmpty ifFalse:[		stream nextChunkPut: sourceSystem printString; newLine ].	self fileOutOn: stream.	stream newLine; newLine.	self classes do: [ :cls |		cls needsInitialize ifTrue: [			stream newLine; nextChunkPut: cls name,' initialize']].	stream newLine.	stream close! !


!CodeProvider methodsFor: 'contents' stamp: 'jmv 3/13/2012 11:42'!
                            commentContents	"documentation for the selected method"	| poss aClass aSelector |	^ (poss _ (aClass _ self selectedClassOrMetaClass)						ifNil:							['----']						ifNotNil:							[(aSelector _ self selectedMessageName)								ifNil:									['---']								ifNotNil:									[(aClass precodeCommentOrInheritedCommentFor: aSelector)", String crString, String crString, self timeStamp""which however misses comments that are between the temps  declaration and the body of the method; those are picked up by [aClass commentOrInheritedCommentFor: aSelector] but that method will get false positives from comments *anywhere* in the method source"]])		isEmptyOrNil			ifTrue:				[aSelector					ifNotNil:						[((aClass methodHeaderFor: aSelector), 'Has no comment') asText makeSelectorBoldIn: aClass]					ifNil:						['Hamna']]			ifFalse:	[aSelector				ifNotNil: [((aClass methodHeaderFor: aSelector), '', poss) asText makeSelectorBoldIn: aClass]				ifNil: [poss]]! !


!ChangeList methodsFor: 'initialization-release' stamp: 'jmv 3/13/2012 22:48'!
                  addItem: item text: text

	changeList addLast: item.
	list addLast: (text collect: [ :x | x isLineSeparator ifTrue: [$/] ifFalse: [x]])! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 3/13/2012 22:44'!
                   scanFile: aFile from: startPosition to: stopPosition

	file _ aFile.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	file position: startPosition.
	'Scanning ', aFile localName, '...'
		displayProgressAt: Sensor mousePoint
		from: startPosition to: stopPosition
		during: [:bar |
			[file position < stopPosition] whileTrue: [ | prevChar |
				bar value: file position.
				[file atEnd not and: [file peek isSeparator]]
					whileTrue: [prevChar _ file next].
				(file peekFor: $!!)
					ifTrue: [
						prevChar isLineSeparator
							ifTrue: [self scanCategory]]
					ifFalse: [
						| itemPosition item |
						itemPosition _ file position.
						item _ file nextChunk.
						file skipStyleChunk.
						item size > 0 ifTrue: [
							self addItem: (ChangeRecord new file: file position: itemPosition type: #doIt)
								text: 'do it: ' , (item contractTo: 160)]]]].
	self clearSelections! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 3/13/2012 12:17'!
                    destroyCurrentCodeOfSelections	"Actually remove from the system any in-memory methods with class and selector identical to items current selected.  This may seem rather arcane but believe me it has its great uses, when trying to split out code.  To use effectively, first file out a change set that you wish to split off.  Then open a ChangeList browser on that fileout.  Now look through the methods, and select any of them which you want to remove completely from the system, then issue this command.  For those methods where you have made changes to pre-existing versions, of course, you won't want to remove them from the system, so use this mechanism with care!!"	|  aClass aChange aList |	aList _ OrderedCollection new.	1 to: changeList size do:		[:index |			(listSelections at: index) ifTrue:				[aChange _ changeList at: index.				(aChange type == #method					and: [(aClass _ aChange methodClass) notNil					and: [aClass includesSelector: aChange methodSelector]])						ifTrue:							[aList add: {aClass. aChange methodSelector}]]].	aList size > 0 ifTrue: [		(self confirm: 'Warning!! This will actually remove ', aList size printString,  ' method(s) from the system!!') ifFalse: [^ self]].	aList do: [ :aPair |		Transcript newLine; show: 'Removed: ', aPair first printString, '.', aPair second.		aPair first removeSelector: aPair second ]! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 3/13/2012 12:18'!
selectConflictsWith	"Selects all method definitions for which there is ALSO an entry in the specified changeSet or changList chosen by the user. 4/11/96 tk"	| aStream all index |	aStream _ WriteStream on: (String new: 200).	(all _ ChangeSorter allChangeSets copy) do: [ :sel |		aStream nextPutAll: (sel name contractTo: 40); newLine].	ChangeList allSubInstancesDo: [ :sel |		aStream nextPutAll: (sel file name); newLine.		all addLast: sel].	aStream skip: -1.	index _ (PopUpMenu labels: aStream contents) startUp.	index > 0 ifTrue: [		self selectConflicts: (all at: index)].! !

!ChangeList methodsFor: 'menu actions' stamp: 'jmv 3/13/2012 16:08'!
            selectUnchangedMethods	"Selects all method definitions for which there is already a method in the current image, whose source is exactly the same.  9/18/96 sw"	Cursor read showWhile: [		| change class |		1 to: changeList size do: [ :i | 			change _ changeList at: i.			listSelections at: i put:				((change type == #method and:					[(class _ change methodClass) notNil]) and:						[(class includesSelector: change methodSelector) and:							[change string = (class sourceCodeAt: change methodSelector) asString ]])]].	self changed: #allSelections! !


!ChangeSorter methodsFor: 'code pane' stamp: 'jmv 3/13/2012 12:21'!
        acceptedStringOrText	"return the source code that shows in the bottom pane"	| sel class strm changeType answer |	self changed: #clearUserEdits.	currentClassName ifNil: [^ myChangeSet preambleString ifNil: ['']].	class _ self selectedClassOrMetaClass.	(sel _ currentSelector)		ifNotNil: [			changeType _ (myChangeSet atSelector: (sel _ sel asSymbol) class: class).			changeType == #remove				ifTrue: [^'Method has been removed (see versions)'].			changeType == #addedThenRemoved				ifTrue: [^'Added then removed (see versions)'].			class ifNil: [^'Method was added, but cannot be found!!'].			(class includesSelector: sel)				ifFalse: [^'Method was added, but cannot be found!!'].			answer _  (class sourceCodeAt: sel).			(#(prettyPrint prettyLineDiffs prettyWordDiffs) includes: contentsSymbol) ifTrue: [				answer _ (class compilerClass new						format: answer						in: class 						notifying: nil)].			self showingAnyKindOfDiffs				ifTrue: [ answer _ (self diffFromPriorSourceFor: answer) ].			^ answer asText makeSelectorBoldIn: class ]		ifNil: [			strm _ WriteStream on: (String new: 100).			(myChangeSet classChangeAt: currentClassName) do: [ :each |				each == #remove ifTrue: [strm nextPutAll: 'Entire class was removed.'; newLine].				each == #addedThenRemoved ifTrue: [strm nextPutAll: 'Class was added then removed.'; newLine].				each == #rename ifTrue: [strm nextPutAll: 'Class name was changed.'; newLine].				each == #add ifTrue: [strm nextPutAll: 'Class definition was added.'; newLine].				each == #change ifTrue: [strm nextPutAll: 'Class definition was changed.'; newLine].				each == #reorganize ifTrue: [strm nextPutAll: 'Class organization was changed.'; newLine].				each == #comment ifTrue: [strm nextPutAll: 'New class comment.'; newLine.				]].			^ strm contents].! !


!ChangeSorter class methodsFor: 'adding' stamp: 'jmv 3/13/2012 12:22'!
                   newChangesFromStream: aStream named: aName	"File in the code from the stream into a new change set whose	name is derived from aName. Leave the 'current change set'	unchanged. Return the new change set or nil on failure."	| oldChanges newName newSet |	oldChanges _ ChangeSet current.	PreviousSet _ oldChanges name. 		"so a Bumper update can find it"	newName _ (aName prefixAndSuffix: $-)		ifNotNil: [ :ary | ary first ]		ifNil: [ aName sansPeriodSuffix ].	newSet _ self basicNewChangeSet: newName.	[		newSet ifNotNil: [			ChangeSet newChanges: newSet.			aStream fileInAnnouncing: 'Loading ', newName, '...'.			Transcript show: 'File ', aName, ' successfully filed in to change set ', newName; newLine].		aStream close	] ensure: [		ChangeSet  newChanges: oldChanges].	^ newSet! !


!Color class methodsFor: 'class initialization' stamp: 'jmv 3/14/2012 08:54'!
                  named: newName put: aColor
	"Add a new color to the list and create an access message and a class variable for it.  The name should start with a lowercase letter.  (The class variable will start with an uppercase letter.)  (Color colorNames) returns a list of all color names.  "
	| str cap sym accessor csym |
	(aColor isKindOf: self) ifFalse: [^ self error: 'not a Color'].
	str _ newName asString.
	sym _ str asSymbol.
	cap _ str capitalized.
	csym _ cap asSymbol.
	(self class canUnderstand: sym) ifFalse: [
		"define access message"
		accessor _ str, String newLineString, String tab, '^', cap.
		self class compile: accessor
			classified: 'named colors'].
	(self classPool includesKey: csym) ifFalse: [
		self addClassVarName: cap].
	(ColorNames includes: sym) ifFalse: [
		ColorNames add: sym].
	^ self classPool at: csym put: aColor! !


!Compiler class methodsFor: 'utilities' stamp: 'jmv 3/13/2012 12:28'!
           recompileAllFrom: firstName 	"Recompile all classes, starting with given name."	Smalltalk allClassesDo: [ :class | 		class name >= firstName			ifTrue: [				Transcript show: class name; newLine.				class compileAll ]]	"Compiler recompileAllFrom: 'AAABodyShop'."! !


!CompositionScanner methodsFor: 'scanning' stamp: 'jmv 3/14/2012 09:11'!
   composeFrom: startIndex inRectangle: lineRectangle firstLine: firstLine leftSide: leftSide rightSide: rightSide

	"Answer an instance of TextLineInterval that represents the next line in the paragraph."
	| runLength done stopCondition xtraSpaceBefore spaceAfterParagraph |
	
	lastIndex _ startIndex.	"scanning sets last index"
	destY _ lineRectangle top.
	lineHeight _ baseline _ 0.  "Will be increased by setFont"
	self setStopConditions.	"also sets font, style, etc"

	"Set up margins"
	leftMargin _ lineRectangle left.
	rightMargin _ lineRectangle right.
	xtraSpaceBefore _ 0.
	spaceAfterParagraph _ 0.
	paragraphStyle ifNotNil: [
		leftSide ifTrue: [
			leftMargin _ leftMargin +
				((firstLine and: [ paragraphStyle isListStyle not ])
					ifTrue: [ paragraphStyle firstIndent ]
					ifFalse: [ paragraphStyle restIndent ])].
		rightSide ifTrue: [
			rightMargin _ rightMargin - paragraphStyle rightIndent].
		firstLine ifTrue: [ xtraSpaceBefore _ paragraphStyle spaceBefore ].
		spaceAfterParagraph _ paragraphStyle spaceAfter ].
	destX _ spaceX _ leftMargin.

	runLength _ text runLengthFor: startIndex.
	runStopIndex _ (lastIndex _ startIndex) + (runLength - 1).
	line _ (TextLine start: lastIndex stop: 0 internalSpaces: 0 paddingWidth: 0)
				rectangle: lineRectangle.
	line isFirstLine: firstLine.
	spaceCount _ 0.
	leftMargin _ destX.
	line leftMargin: leftMargin.

	done _ false.
	self placeEmbeddedObject.
	[ done ]
		whileFalse: [
			stopCondition _ self scanCharactersFrom: lastIndex to: runStopIndex
				in: text string rightX: rightMargin stopConditions: stopConditions
				kern: kern.
			"See setStopConditions for stopping conditions for composing."
			(self perform: stopCondition) ifTrue: [
				^ line 
					lineHeight: lineHeight + xtraSpaceBefore + 
						(stopCondition == #doNewLine ifTrue: [spaceAfterParagraph] ifFalse: [0]) 
					baseline: baseline + xtraSpaceBefore ]]! !


!ContentPack class methodsFor: 'code pack' stamp: 'jmv 3/13/2012 16:54'!
      compilePayloadWith: contentMap	self compile: 'contentMap' , String newLineString , '	^ ' , contentMap asString.	self compile: 'objectCount ^ ' , payload size asString.	^ payload withIndexDo: [ :blob :index |		self compile:			'object' ,			index asString ,			 String newLineString ,			'	^ ' ,			blob surroundedBySingleQuotes 	]! !


!ContextPart methodsFor: 'debugger access' stamp: 'jmv 3/13/2012 12:36'!
                               errorReportOn: strm	"Write a detailed error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report.  Suppress any errors while getting printStrings.  Limit the length."	| cnt aContext startPos | 	strm print: Date today; space; print: Time now; newLine.	strm newLine.	strm nextPutAll: 'VM: ';		nextPutAll: Smalltalk platformName asString;		nextPutAll: ' - ';		nextPutAll: Smalltalk vmVersion asString;		newLine.	strm nextPutAll: 'Image: ';		nextPutAll: Smalltalk version asString;		nextPutAll: ' [';		nextPutAll: Smalltalk lastUpdateString asString;		nextPutAll: ']';		newLine.	strm newLine.		"Note: The following is an open-coded version of ContextPart>>stackOfSize: since this method may be called during a low space condition and we might run out of space for allocating the full stack."	cnt _ 0.  startPos _ strm position.	aContext _ self.	[aContext notNil and: [(cnt _ cnt + 1) < 20]] whileTrue: [		aContext printDetails: strm.	"variable values"		strm newLine.		aContext _ aContext sender].	strm newLine; nextPutAll: '--- The full stack ---'; newLine.	aContext _ self.	cnt _ 0.	[aContext == nil] whileFalse: [		cnt _ cnt + 1.		cnt = 20 ifTrue: [strm nextPutAll: ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'; newLine ].		strm print: aContext; newLine.  "just class>>selector"			strm position > (startPos+40000) ifTrue: [strm nextPutAll: '...etc...'.			^ self]. 	"exit early"		cnt > 60 ifTrue: [strm nextPutAll: '-- and more not shown --'.  ^ self].		aContext _ aContext sender].! !

!ContextPart methodsFor: 'debugger access' stamp: 'jmv 3/13/2012 12:29'!
 longStack	"Answer a String showing the top 100 contexts on my sender chain."	^ String streamContents: [ :strm |		(self stackOfSize: 100)			do: [:item | strm print: item; newLine]]! !

!ContextPart methodsFor: 'debugger access' stamp: 'jmv 3/13/2012 12:30'!
                          shortStack	"Answer a String showing the top ten contexts on my sender chain."	^ String streamContents: [ :strm |		(self stackOfSize: 10)			do: [:item | strm print: item; newLine]]! !

!ContextPart methodsFor: 'debugger access' stamp: 'jmv 3/13/2012 12:37'!
                          tempsAndValuesLimitedTo: sizeLimit indent: indent	"Return a string of the temporary variabls and their current values"	| aStream |	aStream _ WriteStream on: (String new: 100).	self tempNames		doWithIndex: [:title :index |			indent timesRepeat: [aStream tab].			aStream nextPutAll: title; nextPut: $:; space; tab.			aStream nextPutAll: 				((self tempAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1)).			aStream newLine].	^aStream contents! !

!ContextPart methodsFor: 'printing' stamp: 'jmv 3/13/2012 22:35'!
     printDetails: strm
	"Put my class>>selector and arguments and temporaries on the stream.  Protect against errors during printing."

	| str |
	self printOn: strm.  

	strm newLine; tab; nextPutAll: 'Arguments and temporary variables: '; newLine.
	str := [self tempsAndValuesLimitedTo: 160 indent: 2] ifError: [:err :rcvr | 
						'<<error during printing>>'].
	strm nextPutAll: str.
	strm peekLast isLineSeparator ifFalse: [strm newLine]! !


!BlockContext methodsFor: 'evaluating' stamp: 'jmv 3/13/2012 23:02'!
 valueSupplyingAnswers: aListOfPairs
	"evaluate the block using a list of questions / answers that might be called upon to
	automatically respond to Object>>confirm: or FillInTheBlank requests"

	^self
		on: ProvideAnswerNotification
		do: [ :notification |
			| caption |
			caption _ notification messageText withBlanksCondensed. "to remove new lines"
			aListOfPairs
				detect:  [ :each |
					caption = each first
						or: [ (caption includesSubstring: each first caseSensitive: false)
						or: [ each first match: caption ] ] ]
				ifFound: [ :answer | notification resume: answer second ]
				ifNone: [
					(ProvideAnswerNotification signal: notification messageText)
						ifNil: [ notification resume ]
						ifNotNil: [ :outerAnswer | notification resume: outerAnswer ] ] ]! !

!BlockContext methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:16'!
       fullPrintOn: aStream	aStream print: self; newLine.	(self decompile ifNil: ['--source missing--']) printOn: aStream indent: 0! !

!BlockContext methodsFor: 'printing' stamp: 'jmv 3/13/2012 16:10'!
                          printOn: aStream	| decompilation blockString truncatedBlockString |	home == nil ifTrue: [^aStream nextPutAll: 'a BlockContext with home=nil'].	aStream nextPutAll: '[] in '.	super printOn: aStream.	decompilation := [self decompile ifNil: ['--source missing--']]						on: Error						do: [:ex| ' (error in decompilation)'].	blockString := (decompilation isString					ifTrue: [decompilation]					ifFalse: [decompilation printString]) withBlanksCondensed.	truncatedBlockString := blockString truncateWithElipsisTo: 80.	truncatedBlockString size < blockString size ifTrue:		[truncatedBlockString := truncatedBlockString, ']}'].	aStream space; nextPutAll: truncatedBlockString! !


!ContextPart class methodsFor: 'examples' stamp: 'jmv 3/13/2012 12:32'!
      trace: aBlock on: aStream		"ContextPart trace: [3 factorial]"	"This method uses the simulator to print calls to a file."	| prev |	prev _ aBlock.	^ thisContext sender		runSimulated: aBlock		contextAtEachStep: [ :current |			Sensor anyButtonPressed ifTrue: [^ nil].			current == prev				ifFalse: [					prev sender ifNil: [						aStream space; nextPut: $^.						self carefullyPrint: current top on: aStream].					aStream newLine.					(current depthBelow: aBlock) timesRepeat: [aStream space].					self carefullyPrint: current receiver on: aStream.					aStream space; nextPutAll: current selector.					prev _ current]]! !


!Debugger methodsFor: 'accessing' stamp: 'jmv 3/13/2012 23:13'!
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
	selector := self selectedClass parserClass new parseSelector: aText.
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


!Debugger class methodsFor: 'class initialization' stamp: 'jmv 3/14/2012 07:49'!
                          openContext: aContext label: aString contents: contentsStringOrNil	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."	<primitive: 19> "Simulation guard"	ErrorRecursion not & Preferences logDebuggerStackToFile ifTrue:		[Smalltalk logError: aString inContext: aContext to: 'CuisDebug.log'].	ErrorRecursion ifTrue: [		ErrorRecursion _ false.		contentsStringOrNil			ifNil: [				self primitiveError: 'Can not open debugger due to recursion error.', 					String newLineString, aString]			ifNotNil: [				self primitiveError: 'Can not open debugger due to recursion error.', 					String newLineString, aString, String newLineString, contentsStringOrNil ]].	ErrorRecursion _ true.	self informExistingDebugger: aContext label: aString.	(Debugger context: aContext)		openNotifierContents: contentsStringOrNil		label: aString.	ErrorRecursion _ false.	Processor activeProcess suspend.! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 3/13/2012 16:54'!
                         openOn: process context: context label: title contents: contentsStringOrNil fullView: bool	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."	| errorWasInUIProcess |	Preferences logDebuggerStackToFile ifTrue: [		Smalltalk logError: title inContext: context to: 'CuisDebug.log'].	errorWasInUIProcess _ ProjectX newProcessIfUIX: process.	"schedule debugger in deferred UI message to address redraw	problems after opening a debugger e.g. from the testrunner."	WorldState addDeferredUIMessage: [ 		[	| debugger |			debugger _ self new process: process controller: nil context: context.			bool				ifTrue: [debugger openFullNoSuspendLabel: title]				ifFalse: [debugger openNotifierContents: contentsStringOrNil label: title].			debugger errorWasInUIProcess: errorWasInUIProcess.		] on: Error do: [ :ex |			self primitiveError:				'Error while trying to open Debugger', String newLineString,				'Orginal error: ', 				title asString, '.', String newLineString,				'	Debugger error: ', 				([ex description] on: Error do: ['a ', ex class printString]), ':'		]	].	process suspend! !


!DebuggerMethodMap methodsFor: 'accessing' stamp: 'jmv 3/13/2012 12:32'!
        tempsAndValuesForContext: aContext	"Return a string of the temporary variabls and their current values"	| aStream |	aStream := WriteStream on: (String new: 100).	(self tempNamesForContext: aContext) doWithIndex: [ :title :index |		aStream nextPutAll: title; nextPut: $:; space; tab.		aContext print: (self namedTempAt: index in: aContext) on: aStream.		aStream newLine].	^aStream contents! !


!DebuggerMethodMapForClosureCompiledMethods methodsFor: 'private' stamp: 'jmv 3/13/2012 23:13'!
              privateDereference: tempReference in: aContext put: aValue
	"Assign the temporary with reference tempReference in aContext.
	 tempReference can be
		integer - direct temp reference
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index
		#( outer. temp reference ) - a temp reference in an outer context."
	^tempReference isInteger
		ifTrue:
			[aContext tempAt: tempReference put: aValue]
		ifFalse:
			[tempReference first == #outer
				ifTrue:
					[self privateDereference: tempReference last
						in: aContext outerContext
						put: aValue]
				ifFalse: "If stopped before indirection vectors are created they will be nil."
					[(aContext tempAt: tempReference first)
						ifNil: [ self inform: 'Cannot assign remote temp because indirection vector is nil.\Too early in method execution?' withNewLines.
							nil]
						ifNotNil:
							[:indirectionVector|
							indirectionVector
								at: tempReference second
								put: aValue]]]! !


!Decompiler methodsFor: 'private' stamp: 'jmv 3/13/2012 12:33'!
         interpretNextInstructionFor: client	| code varNames |"Change false here will trace all state in Transcript."true ifTrue: [^ super interpretNextInstructionFor: client].	varNames := self class allInstVarNames.	code := (self method at: pc) radix: 16.	Transcript newLine; newLine; print: pc; space;		nextPutAll: '<' , code, '>'.	8 to: varNames size do:		[:i | i <= 10 ifTrue: [Transcript newLine]				ifFalse: [Transcript space; space].		Transcript nextPutAll: (varNames at: i);				nextPutAll: ': '; print: (self instVarAt: i)].	Transcript endEntry.	^ super interpretNextInstructionFor: client! !


!DiskProxy methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 12:33'!
                  comeFullyUpOnReload: smartRefStream	"Internalize myself into a fully alive object after raw loading from a DataStream. (See my class comment.)  DataStream will substitute the object from this eval for the DiskProxy."	| globalObj symbol |	symbol _ globalObjectName.	"See if class is mapped to another name"	(smartRefStream respondsTo: #renamed) ifTrue: [		symbol _ smartRefStream renamed at: symbol ifAbsent: [symbol]].	"map"	globalObj _ Smalltalk at: symbol ifAbsent: [		preSelector == nil & (constructorSelector = #yourself) ifTrue: [			Transcript newLine; show: symbol, ' is undeclared.'.			(Undeclared includesKey: symbol) ifTrue: [^ Undeclared at: symbol].			Undeclared at: symbol put: nil.			^ nil].		^ self error: 'Global "', symbol, '" not found'].	preSelector ifNotNil: [		Symbol hasInterned: preSelector ifTrue: [:selector |			globalObj _ globalObj perform: selector]].	constructorSelector ifNil: [^ globalObj].	Symbol hasInterned: constructorSelector ifTrue: [:selector |		^ globalObj perform: selector				withArguments: constructorArgs].				"args not checked against Renamed"	^ nil 	"was not in proper form"! !


!Editor methodsFor: 'new selection' stamp: 'jmv 3/13/2012 11:43'!
 selectWord	"Select delimited text or word--the result of double-clicking.""	^self selectWordLeftDelimiters: String crString rightDelimiters: String crString"	"Disable 'double click selects paragraph' in non-Smalltalk editors"	^self selectWordLeftDelimiters: '' rightDelimiters: ''! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 3/13/2012 15:20'!
                    enter: aKeyboardEvent	"Enter / return key was pressed"	"Process the various Enter / Return keystrokes"	"Not sure if this is ever called"		^self returnKey: aKeyboardEvent! !

!Editor methodsFor: 'private' stamp: 'jmv 3/13/2012 22:20'!
                 beginningOfNextParagraph: position
	| s |
	s _ self privateCurrentString.
	^ (s
		indexOf: Character newLineCharacter
		startingAt: position
		ifAbsent: [ s size ])
			+ 1! !

!Editor methodsFor: 'private' stamp: 'jmv 3/13/2012 22:20'!
                    beginningOfParagraph: position
	| s |
	s _ self privateCurrentString.
	^ (s
		lastIndexOf: Character newLineCharacter
		startingAt: (position min: s size)
		ifAbsent: [ 0 ])
			+ 1.! !

!Editor methodsFor: 'private' stamp: 'jmv 3/13/2012 22:22'!
          endOfParagraph: position
	| s |
	s _ self privateCurrentString.
	^ s
		indexOf: Character newLineCharacter
		startingAt: position
		ifAbsent: [ s size + 1 ]! !

!Editor methodsFor: 'private' stamp: 'jmv 3/13/2012 22:29'!
   lines
	"Compute lines based on logical line breaks, not optical (which may change due to line wrapping of the editor).
	Subclasses using kinds of Paragraphs can instead use the service provided by it (i.e. optical lines).
	"
	| lines string index lineIndex stringSize |
	string _ self privateCurrentString.
	"Empty strings have no lines at all. Think of something."
	string isEmpty ifTrue:[^{#(1 0 0)}].
	stringSize _ string size.
	lines _ OrderedCollection new: (string size // 15).
	index _ 0.
	lineIndex _ 0.
	string linesDo: [ :line |
		lines addLast: (Array
			with: (index _ index + 1)
			with: (lineIndex _ lineIndex + 1)
			with: (index _ index + line size min: stringSize))].
	"Special workaround for last line empty."
	"lines last last < stringSize"
	string last isLineSeparator ifTrue:[
		lines addLast:{stringSize +1. lineIndex+1. stringSize}].
	^lines! !


!Editor class methodsFor: 'class initialization' stamp: 'jmv 3/13/2012 15:20'!
           initializeKeystrokeActions	"Initialize the table for regular (i.e. non-command) keystroke dispatch"	"	self initializeKeystrokeActions	"	| actions |	actions _ Array new: 256 withAll: #normalCharacter:.	0 to: 31 do: [ :i | actions at: i+1 put: #noop: ].	actions at: 1 + 1 put: #cursorHome:.				"home key"	actions at: 3 + 1 put: #enter:.						"enter / return key"	actions at: 4 + 1 put: #cursorEnd:.				"end key"	actions at: 5 + 1 put: #noop:.						"insert key"	actions at: 8 + 1 put: #backspace:.				"macDelete winBackspace key"	actions at: 9 + 1 put: #normalCharacter:.		"tab"	actions at: 11 + 1 put: #cursorPageUp:.			"page up key"	actions at: 12 + 1 put: #cursorPageDown:.		"page down key"	actions		at:  InputSensor returnKey + 1		put: #returnKey:.									"return (sometimes labelled enter) key"	actions at: 27 + 1 put: #offerMenuFromEsc:.	"escape key"	actions at: 28 + 1 put: #cursorLeft:.				"left arrow key"	actions at: 29 + 1 put: #cursorRight:.				"right arrow key"	actions at: 30 + 1 put: #cursorUp:.				"up arrow key"	actions at: 31 + 1 put: #cursorDown:.			"down arrow key"	actions at: 127 + 1 put: #forwardDelete:.		"winDelete key"	KeystrokeActions _ actions! !


!EventSensor methodsFor: 'test' stamp: 'jmv 3/13/2012 12:37'!
                  printEventBuffer: evtBuf	| type buttons macRomanCode modifiers position pressType stamp unicodeCodePoint |	type _ evtBuf first.	stamp _ evtBuf second.	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ].	type = EventSensor eventTypeMouse		ifTrue: [			position _ evtBuf third @ evtBuf fourth.			buttons _ evtBuf fifth.			modifiers _ evtBuf sixth.			Transcript				newLine;				show: 'Mouse';				show: ' position:', position printString;				show: ' buttons:', buttons printString;				show: ' modifiers:', modifiers printString.			].	type = EventSensor eventTypeKeyboard 		ifTrue: [			macRomanCode _ evtBuf third.			unicodeCodePoint _ evtBuf sixth.			pressType _ evtBuf fourth.			modifiers _ evtBuf fifth.			pressType = EventSensor eventKeyDown ifTrue: [				type _ #keyDown].			pressType = EventSensor eventKeyUp ifTrue: [				type _ #keyUp].			pressType = EventSensor eventKeyChar ifTrue: [				type _ #keystroke].			Transcript				newLine;				show: type;				show: ' macRomanCode:', macRomanCode printString, '-', 					(Character value: (Character macRomanToLatin1: macRomanCode)) asString, '-';				show: ' unicodeCodePoint:', unicodeCodePoint printString.			(Character iso8859s15CodeForUnicodeCodePoint: unicodeCodePoint) ifNotNil: [ :latin15 |				Transcript show: '-', (Character value: latin15) asString, '-' ].			Transcript				show: ' modifiers:', modifiers printString.			(modifiers anyMask: 8) ifTrue: [ Transcript show: ' [commandWinAlt]' ].			(modifiers anyMask: 4) ifTrue: [ Transcript show: ' [macOption]' ].			(modifiers anyMask: 2) ifTrue: [ Transcript show: ' [control]' ].			(modifiers anyMask: 1) ifTrue: [ Transcript show: ' [shift]' ].			].! !


!ExceptionTester methodsFor: 'logging' stamp: 'jmv 3/13/2012 12:33'!
                 contents	^( self log		inject: (WriteStream on: (String new: 80))		into: [ :result :item |			result 				newLine; 				nextPutAll: item;				yourself] ) contents! !


!FFT methodsFor: 'testing' stamp: 'jmv 3/13/2012 12:34'!
                            testFullPrecision	 "	Display restoreAfter: [(FFT new nu: 15) testFullPrecision].	  --  Test on an array of 32768 samples"	"Initialize to pure (co)Sine Wave, plot, transform, plot, invert and plot again.	Allow for full 64bit Double precision, do not use the fast plugin."	self basicRealData: ((1 to: n) collect: [:i | (Float pi * (i-1) / (n/8)) cos]).	self plot: realData in: (100@20 extent: 256@60).	Transcript newLine; print: (Time millisecondsToRun: [ self transformForward: true ]); endEntry.	self plot: realData in: (100@100 extent: 256@60).	self plot: imagData in: (100@180 extent: 256@60).	Transcript newLine; print: (Time millisecondsToRun:[self transformForward: false]); endEntry.	self plot: realData in: (100@260 extent: 256@60)! !

!FFT methodsFor: 'plugin-testing' stamp: 'jmv 3/13/2012 12:34'!
             pluginTest  	"	Display restoreAfter: [(FFT new nu: 15) pluginTest].	"	"Test on an array of 32768 samples"	"Initialize to pure (co)Sine Wave, plot, transform, plot, invert and plot again"	self realData: ((1 to: n) collect: [:i | (Float pi * (i-1) / (n/8)) cos]).	self plot: realData in: (100@20 extent: 256@60).	Transcript newLine; print: (Time millisecondsToRun:[self pluginTransformData: true]); endEntry.	self plot: realData in: (100@100 extent: 256@60).	self plot: imagData in: (100@180 extent: 256@60).	Transcript newLine; print: (Time millisecondsToRun:[self pluginTransformData: false]); endEntry.	self plot: realData in: (100@260 extent: 256@60)! !


!FMSound methodsFor: 'storing' stamp: 'jmv 3/13/2012 12:34'!
     storeOn: strm	| env |	strm nextPutAll: '(((FMSound';		nextPutAll: ' pitch: '; print: self pitch;		nextPutAll: ' dur: '; print: self duration;		nextPutAll: ' loudness: '; print: self loudness; nextPutAll: ')';		nextPutAll: ' modulation: '; print: self modulation;		nextPutAll: ' ratio: '; print: self ratio; nextPutAll: ')'.	1 to: envelopes size do:		[:i | env _ envelopes at: i.		strm newLine; nextPutAll: '    addEnvelope: '. env storeOn: strm.		i < envelopes size ifTrue: [strm nextPutAll: ';']].	strm  nextPutAll: ')'.! !


!FileDirectory class methodsFor: 'system start up' stamp: 'jmv 3/13/2012 16:55'!
                   openSources: sourcesName andChanges: changesName forImage: imageName 	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."	"Note: SourcesName and imageName are full paths; changesName is a  	local name."	| sources changes msg wmsg |	msg _ 'Squeak cannot locate &fileRef.Please check that the file is named properly and is in thesame directory as this image.'.	wmsg _ 'Squeak cannot write to &fileRef.Please check that you have write permission for this file.You won''t be able to save this image correctly until you fix this.'.	"Do not open source files if internalized (i.e. notNil)"	sources _ (SourceFiles at: 1) ifNil: [ self openSources: sourcesName forImage: imageName ].	changes _ (SourceFiles at: 2) ifNil: [ self openChanges: changesName forImage: imageName ].	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])		ifTrue: [			Smalltalk platformName = 'Mac OS'				ifTrue: [					msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].			self inform: (msg copyReplaceAll: '&fileRef' with: 'the sources file named ' , sourcesName)].	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])		ifTrue: [self inform: (msg copyReplaceAll: '&fileRef' with: 'the changes file named ' , changesName)].	((Preferences valueOfFlag: #warnIfNoChangesFile) and: [changes notNil])		ifTrue: [			changes isReadOnly				ifTrue: [					self inform: (wmsg copyReplaceAll: '&fileRef' with: 'the changes file named ' , changesName)].			((changes next: 200)					includesSubString: String crlfString)				ifTrue: [					self inform: 'The changes file named ' , changesName , 'has been injured by an unpacking utility.  Lfs were changed to CrLfs.Please set the preferences in your decompressing program to "do not convert text files" and unpack the system again.']].	SourceFiles _ Array with: sources with: changes! !


!FileDoesNotExistException class methodsFor: 'examples' stamp: 'jmv 3/13/2012 12:34'!
     example	"FileDoesNotExistException example"	| result |	result _ [(StandardFileStream readOnlyFileNamed: 'error42.log') contentsOfEntireFile]		on: FileDoesNotExistException		do: [:ex | 'No error log'].	Transcript show: result; newLine! !


!FileList methodsFor: 'private' stamp: 'jmv 3/13/2012 12:37'!
            defaultContents	acceptedContentsCache _ (list		ifNil: [String new]		ifNotNil: [String streamContents:					[:s | s nextPutAll: 'NO FILE SELECTED'; newLine.					s nextPutAll: '  -- Folder Summary --'; newLine.					list do: [:item | s nextPutAll: item; newLine]]]).	brevityState _ #FileList.	^ acceptedContentsCache! !

!FileList methodsFor: 'private' stamp: 'jmv 3/13/2012 12:34'!
                             readContentsHex: brevity	"retrieve the contents from the external file unless it is too long.	  Don't create a file here.  Check if exists."	| f size data hexData s |	f := directory oldFileOrNoneNamed: self fullName. 	f ifNil: [^ 'For some reason, this file cannot be read'].	((size := f size)) > 5000 & brevity		ifTrue: [data := f next: 10000. f close. brevityState := #briefHex]		ifFalse: [data := f contentsOfEntireFile. brevityState := #fullHex].	s := WriteStream on: (String new: data size*4).	0 to: data size-1 by: 16 do:		[:loc | s nextPutAll: loc hex; space;			nextPut: $(; print: loc; nextPut: $); space; tab.		loc+1 to: (loc+16 min: data size) do: [:i | s nextPutAll: (data at: i) hex; space].		s newLine].	hexData := s contents.	^ acceptedContentsCache _ ((size > 5000) & brevity		ifTrue: ['File ''{1}'' is {2} bytes long.You may use the ''get'' command to read the entire file.Here are the first 5000 characters...------------------------------------------{3}------------------------------------------... end of the first 5000 characters.' format: {fileName. size. hexData}]		ifFalse: [hexData])! !

!FileList methodsFor: 'private' stamp: 'jmv 3/13/2012 22:59'!
              updateFileList
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	| patterns |
	patterns _ OrderedCollection new.
	Cursor wait showWhile: [
		(pattern findTokens: (String with: Character crCharacter with: Character lfCharacter with: $;))
			do: [ :each |
				(each includes: $*) | (each includes: $#)
					ifTrue: [ patterns add: each]
					ifFalse: [
						each isEmpty
							ifTrue: [ patterns add: '*']
							ifFalse: [ patterns add: '*' , each , '*']]].

	list _ self listForPatterns: patterns.
	listIndex _ 0.
	fileName _ nil.
	acceptedContentsCache _ ''.
	self changed: #fileList.
	self changed: #updateButtonRow]! !


!FillInTheBlank methodsFor: 'accessing' stamp: 'jmv 3/14/2012 08:39'!
                            acceptOnCR	"Answer whether pressing <return> should cause input to be accepted."	^ acceptOnCR! !


!FillInTheBlankMorph class methodsFor: 'instance creation' stamp: 'jmv 3/14/2012 08:39'!
request: queryString initialAnswer: defaultAnswer centerAt: aPoint inWorld: aWorld onCancelReturn: returnOnCancel acceptOnCR: acceptBoolean answerExtent: answerExtent	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts.   If the user cancels, answer returnOnCancel."	"
	FillInTheBlankMorph		request: 'Type something, then type [Return].'		initialAnswer: 'yo ho ho!!'		centerAt: Display center
	"	| aFillInTheBlankMorph |	aFillInTheBlankMorph _ self new		setQuery: queryString		initialAnswer: defaultAnswer		answerExtent: answerExtent		acceptOnCR: acceptBoolean.	aFillInTheBlankMorph responseUponCancel: returnOnCancel.	aWorld addMorph: aFillInTheBlankMorph centeredNear: aPoint.	^ aFillInTheBlankMorph getUserResponse! !


!Form methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:37'!
               store15To24HexBitsOn:aStream	| buf i lineWidth |	"write data for 16-bit form, optimized for encoders writing directly to files to do one single file write rather than 12. I'm not sure I understand the significance of the shifting pattern, but I think I faithfully translated it from the original"	lineWidth _ 0.	buf _ String new: 12.	bits do: [:word | 		i _ 0.		"upper pixel"		buf at: (i _ i + 1) put: ((word bitShift: -27) bitAnd: 15) asHexDigit.		buf at: (i _ i + 1) put: ((word bitShift: -32) bitAnd: 8) asHexDigit.		buf at: (i _ i + 1) put: ((word bitShift: -22) bitAnd: 15) asHexDigit.		buf at: (i _ i + 1) put: ((word bitShift: -27) bitAnd: 8) asHexDigit.		buf at: (i _ i + 1) put: ((word bitShift: -17) bitAnd: 15) asHexDigit.		buf at: (i _ i + 1) put: ((word bitShift: -22) bitAnd: 8) asHexDigit.		"lower pixel"		buf at: (i _ i + 1) put: ((word bitShift: -11) bitAnd: 15) asHexDigit.		buf at: (i _ i + 1) put: ((word bitShift: -16) bitAnd: 8) asHexDigit.		buf at: (i _ i + 1) put: ((word bitShift: -6) bitAnd: 15) asHexDigit.		buf at: (i _ i + 1) put: ((word bitShift: -11) bitAnd: 8) asHexDigit.		buf at: (i _ i + 1) put: ((word bitShift: -1) bitAnd: 15) asHexDigit.		buf at: (i _ i + 1) put: ((word bitShift: -6) bitAnd: 8) asHexDigit.		aStream nextPutAll: buf.		lineWidth _ lineWidth + 12.		lineWidth > 100 ifTrue: [ aStream newLine. lineWidth _ 0 ].		"#( 31 26 21 15 10 5 )  do:[:startBit | ]"	].! !

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 15:31'!
                            storeBitsOn:aStream base:anInteger	bits do: [:word | 		anInteger = 10			ifTrue: [aStream space]			ifFalse: [aStream newLineTab: 2].		word printOn: aStream base: anInteger].! !

!Form methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 15:31'!
            storeOn: aStream base: anInteger 	"Store the receiver out as an expression that can be evaluated to recreate a Form with the same contents as the original."	self unhibernate.	aStream nextPut: $(.	aStream nextPutAll: self species name.	aStream newLineTab: 1.	aStream nextPutAll: 'extent: '.	self extent printOn: aStream.	aStream newLineTab: 1.	aStream nextPutAll: 'depth: '.	self depth printOn: aStream.	aStream newLineTab: 1.	aStream nextPutAll: 'fromArray: #('.	self storeBitsOn:aStream base:anInteger.	aStream nextPut: $).	aStream newLineTab: 1.	aStream nextPutAll: 'offset: '.	self offset printOn: aStream.	aStream nextPut: $).! !


!ColorForm methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:27'!
              storeOn: aStream	aStream nextPut: $(.	super storeOn: aStream.	aStream		newLine; tab;		nextPutAll: 'colorsFromArray: #('.	self colors do: [:color |		color storeArrayOn: aStream].	aStream nextPutAll: ' ))'.! !


!GZipSurrogateStream methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 12:35'!
                 timeStamp	"Append the current time to the receiver as a String."	self bufferStream nextChunkPut:	"double string quotes and !!s"		(String streamContents: [:s | Smalltalk timeStamp: s]) printString.	self bufferStream newLine! !


!HandleMorph methodsFor: 'event handling' stamp: 'jmv 3/13/2012 10:20'!
                keyStroke: evt	"Check for cursor keys"	| keyValue |	(owner is: #HandMorph) ifFalse: [ ^self ].	keyValue _ evt keyValue.	keyValue = 28 ifTrue:[^self position: self position - (1@0)].	keyValue = 29 ifTrue:[^self position: self position + (1@0)].	keyValue = 30 ifTrue:[^self position: self position - (0@1)].	keyValue = 31 ifTrue:[^self position: self position + (0@1)].	"Special case for return"	evt isReturnKey ifTrue:[		"Drop the receiver and be done"	self flag: #arNote. "Probably unnecessary"		owner releaseKeyboardFocus: self.		self delete].! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 3/14/2012 08:38'!
     crAction	"Return the action to perform when user presses <Return> key"	^self valueOfProperty: #crAction! !

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 3/13/2012 10:54'!
              acceptContents	"The message is sent when the user hits return or Cmd-S.	Accept the current contents and end editing."	"Inform the model of text to be accepted, and return true if OK."	| ok prevSelection prevScrollValue |	prevSelection _ self editor selectionInterval copy.	prevScrollValue _ owner verticalScrollBar value.	(self canDiscardEdits and: [(self hasProperty: #alwaysAccept) not]) 		ifTrue: [^self flash].	self hasEditingConflicts 		ifTrue: [			(self confirm: 'Caution!! Contents were savedelsewhere since you startedediting them here.  Accept anyway?' ) 					ifFalse: [^self flash]].	ok _ model acceptContentsFrom: owner.	ok == true		ifTrue: [ model refetch ].	"sps 8/13/2001 22:41: restore selection and scroll info"		["During the step for the browser, updatePaneIfNeeded is called, and 		invariably resets the contents of the codeholding PluggableTextMorph		at that time, resetting the cursor position and scroller in the process.		The following line forces that update without waiting for the step, 		then restores the cursor and scrollbar"		ok			ifTrue: [				self editor selectFrom: prevSelection first to: prevSelection last.				WorldState addDeferredUIMessage: [						World activeHand newKeyboardFocus: self.						owner setScrollDeltas.						owner verticalScrollBar setValue: prevScrollValue ]]	] on: Error do: nil! !

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 3/13/2012 10:55'!
  cancelEdits	"The message is sent when the user hits Cmd-L.	Cancel the current contents and end editing."	self releaseEditorAndParagraph.	model refetch! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 3/13/2012 10:22'!
                      processKeyStroke: evt	| action |	(acceptOnCR and: [evt isReturnKey])		ifTrue: [^ self acceptContents].	self pauseBlinking.	evt isReturnKey ifTrue: [	"Return - check for special action"		action _ self crAction.		action ifNotNil: [			^action value]].	self handleInteraction: [ editor processKeyStroke: evt ].	self updateFromParagraph.	self eventHandler ifNotNil: [		"like'super keyStroke: evt'"		self eventHandler keyStroke: evt fromMorph: self].	self scrollSelectionIntoView! !


!InstructionPrinter methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:36'!
        print: instruction 	"Append to the receiver a description of the bytecode, instruction." 	| code |	stream tab: self indent.	printPC ifTrue: [stream print: oldPC; space].	stream tab: (innerIndents at: oldPC).	stream nextPut: $<.	oldPC to: scanner pc - 1 do: 		[:i | 		code := (method at: i) radix: 16.		stream nextPut: 			(code size < 2				ifTrue: [$0]				ifFalse: [code at: 1]).		stream nextPut: code last; space].	stream skip: -1.	stream nextPut: $>.	stream space.	stream nextPutAll: instruction.	stream newLine.	oldPC := scanner pc.	"(InstructionPrinter compiledMethodAt: #print:) symbolic."! !


!InstructionPrinter class methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:37'!
   printClass: class 	"Create a file whose name is the argument followed by '.bytes'. Store on 	the file the symbolic form of the compiled methods of the class."	| file |	file := FileStream newFileNamed: class name , '.bytes'.	class selectorsDo: [ :sel | 		file newLine; nextPutAll: sel; newLine.		(self on: (class compiledMethodAt: sel)) printInstructionsOn: file].	file close	"InstructionPrinter printClass: Parser."! !


!LightWidget methodsFor: 'DNU' stamp: 'jmv 3/13/2012 12:37'!
                    doesNotUnderstand: aMessage	Transcript newLine; show: self printString, ' >> ', aMessage printString! !


!ButtonLW methodsFor: 'event handling' stamp: 'jmv 3/13/2012 10:19'!
                keyDown: evt	"Handle a key down event."		evt isReturnKey ifTrue: [		pressed _ true.		self updateView ].	super keyDown: evt! !

!ButtonLW methodsFor: 'event handling' stamp: 'jmv 3/13/2012 10:19'!
                      keyStroke: anEvent			anEvent isReturnKey ifTrue: [		self performAction.		^self updateView].	"Call super for ignored keys"	super keyStroke: anEvent! !

!ButtonLW methodsFor: 'event handling' stamp: 'jmv 3/13/2012 10:19'!
                             keyUp: evt	"Handle a key up event."	"Warning: action is carried out in #keystroke: and not here because LIRC does not send	#keyDown: / #keyUp"		evt isReturnKey ifTrue: [		pressed _ false.		self updateView ].	super keyUp: evt! !


!CheckBoxLW methodsFor: 'event handling' stamp: 'jmv 3/13/2012 10:19'!
           keyStroke: anEvent			anEvent isReturnKey ifTrue: [		selected _ selected not.		target perform: action with: selected.		^self updateView].	"Call super for ignored keys"	super keyStroke: anEvent! !


!EntryField2LW methodsFor: 'accessing' stamp: 'jmv 3/14/2012 08:38'!
              crAction: aSymbol	"Set the action to execute when the user presses <Return>.	Optional. Used usually when there are no others widgets to go to, 	and input should be accepted and processed"		^ crAction _ aSymbol! !

!EntryField2LW methodsFor: 'event handling' stamp: 'jmv 3/13/2012 10:28'!
                           keyStroke: aKeyboardEvent	"Handle a keystroke event."	| k |	aKeyboardEvent commandAltKeyPressed		ifTrue: [ ^super keyStroke: aKeyboardEvent ].		k _ aKeyboardEvent keyValue.	aKeyboardEvent isReturnKey ifTrue: [		crAction ifNotNil: [				self performActionWith: contents.			self updateView.			^target perform: crAction ].		^ aKeyboardEvent hand keyboardFocusNext ].	(k = 30 or: [ k = 31 ])		ifTrue: [ ^super keyStroke: aKeyboardEvent ].	(k = 28 and: [ editor isAtStart ])		ifTrue: [ ^super keyStroke: aKeyboardEvent ].	(k = 29 and: [ editor isAtEnd ])		ifTrue: [ ^super keyStroke: aKeyboardEvent ].	self handleInteraction: [ editor processKeyStroke: aKeyboardEvent ].	self updateFromContents! !


!MIDIFileReader methodsFor: 'chunk reading' stamp: 'jmv 3/13/2012 12:37'!
          readHeaderChunk	| chunkType chunkSize division |	chunkType _ self readChunkType.	chunkType = 'RIFF' ifTrue:[chunkType _ self riffSkipToMidiChunk].	chunkType = 'MThd' ifFalse: [self scanForMIDIHeader].	chunkSize _ self readChunkSize.	fileType _ self next16BitWord.	trackCount _ self next16BitWord.	division _ self next16BitWord.	(division anyMask: 16r8000)		ifTrue: [self error: 'SMPTE time formats are not yet supported']		ifFalse: [ticksPerQuarter _ division].	maxNoteTicks _ 12 * 4 * ticksPerQuarter.		"longest acceptable note; used to detect stuck notes"	"sanity checks"	((chunkSize < 6) or: [chunkSize > 100])		ifTrue: [self error: 'unexpected MIDI header size ', chunkSize printString].	(#(0 1 2) includes: fileType)		ifFalse: [self error: 'unknown MIDI file type ', fileType printString].	Transcript		show: 'Reading Type ', fileType printString, ' MIDI File (';		show: trackCount printString, ' tracks, ';		show: ticksPerQuarter printString, ' ticks per quarter note)';		newLine.! !

!MIDIFileReader methodsFor: 'track reading' stamp: 'jmv 3/13/2012 12:37'!
                             readTrackContents: byteCount	| info |	strings _ OrderedCollection new.	track _ OrderedCollection new.	trackStream _ ReadStream on: (stream next: byteCount).	activeEvents _ OrderedCollection new.	self readTrackEvents.	(tracks isEmpty and: [self isTempoTrack: track])		ifTrue: [tempoMap _ track asArray]		ifFalse: [			"Note: Tracks without note events are currently not saved to			 eliminate clutter in the score player. In control applications,			 this can be easily changed by modifying the following test."			(self trackContainsNotes: track) ifTrue: [				tracks add: track asArray.				info _ WriteStream on: (String new: 100).				strings do: [:s | info nextPutAll: s; newLine].				trackInfo add: info contents]].	strings _ track _ trackStream _ activeEvents _ nil.! !

!MIDIFileReader methodsFor: 'private' stamp: 'jmv 3/13/2012 12:37'!
      report: aString	Transcript show: aString; newLine! !


!MIDIInputParser methodsFor: 'midi monitor' stamp: 'jmv 3/13/2012 12:38'!
                             printCmd: cmdByte with: arg1 with: arg2	"Print the given MIDI command."	| cmd ch bend |	cmdByte < 240		ifTrue: [  "channel message" 			cmd _ cmdByte bitAnd: 2r11110000.			ch _ (cmdByte bitAnd: 2r00001111) + 1]		ifFalse: [cmd _ cmdByte].  "system message"	cmd = 128 ifTrue: [		^ Transcript show: ('key up ', arg1 printString, ' vel: ', arg2 printString, ' chan: ', ch printString); newLine].	cmd = 144 ifTrue: [		^ Transcript show: ('key down: ', arg1 printString, ' vel: ', arg2 printString, ' chan: ', ch printString); newLine].	cmd = 160 ifTrue: [		^ Transcript show: ('key pressure: ', arg1 printString, ' val: ', arg2 printString, ' chan: ', ch printString); newLine].	cmd = 176 ifTrue: [		^ Transcript show: ('CC', arg1 printString, ': val: ', arg2 printString, ' chan: ', ch printString); newLine].	cmd = 192 ifTrue: [		^ Transcript show: ('prog: ', (arg1 + 1) printString, ' chan: ', ch printString); newLine].	cmd = 208 ifTrue: [		^ Transcript show: ('channel pressure ', arg1 printString, ' chan: ', ch printString); newLine].	cmd = 224 ifTrue: [		bend _ ((arg2 bitShift: 7) + arg1) - 8192.		^ Transcript show: ('bend: ', bend printString, ' chan: ', ch printString); newLine].	cmd = 240 ifTrue: [		^ Transcript show: ('system exclusive: ', (arg1 at: 1) printString, ' (', arg1 size printString, ' bytes)'); newLine].	Transcript show: 'cmd: ', cmd printString, ' arg1: ', arg1 printString, ' arg2: ', arg2 printString; newLine.! !


!Matrix methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:38'!
   print	self printOn: Transcript.	Transcript newLine! !

!Matrix methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:38'!
          printOn: aStream	1 to: self height do: [ :i |		aStream nextPutAll: '| '.		1 to: self width do: [ :j |			aStream nextPutAll: ((self i: i j: j) printPaddedLeft: 1 decimalPlaces: 2).			"aStream print: ((self i: i j: j) roundTo: 0.001)."			aStream nextPut: $  ].		aStream nextPut: $|; newLine ]! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 3/13/2012 22:16'!
             addTitle: aString
	"Add a title line at the top of this menu Make aString its initial 
	contents.  
	If aSelector is not nil, then periodically obtain fresh values for its 
	contents by sending aSelector to aTarget.."

	| s p w |
	
	titleMorph _ RectangleMorph new.
	self setTitleParametersFor: titleMorph.
	p _ titleMorph position + (8@2).
	aString asString linesDo: [ :line | 
		s _ StringMorph new
			contents: line;
			font: Preferences standardMenuFont bold.
		s position: p.
		titleMorph addMorphBack: s.
		p _ p + (0@(s height+2)) ].
	w _ titleMorph submorphs inject: 0 into: [ :prev :each |
		prev max: each width ].
	titleMorph height: p y; width: w + 8.
	self addMorphFront: titleMorph.
	
	(self hasProperty: #needsStayUpIcons) ifTrue: [ self addStayUpIcons ]! !

!MenuMorph methodsFor: 'construction' stamp: 'jmv 3/13/2012 22:16'!
                      labels: labelList lines: linesArray selections: selectionsArray
	"This method allows the receiver to accept old-style SelectionMenu creation messages. It should be used only for backward compatibility during the MVC-to-Morphic transition. New code should be written using the other menu construction protocol such as addList:."
	"Labels can be either a sting with embedded crs, or a collection of strings."

	| labelArray |
	labelArray _ (labelList isMemberOf: String)
		ifTrue: [ labelList lines ]
		ifFalse: [ labelList ].
	1 to: labelArray size do: [ :i |
		self add: (labelArray at: i) action: (selectionsArray at: i).
		(linesArray includes: i) ifTrue: [ self addLine ]]! !

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 3/13/2012 23:13'!
                keyStroke: evt 
	| matchString char asc selectable help |
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self activeHand.
	char := evt keyCharacter.
	asc := char asciiValue.
	evt isReturnKey
		ifTrue: 
			[selectedItem ifNotNil: 
					[selectedItem hasSubMenu 
						ifTrue: 
							[evt hand newMouseFocus: selectedItem subMenu.
							^evt hand newKeyboardFocus: selectedItem subMenu]
						ifFalse: 
							["self delete."

							^selectedItem invokeWithEvent: evt]].
			(selectable := self items) size = 1 
				ifTrue: [^selectable first invokeWithEvent: evt].
			^self].
	asc = 27 
		ifTrue: 
			["escape key"

			self valueOfProperty: #matchString
				ifPresentDo: 
					[:str | 
					str isEmpty 
						ifFalse: 
							["If filtered, first ESC removes filter"

							self setProperty: #matchString toValue: String new.
							self selectItem: nil event: evt.
							^self displayFiltered: evt]].
			"If a stand-alone menu, just delete it"
			popUpOwner ifNil: [^self delete].
			"If a sub-menu, then deselect, and return focus to outer menu"
			self selectItem: nil event: evt.
			evt hand newMouseFocus: popUpOwner owner.
			^evt hand newKeyboardFocus: popUpOwner owner].
	(asc = 28 or: [asc = 29]) 
		ifTrue: 
			["left or right arrow key"

			(selectedItem notNil and: [selectedItem hasSubMenu]) 
				ifTrue: 
					[evt hand newMouseFocus: selectedItem subMenu.
					selectedItem subMenu moveSelectionDown: 1 event: evt.
					^evt hand newKeyboardFocus: selectedItem subMenu]].
	asc = 30 ifTrue: [^self moveSelectionDown: -1 event: evt].	"up arrow key"
	asc = 31 ifTrue: [^self moveSelectionDown: 1 event: evt].	"down arrow key"
	asc = 11 ifTrue: [^self moveSelectionDown: -5 event: evt].	"page up key"
	asc = 12 ifTrue: [^self moveSelectionDown: 5 event: evt].	"page down key"
	matchString := self valueOfProperty: #matchString ifAbsentPut: [String new].
	matchString := char = Character backspace 
				ifTrue: 
					[matchString isEmpty ifTrue: [matchString] ifFalse: [matchString allButLast]]
				ifFalse: [matchString copyWith: evt keyCharacter].
	self setProperty: #matchString toValue: matchString.
	self displayFiltered: evt.
	help _ HoverHelpMorph contents: 'Enter text to\narrow selection down\to matching items ' withNewLines.
	help popUpForHand: self activeHand.
! !


!MVCMenuMorph class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 22:15'!
            from: aPopupMenu title: titleStringOrNil
	"Answer a MenuMorph constructed from the given PopUpMenu. Used to simulate MVC-style menus in a Morphic-only world."

	| menu items lines selections labelString j emphasis |
	menu _ self new.
	titleStringOrNil ifNotNil: [
		titleStringOrNil isEmpty ifFalse: [menu addTitle: titleStringOrNil]].
	labelString _ aPopupMenu labelString.
	items _ labelString asString lines.
	(labelString is: #Text) ifTrue: [
		"Pass along text emphasis if present"
		j _ 1.
		items _ items collect: [ :item |
			j _ labelString asString findString: item startingAt: j.
			emphasis _ TextEmphasis new emphasisCode: (labelString emphasisAt: j).
			item asText addAttribute: emphasis]].
	lines _ aPopupMenu lineArray.
	lines ifNil: [lines _ #()].
	menu cancelValue: 0.
	menu defaultTarget: menu.
	selections _ (1 to: items size) asArray.
	1 to: items size do: [ :i |
		menu add: (items at: i) selector: #selectMVCItem: argument: (selections at: i).
		(lines includes: i) ifTrue: [menu addLine]].
	^ menu
! !


!Message methodsFor: 'stub creation' stamp: 'jmv 3/13/2012 12:38'!
                        createStubMethod	| argNames aOrAn argName arg argClassName |	argNames _ Set new.	^ String streamContents: [ :s |		self selector keywords doWithIndex: [ :key :i |			s nextPutAll: key.			((key last = $:) or: [self selector isInfix]) ifTrue: [				arg _ self arguments at: i.				argClassName _ (arg isKindOf: Class) ifTrue: ['Class'] ifFalse: [arg class name].				aOrAn _ argClassName first isVowel ifTrue: ['an'] ifFalse: ['a'].				argName _ aOrAn, argClassName.				[argNames includes: argName] whileTrue: [argName _ argName, i asString].				argNames add: argName.				s nextPutAll: ' '; nextPutAll: argName; space			].		].		s newLine; tab.		s nextPutAll: 'self shouldBeImplemented'	]! !


!MessageTally methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:38'!
                             fullPrintExactOn: aStream	aStream nextPutAll: '**Tree**'; newLine.	self		treePrintOn: aStream		tabs: OrderedCollection new		thisTab: ''		total: tally		totalTime: time		tallyExact: true		orThreshold: nil.	aStream nextPut: Character newPage; newLine.	aStream nextPutAll: '**Leaves**'; newLine.	self leavesPrintExactOn: aStream! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:38'!
        fullPrintOn: aStream threshold: perCent	| threshold |  	threshold _ (perCent asFloat / 100 * tally) rounded.	aStream nextPutAll: '**Tree**'; newLine.	self		rootPrintOn: aStream		total: tally		totalTime: time		threshold: threshold.	aStream nextPut: Character newPage; newLine.	aStream nextPutAll: '**Leaves**'; newLine.	self		leavesPrintOn: aStream		threshold: threshold! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:38'!
                          printOn: aStream total: total totalTime: totalTime tallyExact: isExact 	isExact 		ifTrue: [			| myTally |			myTally := tally.			receivers				ifNotNil: [receivers do: [:r | myTally := myTally - r tally]].			aStream				print: myTally;				space]		ifFalse: [			| percentage |			percentage := tally asFloat / total * 100.0 roundTo: 0.1.			aStream				print: percentage;				nextPutAll: '% {';				print: (percentage * totalTime / 100) rounded;				nextPutAll: 'ms} '].	receivers		ifNil: [			aStream				nextPutAll: 'primitives';				newLine]		ifNotNil: [			| className aSelector aClass |			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].			className := aClass name contractTo: self maxClassNameSize.			aStream				nextPutAll: class name;				nextPutAll: (aClass = class 							ifTrue: ['>>']							ifFalse: ['(' , aClass name , ')>>']);				nextPutAll: (aSelector 							contractTo: self maxClassPlusSelectorSize - className size);				newLine]! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:39'!
                         rootPrintOn: aStream total: total totalTime: totalTime threshold: threshold	| groups |	groups _ (self sonsOver: threshold) groupBy: [ :aTally | aTally process] having: [ :g | true ].	groups do: [ :g |		| sons p |		sons _ g asArray sort.		p _ g anyOne process.		(reportOtherProcesses or: [ p notNil ]) ifTrue: [			aStream nextPutAll: '--------------------------------'; newLine.			aStream nextPutAll: 'Process: ',  (p ifNil: [ 'other processes'] ifNotNil: [ p browserPrintString]); newLine.			aStream nextPutAll: '--------------------------------'; newLine.			sons do: [ :son |				son					treePrintOn: aStream					tabs: OrderedCollection new					thisTab: ''					total: total					totalTime: totalTime					tallyExact: false					orThreshold: threshold]].	]! !

!MessageTally methodsFor: 'reporting' stamp: 'jmv 3/13/2012 12:39'!
                  report: strm cutoff: threshold 	tally = 0		ifTrue: [strm nextPutAll: ' - no tallies obtained']		ifFalse: [ 			strm nextPutAll: ' - '; print: tally; nextPutAll: ' tallies, ', time printString, ' msec.'; newLine; newLine.			self fullPrintOn: strm threshold: threshold].			time isZero ifFalse:	[		self reportGCStatsOn: strm].! !

!MessageTally methodsFor: 'reporting' stamp: 'jmv 3/13/2012 12:39'!
             reportGCStatsOn: str	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |	upTime _ time.	oldSpaceEnd			_ gcStats at: 1.	youngSpaceEnd		_ gcStats at: 2.	memoryEnd			_ gcStats at: 3.	fullGCs				_ gcStats at: 7.	fullGCTime			_ gcStats at: 8.	incrGCs				_ gcStats at: 9.	incrGCTime			_ gcStats at: 10.	tenureCount			_ gcStats at: 11.	rootOverflows		_ gcStats at: 22.	str newLine.	str	nextPutAll: '**Memory**'; newLine.	str	nextPutAll:	'	old			';		nextPutAll: oldSpaceEnd asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.	str	nextPutAll: '	young		';		nextPutAll: (youngSpaceEnd - oldSpaceEnd) asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.	str	nextPutAll: '	used		';		nextPutAll: youngSpaceEnd asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.	str	nextPutAll: '	free		';		nextPutAll: (memoryEnd - youngSpaceEnd) asStringWithCommasSigned; nextPutAll: ' bytes'; newLine.	str newLine.	str	nextPutAll: '**GCs**'; newLine.	str	nextPutAll: '	full			';		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime asStringWithCommas; nextPutAll: 'ms (';		print: ((fullGCTime / upTime * 100) roundTo: 1.0);		nextPutAll: '% uptime)'.	fullGCs = 0 ifFalse:		[str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) roundTo: 1.0); nextPutAll: 'ms'].	str	newLine.	str	nextPutAll: '	incr		';		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime asStringWithCommas; nextPutAll: 'ms (';		print: ((incrGCTime / upTime * 100) roundTo: 1.0);		nextPutAll: '% uptime)'.	incrGCs = 0 ifFalse:		[str nextPutAll:', avg '; print: ((incrGCTime / incrGCs) roundTo: 1.0); nextPutAll: 'ms'].	str newLine.	str	nextPutAll: '	tenures		';		nextPutAll: tenureCount asStringWithCommas.	tenureCount = 0 ifFalse:		[str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].	str	newLine.	str	nextPutAll: '	root table	';		nextPutAll: rootOverflows asStringWithCommas; nextPutAll:' overflows'.	str newLine.! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 3/13/2012 12:39'!
                  tallySendsTo: receiver inBlock: aBlock showTree: treeOption	"	MessageTally tallySends: [3.14159 printString]	"	"This method uses the simulator to count the number of calls on each method	invoked in evaluating aBlock. If receiver is not nil, then only sends	to that receiver are tallied.	Results are presented as leaves, sorted by frequency,	preceded, optionally, by the whole tree."	| prev tallies startTime totalTime |	startTime _ Time millisecondClockValue.	tallies _ MessageTally new class: aBlock receiver class method: aBlock method.	tallies reportOtherProcesses: true.	"Do NOT filter nodes with nil process"	prev _ aBlock.	thisContext sender		runSimulated: aBlock		contextAtEachStep: [ :current |			current == prev ifFalse: [ "call or return"				prev sender ifNotNil: [ "call only"					(receiver == nil or: [ current receiver == receiver ])						ifTrue: [ tallies tally: current by: 1 ]].				prev _ current]].	totalTime _ Time millisecondClockValue - startTime // 1000.0 roundTo: 0.01.	SystemWindow		editText: (TextModel withText: (String streamContents: [ :s |			s nextPutAll: 'This simulation took ' , totalTime printString, ' seconds.'; newLine.			treeOption				ifTrue: [ tallies fullPrintExactOn: s ]				ifFalse: [ tallies leavesPrintExactOn: s ]]))		label: 'Spy Results'		wrap: false! !


!Metaclass methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 15:33'!
           definition	"Refer to the comment in ClassDescription|definition."	^ String streamContents: [ :strm |		strm print: self;			newLine;			tab;			nextPutAll: 'instanceVariableNames: ';			store: self instanceVariablesString]! !

!Metaclass methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:39'!
                        fileOutInitializerOn: aStream	(self methodDict includesKey: #initialize) ifTrue: [		aStream newLine.		aStream nextChunkPut: thisClass name , ' initialize'].! !

!Metaclass methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:40'!
                          fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool	super fileOutOn: aFileStream		moveSource: moveSource		toFile: fileIndex.	(aBool and:[moveSource not and: [self methodDict includesKey: #initialize]]) ifTrue: [		aFileStream newLine.		aFileStream newLine.		aFileStream nextChunkPut: thisClass name , ' initialize'.		aFileStream newLine ]! !


!MethodContext methodsFor: 'printing' stamp: 'jmv 3/13/2012 22:36'!
                             printDetails: strm
	"Put my class>>selector and instance variables and arguments and temporaries on the stream.  Protect against errors during printing."

	| pe str pos |
	self printOn: strm.
	strm newLine.
	strm tab; nextPutAll: 'Receiver: '.
	pe _ '<<error during printing>>'.
	strm nextPutAll: ([receiver printStringLimitedTo: 90] ifError: [:err :rcvr | pe]).

	strm newLine; tab; nextPutAll: 'Arguments and temporary variables: '; newLine.
	str _ [(self tempsAndValuesLimitedTo: 80 indent: 2) 
				padded: #right to: 1 with: $x] ifError: [:err :rcvr | pe].
	strm nextPutAll: (str allButLast).

	strm newLine; tab; nextPutAll: 'Receiver''s instance variables: '; newLine.
	pos _ strm position.
	[receiver longPrintOn: strm limitedTo: 80 indent: 2] ifError: [:err :rcvr | 
				strm nextPutAll: pe].
	pos = strm position ifTrue: ["normal printString for an Array (it has no inst vars)"
		strm nextPutAll: ([receiver printStringLimitedTo: 90] ifError: [:err :rcvr | pe])].
	strm peekLast isLineSeparator ifFalse: [strm newLine].! !


!MethodDeletionChangeRecord methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/13/2012 12:40'!
                               fileOutOn: stream	"File the receiver out on the given file stream"	stream nextPut: $!!; nextChunkPut: 'methodRemoval: ', self methodClassName, ' ', self methodSelector; newLine.	stream nextChunkPut: self methodClassName, ' removeSelector: ', self methodSelector; newLine! !


!NameLookupFailure methodsFor: 'accessing' stamp: 'jmv 3/13/2012 23:13'!
defaultAction
	"Backward compatibility"
	| response |
	response _ (PopUpMenu labels: 'Retry\Give Up' withNewLines)
			startUpWithCaption: self messageText.
	^ response = 2
		ifFalse: [self retry]! !


!ObjectExplorerWrapper methodsFor: 'nil' stamp: 'jmv 3/13/2012 23:03'!
                asString
	| explorerString string |
	explorerString _ 
		[item printString]
			on: Error 
			do: ['<error in printString: evaluate "' , itemName , ' printString" to debug>'].
	string _ itemName , ': ' , explorerString.
	^ string withBlanksCondensed! !


!OneLineEditorMorph methodsFor: 'event handling' stamp: 'jmv 3/13/2012 10:22'!
                   keyStroke: aKeyboardEvent	"Handle a keystroke event."	(self focusKeyboardFor: aKeyboardEvent)		ifTrue: [ ^ self ].	(self closeWindowFor: aKeyboardEvent)		ifTrue: [ ^ self ].	"Return - check for special action	Note: Code below assumes that this was some	input field reacting on Return. Break the keyboard	focus so that the receiver can be safely deleted.	jmv - Currently not implemented"	"	evt isReturnKey ifTrue: [		action _ self crAction.		action ifNotNil: [			evt hand newKeyboardFocus: nil.			^action value ] ].	"	self pauseBlinking.	self handleInteraction: [ self editor processKeyStroke: aKeyboardEvent ].	self updateFromContents.	super keyStroke: aKeyboardEvent  "sends to keyStroke event handler, if any"! !


!PNGReadWriter methodsFor: 'accessing' stamp: 'jmv 3/13/2012 12:41'!
                       nextImage	bigEndian := Smalltalk isBigEndian.	filtersSeen _ Bag new.	globalDataChunk _ nil.	transparentPixelValue _ nil.	unknownChunks _ Set new.	stream reset.	stream binary.	stream skip: 8.	[stream atEnd] whileFalse: [self processNextChunk].	"Set up our form"	palette ifNotNil:[		"Dump the palette if it's the same as our standard palette"		palette = (StandardColors copyFrom: 1 to: palette size) 			ifTrue:[palette := nil]].	(depth <= 8 and:[palette notNil]) ifTrue:[		form := ColorForm extent: width@height depth: depth.		form colors: palette.	] ifFalse:[		form := Form extent: width@height depth: depth.	].	backColor ifNotNil:[form fillColor: backColor].	chunk _ globalDataChunk ifNil:[self error: 'image data is missing'].	chunk ifNotNil: [self processIDATChunk].	unknownChunks isEmpty ifFalse: [		"Transcript show: ' ',unknownChunks asSortedCollection asArray printString."	].	self debugging ifTrue: [		Transcript newLine; show: 'form = ',form printString.		Transcript newLine; show: 'colorType = ',colorType printString.		Transcript newLine; show: 'interlaceMethod = ',interlaceMethod printString.		Transcript newLine; show: 'filters = ',filtersSeen sortedCounts asArray printString.	].	^ form! !

!PNGReadWriter methodsFor: 'chunks' stamp: 'jmv 3/13/2012 12:42'!
      processNonInterlaced	| z filter temp copyMethod debug |	debug := self debugging.	copyMethod _ #(copyPixelsGray: nil copyPixelsRGB: copyPixelsIndexed:		  copyPixelsGrayAlpha: nil copyPixelsRGBA:) at: colorType+1.	debug ifTrue: [ Transcript newLine; nextPutAll: 'NI chunk size='; print: chunk size ].	z _ ZLibReadStream on: chunk from: 1 to: chunk size.	prevScanline _ ByteArray new: bytesPerScanline.	thisScanline := ByteArray new: bytesPerScanline.		(colorType = 0 and: [ bitsPerChannel < 16]) ifTrue: [		auxSource _ Form extent: 1 @ (thisScanline size // 4) depth: 32 bits: thisScanline.		auxDest _ Form extent: 1 @ (form bits size) depth: 32 bits: form bits.		auxCMap _ Smalltalk isLittleEndian			ifTrue:[ColorMap 					shifts: #(-24 -8 8 24) 					masks: #(16rFF000000 16r00FF0000 16r0000FF00 16r000000FF)].		auxBitBlt _ (BitBlt toForm: auxDest)			sourceForm: auxSource;			colorMap: auxCMap;			combinationRule: 3 ].	0 to: height-1 do: [ :y |		filter _ (z next: 1) first.		debug ifTrue:[filtersSeen add: filter].		thisScanline _ z next: bytesPerScanline into: thisScanline startingAt: 1.		(debug and: [ thisScanline size < bytesPerScanline ]) ifTrue: [			Transcript nextPutAll: ('wanted {1} but only got {2}' format: { bytesPerScanline. thisScanline size }); newLine ].		filter = 0 ifFalse:[self filterScanline: filter count: bytesPerScanline].		self perform: copyMethod with: y.		temp := prevScanline.		prevScanline := thisScanline.		thisScanline := temp.		].	z atEnd ifFalse:[self error:'Unexpected data'].	debug ifTrue: [Transcript  nextPutAll: ' compressed size='; print: z position  ].! !

!PNGReadWriter methodsFor: 'writing' stamp: 'jmv 3/13/2012 12:41'!
                              writeChunk: crcStream	| bytes length crc debug |	debug := self debugging.	bytes := crcStream originalContents.	length := crcStream position.	crc := self updateCrc: 16rFFFFFFFF from: 1 to: length in: bytes.	crc := crc bitXor: 16rFFFFFFFF.	debug ifTrue: [		Transcript newLine;			print: stream position; space;			nextPutAll: (bytes copyFrom: 1 to: 4) asString;			nextPutAll: ' len='; print: length;			nextPutAll: ' crc=0x'; nextPutAll: crc printStringHex  ].	stream nextNumber: 4 put: length-4. "exclude chunk name"	stream next: length putAll: bytes startingAt: 1.	stream nextNumber: 4 put: crc.	debug ifTrue: [ Transcript nextPutAll: ' afterPos='; print: stream position ].	crcStream resetToStart.! !

!PNGReadWriter methodsFor: 'writing' stamp: 'jmv 3/13/2012 12:41'!
                writeIDATChunkOn: aStream	"Write the IDAT chunk"	| z |	aStream nextPutAll: 'IDAT' asByteArray.	z _ ZLibWriteStream on: aStream.	form depth <= 8 		ifTrue:[self writeType3DataOn: z]		ifFalse:[ self writeType6DataOn: z].	self debugging ifTrue: [		Transcript newLine;			nextPutAll: 'compressed size=';			print: aStream position;			nextPutAll: ' uncompressed size=';			print: z position  ]! !


!PackageFile methodsFor: 'services' stamp: 'jmv 3/13/2012 12:41'!
          install: aFileStream	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."	| localName newChangeSet |	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."	'=============' print.	('classesToRemove: ', classesToRemove printString) print.	'=============' print.	'methodsToRemove: ' print.	methodsToRemove do: [ :methodReference | methodReference print ].	'=============' print.		"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"	"These were created in #fromFileStream: ... reuse?"	localName _ FileDirectory localNameFor: fullName.	newChangeSet _ ChangeSorter basicNewChangeSetLike: 'install package ', localName.	newChangeSet ifNotNil: [		ChangeSet newChanges: newChangeSet.		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine].		"Tirar undeclared al transcript. warning si quedaron undeclared	Es feo que tire an transcript undeclareds que despues no lo son..."	Smalltalk cleanOutUndeclared.	Undeclared print.	"Crear, instalar y devolver una instancia de PackageInfo. Descartar la instancia de PackageFile"! !


!Paragraph methodsFor: 'private' stamp: 'jmv 3/13/2012 23:03'!
           indentationOfLineIndex: lineIndex ifBlank: aBlock
	"Answer the number of leading tabs in the line at lineIndex.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value.
	 If the line is word-wrap overflow, back up a line and recur."

	| arrayIndex first last str |
	str _ model actualContents string.
	arrayIndex _ lineIndex.
	[
		first _ (lines at: arrayIndex) first.
		 first > 1 and: [(str at: first - 1) isLineSeparator not ] ] whileTrue: [ "word wrap"
			arrayIndex _ arrayIndex - 1].
	last _ (lines at: arrayIndex) last.
	
	^(str copyFrom: first to: last) indentationIfBlank: aBlock! !


!ParseNode methodsFor: 'private' stamp: 'jmv 3/13/2012 22:38'!
         nextWordFrom: aStream setCharacter: aBlock

	| outStream char |
	outStream _ WriteStream on: (String new: 16).
	[ (aStream peekFor: Character space) or: [ aStream peekFor: Character tab ]] whileTrue.
	[ aStream atEnd or: [
		char _ aStream next.
		char isSeparator ]]
			whileFalse: [ outStream nextPut: char ].
	aBlock value: char.
	^ outStream contents! !

!ParseNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 23:05'!
                         nodePrintOn: aStrm indent: nn
	| var aaStrm myLine |
	"Show just the sub nodes and the code."

	(aaStrm := aStrm) ifNil: [aaStrm := WriteStream on: (String new: 500)].
	nn timesRepeat: [aaStrm tab].
	aaStrm nextPutAll: self class name; space.
	myLine := self printString withBlanksCondensed.
	myLine := myLine copyFrom: 1 to: (myLine size min: 70).
	aaStrm nextPutAll: myLine; newLine.
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var respondsTo: #asReturnNode) ifTrue: [var nodePrintOn: aaStrm indent: nn+1]].
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var isKindOf: SequenceableCollection) ifTrue: [
				var do: [:aNode | 
					(aNode respondsTo: #asReturnNode) ifTrue: [
						aNode nodePrintOn: aaStrm indent: nn+1]]]].
	^ aaStrm
! !

!ParseNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:30'!
       printCommentOn: aStream indent: indent 	| thisComment |	self comment == nil ifTrue: [^ self].	1 to: self comment size do: [ :index |		index > 1 ifTrue: [aStream newLineTab: indent].		aStream nextPut: $".		thisComment := self comment at: index.		self printSingleComment: thisComment			on: aStream			indent: indent.		aStream nextPut: $"]! !


!BlockNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:29'!
 printOn: aStream indent: level	| separateLines |	aStream nextPut: $[.	self		printArgumentsOn: aStream		indent: level.	separateLines _ (self		printTemporaries: temporaries		on: aStream		doPrior: [ aStream space ]) or: [arguments size > 0 ].	Preferences prettyPrintRectangularBlocks		ifTrue: [			"If args+temps > 0 and statements > 1 (or just one complex statement),			put all statements on separate lines"			separateLines				ifTrue: [					(statements size > 1 or: [						statements size = 1 and: [ statements first isComplex ]])							ifTrue: [ aStream newLineTab: (1 max: level) ]							ifFalse: [ aStream space ] ]				ifFalse: [					(statements size = 1 and: [ statements first isComplex not ])						ifTrue: [ aStream space ]]]		ifFalse: [			self isComplex				ifTrue: [ aStream newLineTab: (1 max: level) ]				ifFalse: [ aStream space ] ].	((self printStatementsOn: aStream indent: level) > 0 and: [ aStream peekLast ~= $] ])		ifTrue: [ aStream space ].	aStream nextPut: $]! !

!BlockNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:31'!
                printStatementsOn: aStream indent: levelOrZero	| len shown thisStatement level |	level _ 1 max: levelOrZero.	comment ifNotNil: [		self printCommentOn: aStream indent: level.		aStream newLineTab: level].	len _ shown _ statements size.	(levelOrZero = 0 "top level" and: [statements last isReturnSelf])		ifTrue: [ shown _ 1 max: shown - 1]		ifFalse: ["should a trailing nil be printed or not? Not if it is an implicit result."				(arguments size = 0				and: [ len >= 1				and: [ (statements at: len) == NodeNil				and: [ len = 1					or: [ len > 1						and: [(statements at: len - 1) isMessageNode						and: [(statements at: len - 1) isNilIf ]]]]]])					ifTrue: [ shown _ shown - 1 ]].	1 to: shown do: 		[ :i |		thisStatement _ statements at: i.		thisStatement printOn: aStream indent: level.		i < shown ifTrue: [ aStream nextPut: $.; newLineTab: level ].		"Add a final period. This helps when pretty-diffing a method and a version of it that adds stuff after the end."		(i = shown and: [ levelOrZero = 0 ]) ifTrue: [ aStream nextPut: $. ].		(thisStatement comment notNil and: [ thisStatement comment size > 0 ])			ifTrue: [				i = shown ifTrue: [ aStream newLineTab: level ].				thisStatement printCommentOn: aStream indent: level.				i < shown ifTrue: [ aStream newLineTab: level ]]].	^shown! !

!BlockNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:29'!
                    printWithClosureAnalysisArgumentsOn: aStream indent: level	arguments size = 0 ifTrue: [^self].	arguments do: [ :tempNode |		aStream space; nextPut: $:.		tempNode printDefinitionForClosureAnalysisOn: aStream].	aStream nextPut: $|; space.	"If >0 args and >1 statement, put all statements on separate lines"	statements size > 1 ifTrue: [		aStream newLineTab: level]! !

!BlockNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:29'!
         printWithClosureAnalysisStatementsOn: aStream indent: levelOrZero	| len shown thisStatement level |	level := 1 max: levelOrZero.	comment ifNotNil: [		self printCommentOn: aStream indent: level.		aStream newLineTab: level].	len := shown := statements size.	(levelOrZero = 0 "top level" and: [statements last isReturnSelf])		ifTrue: [shown := 1 max: shown - 1]		ifFalse: [(len = 1 and: [((statements at: 1) == NodeNil) & (arguments size = 0)])					ifTrue: [shown := shown - 1]].	1 to: shown do: 		[:i | 		thisStatement := statements at: i.		thisStatement printWithClosureAnalysisOn: aStream indent: level.		i < shown ifTrue: [aStream nextPut: $.; newLineTab: level].		(thisStatement comment notNil and: [thisStatement comment size > 0])			ifTrue: [				i = shown ifTrue: [aStream newLineTab: level].				thisStatement printCommentOn: aStream indent: level.				i < shown ifTrue: [aStream newLineTab: level]]]! !

!BlockNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:30'!
printWithClosureAnalysisTemporariesOn: aStream indent: level	(temporaries == nil or: [temporaries size = 0]) ifFalse: [		aStream nextPut: $|.		temporaries do: [ :tempNode |			aStream space.			tempNode printDefinitionForClosureAnalysisOn: aStream].		aStream nextPutAll: ' | '.		"If >0 args and >1 statement, put all statements on separate lines"		statements size > 1 ifTrue: [aStream newLineTab: level]]! !


!CascadeNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:31'!
                             printOn: aStream indent: level precedence: p	p > 0 ifTrue: [ aStream nextPut: $( ].	messages first		printReceiver: receiver		on: aStream		indent: level.	1		to: messages size		do: [ :i | 			aStream newLineTab: level + 1.			(messages at: i)				printOn: aStream				indent: level.			i < messages size ifTrue: [ aStream nextPut:$; ] ].	p > 0 ifTrue: [ aStream nextPut: $) ]! !

!CascadeNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:31'!
                           printWithClosureAnalysisOn: aStream indent: level precedence: p 	p > 0 ifTrue: [aStream nextPut: $(].	messages first printWithClosureAnalysisReceiver: receiver on: aStream indent: level.	1 to: messages size do: 		[:i | (messages at: i) printWithClosureAnalysisOn: aStream indent: level.		i < messages size ifTrue: 				[aStream nextPut: $;.				messages first precedence >= 2 ifTrue: [aStream newLineTab: level + 1]]].	p > 0 ifTrue: [aStream nextPut: $)]! !


!Encoder methodsFor: 'private' stamp: 'jmv 3/13/2012 12:33'!
                warnAboutShadowed: name	requestor addWarning: name,' is shadowed'.	selector ifNotNil:		[Transcript newLine; show: class name,'>>', selector, '(', name,' is shadowed)']! !


!MessageNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:32'!
              printCaseOn: aStream indent: level 	"receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]"	| braceNode otherwise extra |	braceNode := arguments first.	otherwise := arguments last.	(arguments size = 1 or: [otherwise isJustCaseError]) ifTrue:		[otherwise := nil].	receiver		printOn: aStream		indent: level		precedence: 3.	aStream nextPutAll: ' caseOf: '.	braceNode isVariableReference		ifTrue: [braceNode printOn: aStream indent: level]		ifFalse: [			aStream nextPutAll: '{'; newLineTab: level + 1.			braceNode casesForwardDo: [ :keyNode :valueNode :last | 				keyNode printOn: aStream indent: level + 1.				aStream nextPutAll: ' -> '.				valueNode printsInNewLine					ifTrue: [						aStream newLineTab: level + 2.						extra := 1]					ifFalse: [extra := 0].				valueNode printOn: aStream indent: level + 1 + extra.				last ifTrue: [aStream nextPut: $}]					ifFalse: [aStream nextPut: $.;							 newLineTab: level + 1]]].	otherwise ifNotNil: [		aStream newLineTab: level + 1; nextPutAll: ' otherwise: '.		 extra := otherwise printsInNewLine					ifTrue: [						aStream newLineTab: level + 2.						1]					ifFalse: [0].		 otherwise printOn: aStream indent: level + 1 + extra]! !

!MessageNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:32'!
                           printKeywords: key arguments: args on: aStream indent: level 	| keywords indent arg kwd doCrTab |	args size = 0 ifTrue:		[ aStream			 space ;			 nextPutAll: key.		^ self ].	keywords := key keywords.	doCrTab := args size > 1.	1		to: (args size min: keywords size)		do:			[ : i | arg := args at: i.			kwd := keywords at: i.			doCrTab				ifTrue: [					aStream newLineTab: level + 1.					indent := 1					"newline after big args" ]				ifFalse:					[ aStream space.					indent := 0 ].			aStream nextPutAll: kwd.			arg printsInNewLine				ifTrue: [ aStream newLineTab: level + indent + 1 ]				ifFalse: [ aStream space ].			arg				printOn: aStream				indent: level + 1 + indent				precedence:					(precedence = 2						ifTrue: [ 1 ]						ifFalse: [ precedence ]) ]! !

!MessageNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:32'!
       printWithClosureAnalysisCaseOn: aStream indent: level 	"receiver caseOf: {[key]->[value]. ...} otherwise: [otherwise]"	| braceNode otherwise extra |	braceNode := arguments first.	otherwise := arguments last.	(arguments size = 1 or: [otherwise isJustCaseError]) ifTrue:		[otherwise := nil].	receiver		printWithClosureAnalysisOn: aStream		indent: level		precedence: 3.	aStream nextPutAll: ' caseOf: '.	braceNode isVariableReference		ifTrue: [braceNode printWithClosureAnalysisOn: aStream indent: level]		ifFalse: 			[aStream nextPutAll: '{'; newLineTab: level + 1.			 braceNode casesForwardDo:				[:keyNode :valueNode :last | 				keyNode printWithClosureAnalysisOn: aStream indent: level + 1.				aStream nextPutAll: ' -> '.				valueNode printsInNewLine					ifTrue: 						[aStream newLineTab: level + 2.						extra := 1]					ifFalse: [extra := 0].				valueNode printWithClosureAnalysisOn: aStream indent: level + 1 + extra.				last ifTrue: [aStream nextPut: $}]					ifFalse: [aStream nextPut: $.;							 newLineTab: level + 1]]].	otherwise ifNotNil: [		aStream newLineTab: level + 1; nextPutAll: ' otherwise: '.		 extra := otherwise printsInNewLine					ifTrue: [						aStream newLineTab: level + 2.						1]					ifFalse: [0].		 otherwise printWithClosureAnalysisOn: aStream indent: level + 1 + extra]! !

!MessageNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:32'!
     printWithClosureAnalysisKeywords: key arguments: args on: aStream indent: level	| keywords indent arg kwd doCrTab |	args size = 0 ifTrue: [aStream space; nextPutAll: key. ^self].	keywords := key keywords.	doCrTab := args size > 2				or: [{receiver} , args anySatisfy:						[:thisArg |						thisArg isBlockNode						or: [thisArg isMessageNode and: [thisArg precedence >= 3]]]].	1 to: (args size min: keywords size) do:		[:i |		arg := args at: i.		kwd := keywords at: i.		doCrTab			ifTrue: [aStream newLineTab: level+1. indent := 1] "newline after big args"			ifFalse: [aStream space. indent := 0].		aStream nextPutAll: kwd; space.		arg printWithClosureAnalysisOn: aStream			indent: level + 1 + indent			precedence: (precedence = 2 ifTrue: [1] ifFalse: [precedence])]! !


!MethodNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:32'!
         printOn: aStream	| selectorNode |	selectorNode _ self selectorNode.	precedence = 1		ifTrue:			[selectorNode isForFFICall				ifTrue: [selectorNode							printAsFFICallWithArguments: arguments							on: aStream							indent: 0]				ifFalse: [aStream nextPutAll: selectorNode key]]		ifFalse:			[selectorNode key keywords withIndexDo:				[:kwd :i | | arg |				arg _ arguments at: i.				i = 1 ifFalse: [ aStream space ].				aStream nextPutAll: kwd; space; nextPutAll: arg key ]].	comment ifNotNil: [		aStream newLineTab: 1.		self printCommentOn: aStream indent: 1].	block printTemporaries: temporaries on: aStream doPrior: [aStream newLineTab: 1].	primitive > 0 ifTrue:		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"			[aStream newLineTab: 1.			 self printPrimitiveOn: aStream]].	self printPropertiesOn: aStream.	self printPragmasOn: aStream.	aStream newLineTab: 1.	block printStatementsOn: aStream indent: 0! !

!MethodNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:32'!
   printPragmasOn: aStream	properties ifNil: [^self].	properties pragmas do: [ :pragma |		"Primitives are printed in printPrimitiveOn:; skip these"		(Parser primitivePragmaSelectors includes: pragma keyword) ifFalse:			[aStream newLineTab: 1.			 pragma printOn: aStream]]! !

!MethodNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:33'!
        printPropertiesOn: aStream	properties ifNil: [^self].	properties propertyKeysAndValuesDo:		[:prop :val|		aStream newLine; tab; nextPut: $<.		prop = #on:in:			ifTrue:				[prop keywords with: val do:					[:k :v | aStream nextPutAll: k; space; nextPutAll: v; space]]			ifFalse:				[prop = #on					ifTrue: [aStream nextPutAll: prop; nextPutAll:': '; nextPutAll: val] 					ifFalse: [aStream nextPutAll: prop; nextPutAll:': '; print: val]]. 		aStream nextPut: $>]! !

!MethodNode methodsFor: 'printing' stamp: 'jmv 3/13/2012 15:30'!
    printWithClosureAnalysisOn: aStream 	self ensureClosureAnalysisDone.	precedence = 1		ifTrue: 			[(self selector includesSubString: '()/')				ifTrue: [aStream nextPutAll: (self selector copyUpTo: $)).						arguments							do: [:arg| aStream nextPutAll: arg key]							separatedBy: [aStream nextPutAll: ', '].						aStream nextPut: $)]				ifFalse: [aStream nextPutAll: self selector]]  "no node for method selector"		ifFalse: 			[self selector keywords with: arguments do: 				[:kwd :arg | 				aStream nextPutAll: kwd; space.				arg printDefinitionForClosureAnalysisOn: aStream.				aStream space]].	comment == nil ifFalse: [			aStream newLineTab: 1.			 self printCommentOn: aStream indent: 1].	temporaries size > 0 ifTrue: [			aStream newLineTab: 1; nextPut: $|.			temporaries do: [:temp | 				aStream space.				temp printDefinitionForClosureAnalysisOn: aStream].			aStream space; nextPut: $|].	primitive > 0 ifTrue:		[(primitive between: 255 and: 519) ifFalse:  "Dont decompile quick prims  e.g, ^ self or ^instVar"			[aStream newLineTab: 1.			 self printPrimitiveOn: aStream]].	self printPropertiesOn: aStream.	self printPragmasOn: aStream.	aStream newLineTab: 1.	block printWithClosureAnalysisStatementsOn: aStream indent: 0! !


!PluggableListMorph methodsFor: 'menus' stamp: 'jmv 3/14/2012 07:53'!
          copyListToClipboard
	"Copy my items to the clipboard as a multi-line string"

	| stream |
	stream _ WriteStream on: (String new: list size * 40).
	list
		do: [:ea | stream nextPutAll: ea asString]
		separatedBy: [ stream newLine ].
	Clipboard storeObject: stream contents! !


!PopUpMenu methodsFor: 'accessing' stamp: 'jmv 3/13/2012 23:06'!
          frameHeight
	"Designed to avoid the entire frame computation (includes MVC form),
	since the menu may well end up being displayed in Morphic anyway."
	| nItems |
	nItems _ 1 + labelString lineCount.
	^ (nItems * Preferences standardMenuFont height) + 4 "border width"! !

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 3/14/2012 08:08'!
  startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skip: -1])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines _ allLabels size.
	lineArray ifNil: [lineArray _ Array new].
	nLinesPer _ segmentHeight // Preferences standardMenuFont height - 5.
	from := 1.
	[ true ] whileTrue: [
		to := (from + nLinesPer) min: nLines.
		subset := (allLabels copyFrom: from to: to) asOrderedCollection.
		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])
			before: subset first.
		subLines _ lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].
		subLines _ (Array with: 1) , subLines.
		index := (PopUpMenu labels: subset asStringWithCr lines: subLines)
					startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
		index = 1
			ifTrue: [from := to + 1.
					from > nLines ifTrue: [ from := 1 ]]
			ifFalse: [index = 0 ifTrue: [^ 0].
					^ from + index - 2]]! !


!CustomMenu methodsFor: 'construction' stamp: 'jmv 3/13/2012 22:15'!
             labels: aString lines: anArrayOrNil
	"This method allows the receiver to accept old-style SelectionMenu creation messages. It should be used only for backward compatibility during the MVC-to-Morphic transition. New code should be written using the other menu construction protocol such as addList:."

	| labelList linesArray |
	labelList _ aString lines.
	linesArray _ anArrayOrNil ifNil: [ #() ].
	1 to: labelList size do: [ :i |
		self add: (labelList at: i) action: (labelList at: i).
			(linesArray includes: i) ifTrue: [ self addLine ]].! !

!CustomMenu methodsFor: 'construction' stamp: 'jmv 3/13/2012 22:15'!
                        labels: labelList lines: linesArray selections: selectionsArray
	"This method allows the receiver to accept old-style SelectionMenu creation messages. It should be used only for backward compatibility during the MVC-to-Morphic transition. New code should be written using the other menu construction protocol such as addList:."
	"Labels can be either a sting with embedded crs, or a collection of strings."
	| labelArray |
	labelArray _ (labelList isMemberOf: String)
		ifTrue: [ labelList lines ]
		ifFalse: [ labelList ].
	1 to: labelArray size do: [ :i |
		self add: (labelArray at: i) action: (selectionsArray at: i).
		(linesArray includes: i) ifTrue: [ self addLine ]]! !

!CustomMenu methodsFor: 'private' stamp: 'jmv 3/13/2012 12:32'!
                         build	"Turn myself into an invokable ActionMenu."	| stream |	stream _ WriteStream on: (String new).	labels do: [:label | stream nextPutAll: label; newLine].	(labels isEmpty) ifFalse: [stream skip: -1].  "remove final cr"	super		labels: stream contents		lines: dividers! !


!PopUpMenu class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 12:41'!
                      labelArray: labelArray lines: lineArray	"Answer an instance of me whose items are in labelArray, with lines 	drawn after each item indexed by anArray. 2/1/96 sw"	labelArray isEmpty ifTrue: [self error: 'Menu must not be zero size'].	^ self		labels: (String streamContents: 			[:stream |			labelArray do: [:each | stream nextPutAll: each; newLine].			stream skip: -1 "remove last CR"])		lines: lineArray"Example:	(PopUpMenu labelArray: #('frog' 'and' 'toad') lines: #()) startUp"! !

!PopUpMenu class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 23:14'!
 withCaption: cap chooseFrom: labels
	"Simply put up a menu.  Get the args in the right order with the caption first.  labels may be either an array of items or a string with CRs in it.  May use backslashes for returns."

	(labels isKindOf: String) 
		ifTrue: [^ (self labels: labels withNewLines lines: nil) startUpWithCaption: cap withNewLines]
		ifFalse: [^ (self labelArray: labels lines: nil) startUpWithCaption: cap withNewLines]! !


!PositionableStream methodsFor: 'accessing' stamp: 'jmv 3/13/2012 11:22'!
                              crLfNextLine	"Answer next line (may be empty), or nil if at end.	Support any line ending character or pair of them"	| answer lineSeparators |	self atEnd ifTrue: [^nil].	lineSeparators _ {Character crCharacter. Character lfCharacter}.	answer _ self upToAny: lineSeparators.	(lineSeparators includes: self peek) ifTrue: [		self next ].	^answer! !

!PositionableStream methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:55'!
                  nextLine
	"Answer next line (may be empty), or nil if at end"

"	self atEnd ifTrue: [^nil].
	^self upTo: Character crCharacter
"
	^self crLfNextLine! !

!PositionableStream methodsFor: 'filein/out' stamp: 'jmv 3/13/2012 12:42'!
                            copyPreamble: preamble from: aStream at: pos 	"Look for a changeStamp for this method by peeking backward.	Write a method preamble, with that stamp if found."	| terminator last50 stamp i |	terminator := $!!.	"Look back to find stamp in old preamble, such as...	Polygon methodsFor: 'private' stamp: 'di 6/25/97 21:42' prior: 34957598!! "	aStream position: pos.	aStream backChunk.	"to beginning of method"	last50 := aStream backChunk.	"to get preamble"	aStream position: pos.	stamp := String new.	(i := last50 		findLastOccurrenceOfString: 'stamp:'		startingAt: 1) > 0 ifTrue: 		[ stamp := (last50 			copyFrom: i + 8			to: last50 size) copyUpTo: $' ].	"Write the new preamble, with old stamp if any."	self		newLine;		nextPut: terminator.	self nextChunkPut: (String streamContents: 			[ :strm | 			strm nextPutAll: preamble.			stamp size > 0 ifTrue: 				[ strm					nextPutAll: ' stamp: ';					print: stamp ] ]).	self newLine! !


!Preferences class methodsFor: 'fonts' stamp: 'jmv 3/13/2012 12:42'!
                             printStandardSystemFonts	"self printStandardSystemFonts"	#(standardListFont 	standardMenuFont windowTitleFont 	standardCodeFont standardButtonFont) do: [:selector |		| font |		font _ Preferences perform: selector.		Transcript			newLine; show: selector;			space; show: font printString]! !

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 3/14/2012 08:09'!
    alternativeBrowseIt
	^ self
		valueOfFlag: #alternativeBrowseIt
		ifAbsent: [ false ]! !


!Process methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:42'!
       longPrintOn: stream	| ctxt |	super printOn: stream.	stream newLine.	ctxt _ self suspendedContext.	[ctxt == nil] whileFalse: [		stream space.		ctxt printOn: stream.		stream newLine.		ctxt _ ctxt sender.	].! !


!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 3/13/2012 12:44'!
                dumpPigStackOn: aStream andClose: aBoolean	"Must run forked on its own process, so the monitored behavior is not affected too much"	| promise tally process depth stack suspendedContext |	promise := Processor tallyCPUUsageFor: 1 every: 10.	tally := promise value.	"WorldState addDeferredUIMessage: [self dumpTallyOnTranscript: tally]."	aStream nextPutAll: '====Al processes===='; newLine.	self dumpTally: tally on: aStream.	aStream newLine; nextPutAll: '====Process using most CPU===='; newLine.	process _ tally sortedCounts first value.	(100.0 * (tally occurrencesOf: process) / tally size) rounded printOn: aStream.	aStream		nextPutAll: ' % ';		nextPutAll: (process browserPrintStringWith: (ProcessBrowser nameAndRulesFor: process) first);		newLine.	depth _ 20.	stack _ process == Processor activeProcess		ifTrue: [thisContext stackOfSize: depth]		ifFalse: [suspendedContext _ process suspendedContext.			suspendedContext				ifNotNil: [suspendedContext stackOfSize: depth]].	stack 		ifNil: [ aStream nextPutAll: 'No context'; newLine]		ifNotNil: [			stack do: [ :c | 				c printOn: aStream.				aStream newLine]].	aBoolean ifTrue: [aStream close]! !

!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 3/13/2012 12:43'!
                   dumpTally: tally on: aStream	"tally is from ProcessorScheduler>>tallyCPUUsageFor:	Dumps lines with percentage of time, hash of process, and a friendly name"	tally sortedCounts do: [ :assoc | | procName |		procName _ (self nameAndRulesFor: assoc value) first.		(((assoc key / tally size) * 100.0) roundTo: 1) printOn: aStream.		aStream			nextPutAll: '%   ';			print: assoc value identityHash; space;			nextPutAll: procName;			newLine.	]! !


!ProgressInitiationException class methodsFor: 'examples and tests' stamp: 'jmv 3/13/2012 12:43'!
                         testWith	"test progress code WITH special handling of progress notifications"	^[ self testWithAdditionalInfo ] 		on: ProgressInitiationException		do: [ :ex | 			ex sendNotificationsTo: [ :min :max :curr |				Transcript show: min printString, '  ', max printString, '  ', curr printString; newLine			].		].! !


!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/13/2012 12:43'!
                      fileInDefinition	(self makeSureSuperClassExists: (definition copyUpTo: Character space)) ifFalse:[^self].	self hasDefinition ifTrue:[		Transcript newLine; show:'Defining ', self name.		self evaluate: self definition].	self exists ifFalse:[^self].	self hasComment ifTrue:[self realClass classComment: self comment].! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/13/2012 12:43'!
                   fileOut	| f |	f := (FileStream newFileNamed: self name,'.st').	self fileOutOn: f.	self needsInitialize ifTrue:[		f newLine; nextChunkPut: self name,' initialize'.	].	f close! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/13/2012 12:43'!
                               fileOutDefinitionOn: aStream	self hasDefinition ifFalse:[^self].	aStream nextChunkPut: self definition; newLine.	self hasComment ifTrue: [		aStream newLine.		self organization commentRemoteStr fileOutOn: aStream]! !

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 3/13/2012 12:43'!
                          fileOutMethods: aCollection on: aStream	"FileOut all methods with selectors taken from aCollection"	| categories |	categories := Dictionary new.	aCollection do:[:sel|		| cat |		cat := self organization categoryOfElement: sel.		cat = self removedCategoryName ifFalse:[			(categories includesKey: cat) 				ifFalse:[ categories at: cat put: Set new ].			(categories at: cat) add: sel].	].	categories associationsDo:[:assoc|		assoc value do: [ :sel |			aStream newLine.			(self sourceCode at: sel) fileOutOn: aStream.		].	].! !


!QuotedPrintableMimeConverter methodsFor: 'conversion' stamp: 'jmv 3/13/2012 12:43'!
            mimeDecode	"Do conversion reading from mimeStream writing to dataStream"	| line s c1 v1 c2 v2 |	[(line _ mimeStream nextLine) isNil] whileFalse: [		line _ line withoutTrailingBlanks.		line size = 0			ifTrue: [dataStream newLine]			ifFalse: [				s _ ReadStream on: line.				[dataStream nextPutAll: (s upTo: $=).				s atEnd] whileFalse: [					c1 _ s next. v1 _ c1 digitValue.					((v1 between: 0 and: 15) and: [s atEnd not])						ifFalse: [dataStream nextPut: $=; nextPut: c1]						ifTrue: [c2 _ s next. v2 _ c2 digitValue.							(v2 between: 0 and: 15)								ifFalse: [dataStream nextPut: $=; nextPut: c1; nextPut: c2]								ifTrue: [dataStream nextPut: (Character value: v1 * 16 + v2)]]].				line last = $= ifFalse: [dataStream newLine]]].	^ dataStream! !


!Random class methodsFor: 'testing' stamp: 'jmv 3/13/2012 12:44'!
                  bucketTest: randy	"Execute this:   Random bucketTest: Random new"	" A quick-and-dirty bucket test. Prints nbuckets values on theTranscript.	  Each should be 'near' the value of ntries. Any run with any value'far' from ntries	  indicates something is very wrong. Each run generates differentvalues.	  For a slightly better test, try values of nbuckets of 200-1000 ormore; go get coffee.	  This is a poor test; see Knuth.   Some 'OK' runs:		1000 1023 998 969 997 1018 1030 1019 1054 985 1003		1011 987 982 980 982 974 968 1044 976		1029 1011 1025 1016 997 1019 991 954 968 999 991		978 1035 995 988 1038 1009 988 993 976"	| nbuckets buckets ntrys slot |	nbuckets := 20.	buckets := Array new: nbuckets.	buckets atAllPut: 0.	ntrys :=  100.	ntrys*nbuckets timesRepeat: [		slot := (randy next * nbuckets) floor + 1.		buckets at: slot put: (buckets at: slot) + 1 ].	Transcript newLine.	1 to: nbuckets do: [ :nb |		Transcript show: (buckets at: nb) printString, ' ' ]! !


!ReferenceStream methodsFor: 'statistics' stamp: 'jmv 3/13/2012 12:44'!
                         statisticsOfRefs	"Analyze the information in references, the objects being written out"	| parents n kids nm ownerBags tallies owners objParent normalReferences |	normalReferences _ self references.	"Exclude unrealized weaks"	parents _ IdentityDictionary new: normalReferences size * 2.	n _ 0.	'Finding Owners...'	displayProgressAt: Sensor mousePoint	from: 0 to: normalReferences size	during: [:bar |	normalReferences keysDo:		[:parent | bar value: (n _ n+1).		kids _ parent class isFixed			ifTrue: [(1 to: parent class instSize) collect: [:i | parent instVarAt: i]]			ifFalse: [parent class isBits ifTrue: [Array new]					 ifFalse: [(1 to: parent basicSize) collect: [:i | parent basicAt: i]]].		(kids select: [:x | normalReferences includesKey: x])			do: [:child | parents at: child put: parent]]].	ownerBags _ Dictionary new.	tallies _ Bag new.	n _ 0.	'Tallying Owners...'	displayProgressAt: Sensor mousePoint	from: 0 to: normalReferences size	during: [:bar |	normalReferences keysDo:  "For each class of obj, tally a bag of owner classes"		[:obj | bar value: (n _ n+1).		nm _ obj class name.		tallies add: nm.		owners _ ownerBags at: nm ifAbsent: [ownerBags at: nm put: Bag new].		(objParent _ parents at: obj ifAbsent: nil) ifNotNil: [			owners add: objParent class name]]].	^ String streamContents: [ :strm | 		tallies sortedCounts do: [ :assn |			n _ assn key.  nm _ assn value.			owners _ ownerBags at: nm.			strm newLine; nextPutAll: nm; space; print: n.			owners size > 0 ifTrue: [				strm newLine; tab; print: owners sortedCounts]]]! !


!RelativeInstructionPrinter methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:44'!
                            print: instruction 	"Append to the receiver a description of the bytecode, instruction." 	stream tab: self indent.	labelling		ifTrue: [stream print: oldPC - method initialPC; space]		ifFalse: [stream tab].	stream tab: (innerIndents at: oldPC).	self printCode ifTrue:		[stream nextPut: $<.		 oldPC to: scanner pc - 1 do: 			[:i | | code |			code := (method at: i) radix: 16.			stream				nextPut: (code size < 2 ifTrue: [$0] ifFalse: [code at: 1]);				nextPut: code last;				space].		 stream skip: -1; nextPut: $>; space].	stream nextPutAll: instruction.	stream newLine.	labelling ifFalse:		[(labels at: scanner pc + 1) ~~ false ifTrue:			[stream nextPutAll: (labels at: scanner pc + 1); nextPut: $:; newLine]].	oldPC := scanner pc! !


!RemoteString methodsFor: 'private' stamp: 'jmv 3/13/2012 12:44'!
        string: aString onFileNumber: fileNumber	"Store this as my string if source files exist."	| theFile |	(SourceFiles at: fileNumber) ifNotNil: [		theFile _ SourceFiles at: fileNumber.		theFile setToEnd; newLine.		self string: aString onFileNumber: fileNumber toFile: theFile]! !


!SampledInstrument class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 12:44'!
            readLoudAndStaccatoInstrument: instName fromDirectory: orchestraDir	"SampledInstrument		readLoudAndStaccatoInstrument: 'oboe'		fromDirectory: 'Tosh:Sample Library:Orchestra'"	| sampleSetDir memBefore memAfter loud short snd |	sampleSetDir _ orchestraDir, ':', instName.	memBefore _ Smalltalk garbageCollect.	loud _ SampledInstrument new readSampleSetFrom: sampleSetDir, ' f'.	short _ SampledInstrument new readSampleSetFrom: sampleSetDir, ' stacc'.	memAfter _ Smalltalk garbageCollect.	Transcript show:		instName, ': ', (memBefore - memAfter) printString,		' bytes; ', memAfter printString, ' bytes left'; newLine.	AbstractSound soundNamed: instName, '-f&stacc' put:		(snd _ SampledInstrument new			allSampleSets: loud;			staccatoLoudAndSoftSampleSet: short).	"fix slow attacks"	snd allNotes do: [:n | n firstSample: (n findStartPointForThreshold: 500)].	AbstractSound soundNamed: instName, '-f' put:		(snd _ SampledInstrument new			allSampleSets: loud).	"fix slow attacks"	snd allNotes do: [:n | n firstSample: (n findStartPointForThreshold: 1000)].! !

!SampledInstrument class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 12:44'!
                readPizzInstrument: instName fromDirectory: orchestraDir	"SampledInstrument		readPizzInstrument: 'violin'		fromDirectory: 'Tosh:Sample Library:Orchestra'"	| sampleSetDir memBefore memAfter sampleSet snd |	sampleSetDir _ orchestraDir, ':', instName, ' pizz'.	memBefore _ Smalltalk garbageCollect.	sampleSet _ SampledInstrument new readSampleSetFrom: sampleSetDir.	memAfter _ Smalltalk garbageCollect.	Transcript show:		instName, ': ', (memBefore - memAfter) printString,		' bytes; ', memAfter printString, ' bytes left'; newLine.	AbstractSound soundNamed: instName, '-pizz' put:		(snd _ SampledInstrument new allSampleSets: sampleSet).	"fix slow attacks"	snd allNotes do: [:n |		n firstSample: (n findStartPointForThreshold: 1000)].	^ snd! !

!SampledInstrument class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 12:45'!
              readSimpleInstrument: instName fromDirectory: orchestraDir	"SampledInstrument		readSimpleInstrument: 'oboe'		fromDirectory: 'Tosh:Sample Library:Orchestra'"	| sampleSetDir memBefore memAfter sampleSet snd |	sampleSetDir _ orchestraDir, ':', instName, ' f'.	memBefore _ Smalltalk garbageCollect.	sampleSet _ SampledInstrument new readSampleSetFrom: sampleSetDir.	memAfter _ Smalltalk garbageCollect.	Transcript show:		instName, ': ', (memBefore - memAfter) printString,		' bytes; ', memAfter printString, ' bytes left'; newLine.	AbstractSound soundNamed: instName, '-f' put:		(snd _ SampledInstrument new allSampleSets: sampleSet).	"fix slow attacks"	snd allNotes do: [:n |		n firstSample: (n findStartPointForThreshold: 1000)].	^ snd! !


!Scanner methodsFor: 'public access' stamp: 'jmv 3/13/2012 11:46'!
     scanTokenPositionsIn: textOrString into: aBlock	"Evaluate aBlock with the start and end positions of all separate non-white-space tokens, including comments, in textOrString."	self initScannerForTokenization.	source := (ReadStream on: textOrString asString).	self step.	self step.	self scanAllTokenPositionsInto: aBlock	"| code |	code := '       #( 1 2 #( 3 4 ))  16r123 123 123.0  ', (Scanner sourceCodeAt: #scanTokenPositionsIn:into:).	Scanner new scanTokenPositionsIn: code into: [:start :end| Transcript cr; nextPut: $_; nextPutAll: (code copyFrom: start to: end); nextPut: $_; endEntry]"	"CodeDiffBuilder buildDisplayPatchFrom:  (Scanner sourceCodeAt: #scanTokenPositionsIn:into:) to:  ((Scanner sourceCodeAt: #scanTokenPositionsIn:into:) copyReplaceAll: String crString with: '')"	"CodeDiffBuilder buildDisplayPatchFrom:  'colorTable ^colorTable ifNil: [colorTable _ ST80ColorTable]' to:'colorTable ^colorTable ifNil: [colorTable _ ST80ColorTable]'"! !


!Parser methodsFor: 'error correction' stamp: 'jmv 3/13/2012 16:53'!
           pasteTempAtMethodLevel: name	| insertion delta theTextString  characterBeforeMark |	theTextString := requestor text string.	characterBeforeMark := theTextString at: tempsMark-1 ifAbsent: [$ ].	(theTextString at: tempsMark) = $| ifTrue: [  		"Paste it before the second vertical bar"		insertion := name, ' '.		characterBeforeMark isSeparator ifFalse: [insertion := ' ', insertion].		delta := 0.	] ifFalse: [		"No bars - insert some with CR, tab"		insertion := '| ' , name , ' |',String newLineString.		delta := 2.	"the bar and CR"		characterBeforeMark = Character tab ifTrue: [			insertion := insertion , String tab.			delta := delta + 1.	"the tab"		].	].	tempsMark := tempsMark +		(self substituteWord: insertion			wordInterval: (tempsMark to: tempsMark-1)			offset: 0) - delta! !

!Parser methodsFor: 'error correction' stamp: 'jmv 3/13/2012 23:13'!
                       removeUnusedTemps
	"Scan for unused temp names, and prompt the user about the prospect of removing each one found"

	| str madeChanges | 
	madeChanges := false.
	str := requestor text asString.
	((tempsMark between: 1 and: str size)
		and: [(str at: tempsMark) = $|]) ifFalse: [^ self].
	encoder unusedTempNames do:
		[:temp | | start end |
		(UnusedVariable name: temp) ifTrue:
			[(encoder lookupVariable: temp ifAbsent: []) isUndefTemp
				ifTrue:
					[end := tempsMark.
					["Beginning at right temp marker..."
					start := end - temp size + 1.
					end < temp size or: [temp = (str copyFrom: start to: end)
							"In Cuis, we chose to use 'tokenish not' and not 'isSeparator'"
							and: [(str at: start-1) tokenish not & (str at: end+1) tokenish not]]]
						whileFalse:
							["Search left for the unused temp"
							end := requestor nextTokenFrom: end direction: -1].
					end < temp size ifFalse:
						[(str at: start-1) = $  ifTrue: [start := start-1].
						requestor correctFrom: start to: end with: ''.
						str := str copyReplaceFrom: start to: end with: ''. 
						madeChanges := true.
						tempsMark := tempsMark - (end-start+1)]]
				ifFalse:
					[self inform:
'You''ll first have to remove the\statement where it''s stored into' withNewLines]]].
	madeChanges ifTrue: [ReparseAfterSourceEditing signal]! !


!Parser class methodsFor: 'class initialization' stamp: 'jmv 3/13/2012 23:13'!
                         initialize
		
	Preferences
		addPreference: #allowBlockArgumentAssignment 
		category: #compiler 
		default: false
		balloonHelp: 'If enabled, the compiler will allow assignment into block arguments.\This provides backward compatibility with the pre-closure compiler.' withNewLines.
	Preferences
		addPreference: #allowUnderscoreAssignments 
		category: #compiler 
		default: true
		balloonHelp: 'When true, $_ (left arrow / underscore) can be used as assignment operator'.
	Preferences
		addPreference: #allowUnderscoreSelectors 
		category: #compiler 
		default: false
		balloonHelp: 'When true, $_ (left arrow / underscore) can be used in selectors and variable names'! !


!ScorePlayer methodsFor: 'accessing' stamp: 'jmv 3/13/2012 12:45'!
                        settingsString	| s |	s _ WriteStream on: (String new: 1000).	s nextPutAll: 'player'; newLine.	s tab; nextPutAll: 'rate: ', self rate printString, ';'; newLine.	s tab; nextPutAll: 'overallVolume: ', self overallVolume printString, ';'; newLine.	1 to: self trackCount do: [:t |		s tab; nextPutAll: 'instrumentForTrack: ', t printString,			' put: (AbstractSound soundNamed: #default);'; newLine.		s tab; nextPutAll: 'mutedForTrack: ', t printString,			' put: ', (self mutedForTrack: t) printString, ';'; newLine.		s tab; nextPutAll: 'volumeForTrack: ', t printString,			' put: ', (self volumeForTrack: t) printString, ';'; newLine.		s tab; nextPutAll: 'panForTrack: ', t printString,			' put: ', (self panForTrack: t) printString, ';'; newLine].	s tab; nextPutAll: 'repeat: ', self repeat printString, '.'; newLine.	^ s contents! !


!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 3/13/2012 12:45'!
asStringWithCr	"Convert to a string with returns between items.  Elements are usually strings.	 Useful for labels for PopUpMenus.	#('something' 'there') asStringWithCr	"		^String streamContents: [ :labelStream |		self do: [ :each |			(each isKindOf: String)				ifTrue: [ labelStream nextPutAll: each; newLine ]				ifFalse: [					each printOn: labelStream.					labelStream newLine ]].		self size > 0 ifTrue: [ labelStream skip: -1 ]]! !


!Bitmap methodsFor: 'filing' stamp: 'jmv 3/13/2012 12:16'!
  storeBits: startBit to: stopBit on: aStream 	"Store my bits as a hex string, breaking the lines every 100 bytes or 	so to comply with the maximum line length limits of Postscript (255 	bytes). "	| lineWidth |	lineWidth := 0.	self		do: [:word | 			startBit				to: stopBit				by: -4				do: [:shift | 					aStream nextPut: (word >> shift bitAnd: 15) asHexDigit.					lineWidth := lineWidth + 1].			(lineWidth > 100)				ifTrue: [					aStream newLine.					lineWidth := 0]].	lineWidth > 0 ifTrue: [ aStream newLine ].! !


!CompiledMethod methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:32'!
     printPrimitiveOn: aStream	"Print the primitive on aStream"	| primIndex primDecl |	(primIndex := self primitive) = 0 ifTrue:		[^self].	primIndex = 120 ifTrue: "External call spec"		[^aStream print: (self literalAt: 1); newLine].	aStream nextPutAll: '<primitive: '.	primIndex = 117		ifTrue: [			primDecl := self literalAt: 1.			(primDecl at: 2) asString printOn: aStream.			(primDecl at: 1) ifNotNil: [ :moduleName |				aStream nextPutAll:' module: '.				moduleName asString printOn: aStream]]		ifFalse: [			aStream print: primIndex].	self primitiveErrorVariableName ifNotNil: [ :primitiveErrorVariableName |		aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].	aStream nextPut: $>; newLine! !

!CompiledMethod methodsFor: 'printing' stamp: 'jmv 3/13/2012 22:54'!
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

!CompiledMethod methodsFor: 'literals' stamp: 'jmv 3/13/2012 12:27'!
headerDescription	"Answer a description containing the information about the form of the 	receiver and the form of the context needed to run the receiver."	| s |	s := '' writeStream.	self header printOn: s.	s newLine; nextPutAll: '"primitive: '.	self primitive printOn: s.	s newLine; nextPutAll: ' numArgs: '.	self numArgs printOn: s.	s newLine; nextPutAll: ' numTemps: '.	self numTemps printOn: s.	s newLine; nextPutAll: ' numLiterals: '.	self numLiterals printOn: s.	s newLine; nextPutAll: ' frameSize: '.	self frameSize printOn: s.	s newLine; nextPutAll: ' isClosureCompiled: '.	self isBlueBookCompiled not printOn: s.	s nextPut: $"; newLine.	^ s contents! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 3/13/2012 22:32'!
  linesOfCode
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."
	| strm line lines |
	lines _ 0.
	strm _ ReadStream on: self getSource.
		[strm atEnd] whileFalse: [
			line _ strm crLfNextLine.
			line isEmpty ifFalse: [
				lines _ lines+1 ]].
	^lines! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 3/13/2012 12:28'!
      putSource: sourceStr fromParseNode: methodNode class: class category: catName	inFile: fileIndex priorMethod: priorMethod	^ self putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: [ :file |		class printCategoryChunk: catName on: file priorMethod: priorMethod.		file newLine ]! !

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 3/13/2012 12:28'!
                     putSource: sourceStr fromParseNode: methodNode class: class category: catName	withStamp: changeStamp inFile: fileIndex priorMethod: priorMethod	^ self putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: [ :file |			class				printCategoryChunk: catName				on: file				withStamp: changeStamp				priorMethod: priorMethod.			file newLine ]! !


!Heap class methodsFor: 'examples' stamp: 'jmv 3/13/2012 12:35'!
      heapExample	"Heap heapExample"	"Create a sorted collection of numbers, remove the elements	sequentially and add new objects randomly.	Note: This is the kind of benchmark a heap is designed for."	| n rnd array time |	n := 5000. "# of elements to sort"	rnd := Random new.	array := (1 to: n) collect:[:i| rnd next].	"First, the heap version"	time := Time millisecondsToRun:[| sorted |		sorted := Heap withAll: array.		1 to: n do: [ :i | 			sorted removeFirst.			sorted add: rnd next].	].	Transcript newLine; show:'Time for Heap: ', time printString,' msecs'.	"The quicksort version"	time := Time millisecondsToRun:[| sorted |		sorted := SortedCollection withAll: array.		1 to: n do:[:i| 			sorted removeFirst.			sorted add: rnd next].	].	Transcript newLine; show:'Time for SortedCollection: ', time printString,' msecs'.! !

!Heap class methodsFor: 'examples' stamp: 'jmv 3/13/2012 12:36'!
                 heapSortExample	"Heap heapSortExample"	"Sort a random collection of Floats and compare the results with	SortedCollection (using the quick-sort algorithm) and 	ArrayedCollection>>mergeSortFrom:to:by: (using the merge-sort algorithm)."	| n rnd array time |	n := 10000. "# of elements to sort"	rnd := Random new.	array := (1 to: n) collect:[:i| rnd next].	"First, the heap version"	time := Time millisecondsToRun: [		| sorted |		sorted := Heap withAll: array.		1 to: n do: [ :i | sorted removeFirst ]].	Transcript newLine; show:'Time for heap-sort: ', time printString,' msecs'.	"The quicksort version"	time := Time millisecondsToRun:[| sorted |		sorted := SortedCollection withAll: array.	].	Transcript newLine; show:'Time for quick-sort: ', time printString,' msecs'.	"The merge-sort version"	time := Time millisecondsToRun:[		array mergeSortFrom: 1 to: array size by: [:v1 :v2| v1 <= v2].	].	Transcript newLine; show:'Time for merge-sort: ', time printString,' msecs'.! !


!SimpleEditor methodsFor: 'typing support' stamp: 'jmv 3/13/2012 10:31'!
                 dispatchOn: aKeyboardEvent	"Carry out the action associated with this character, if any."	| asciiValue |	asciiValue _ aKeyboardEvent keyValue.	"Control keys are handled by KeystrokeActions even if they have any modifiers"	(asciiValue >= 32 and: [		aKeyboardEvent commandAltKeyPressed ]) ifTrue: [		^self perform: (self cmdActions at: asciiValue + 1) with: aKeyboardEvent ].	"We don't support multiple lines. Therefore, we don't process return as a #normalCharacter:"	aKeyboardEvent isReturnKey ifTrue: [		^ true].	^ self perform: (KeystrokeActions at: asciiValue + 1) with: aKeyboardEvent! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 3/13/2012 09:52'!
                          next	"Really write three objects: (version, class structure, object). But only when called from the outside.  "	| version ss object |	^ topCall		ifNil: [ 			topCall _ #marked.			version _ super next.			version class == SmallInteger ifFalse: [^ version].					"version number, else just a regular object, not in our format, "			ss _ super next.			ss class == Array ifFalse: [^ ss].  "just a regular object"			(ss at: 1) = 'class structure' ifFalse: [^ ss].			structures _ ss at: 2.			superclasses _ (ss size > 3 and: [(ss at: 3) = 'superclasses']) 				ifTrue: [ss at: 4]		"class name -> superclass name"				ifFalse: [Dictionary new].			(self verifyStructure = 'conversion method needed') ifTrue: [^ nil].			object _ super next.	"all the action here"			topCall _ nil.	"reset it"			object]		ifNotNil: [			super next]! !

!SmartRefStream methodsFor: 'read write' stamp: 'jmv 3/13/2012 12:45'!
                nextPut: anObject	"Really write three objects: (version, class structure, object).  But only when called from the outside.  If any instance-specific classes are present, prepend their source code.  byteStream will be in fileOut format.	You can see an analysis of which objects are written out by doing: 	(SmartRefStream statsOfSubObjects: anObject)	(SmartRefStream tallyOfSubObjects: anObject)	(SmartRefStream subObjects: anObject ofClass: aClass)"| info |topCall	ifNil: [		topCall _ anObject.		'Please wait while objects are counted' 			displayProgressAt: Sensor mousePoint			from: 0 to: 10			during: [:bar | info _ self instVarInfo: anObject].		byteStream binary.		'Writing an object file' displayProgressAt: Sensor mousePoint			from: 0 to: objCount*4	"estimate"			during: [:bar |				objCount _ 0.				progressBar _ bar.				self setStream: byteStream reading: false.					"set basePos, but keep any class renames"				super nextPut: ReferenceStream versionCode.				super nextPut: info.				super nextPut: anObject.		"<- the real writing"				].					"Note: the terminator, $!!, is not doubled inside object data"		"references is an IDict of every object that got written"		byteStream ascii.		byteStream nextPutAll: '!!'; newLine; newLine.		byteStream padToEndWith: $ .	"really want to truncate file, but can't"		topCall _ progressBar _ nil]	"reset it"	ifNotNil: [		super nextPut: anObject.		progressBar ifNotNil: [progressBar value: (objCount _ objCount + 1)]].! !

!SmartRefStream methodsFor: 'read write' stamp: 'jmv 3/13/2012 23:14'!
                               verifyStructure
	"Compare the incoming inst var name lists with the existing classes.  Prepare tables that will help to restructure those who need it (renamed, reshaped, steady).    If all superclasses are recorded in the file, only compare inst vars of this class, not of superclasses.  They will get their turn.  "


	| newClass newList oldList converting |

	self flag: #bobconv.	

	converting _ OrderedCollection new.
	structures keysDo: [:nm "an old className (symbol)" |
		"For missing classes, there needs to be a method in SmartRefStream like 
			#rectangleoc2 that returns the new class."
		newClass _ self mapClass: nm.	   "does (renamed at: nm put: newClass name)"
		newClass class == String ifTrue: [^ newClass].  "error, fileIn needed"
		newList _ (Array with: newClass classVersion), (newClass allInstVarNames).
		oldList _ structures at: nm.
		newList = oldList 
			ifTrue: [steady add: newClass]  "read it in as written"
			ifFalse: [converting add: newClass name]
	].
	false & converting isEmpty not ifTrue: ["debug" 
			self inform: 'These classes are being converted from existing methods:\' withNewLines,
				converting asArray printString].
! !

!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 3/13/2012 12:45'!
 writeClassRename: newName was: oldName	"Write a method that tells which modern class to map instances to."	| oldVer sel code |	oldVer _ self versionSymbol: (structures at: oldName).	sel _ oldName asString.	sel at: 1 put: (sel at: 1) asLowercase.	sel _ sel, oldVer.	"i.e. #rectangleoc4"	code _ WriteStream on: (String new: 500).	code nextPutAll: sel; newLine.	code newLine; tab; nextPutAll: '^ ', newName.	"Return new class"	self class compile: code contents classified: 'conversion'.! !

!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 3/13/2012 12:49'!
                         writeClassRenameMethod: sel was: oldName fromInstVars: oldList 	"The class coming is unknown.  Ask the user for the existing class it maps to.  If got one, write a method, and restart the obj fileIn.  If none, write a dummy method and get the user to complete it later.  "	| tell choice newName answ code oldVer newList newVer instSel |	self flag: #bobconv.	tell := 'Reading an instance of ' , oldName 				, '.Which modern class should it translate to?'.	answ := (PopUpMenu 				labels: 'Let me type the name nowLet me think about itLet me find a conversion file on the disk') 					startUpWithCaption: tell.	answ = 1 		ifTrue: [			tell := 'Name of the modern class {1} should translate to:' format: {oldName}.			choice := FillInTheBlank request: tell.	"class name"			choice size = 0 				ifTrue: [answ := 'conversion method needed']				ifFalse: 					[newName := choice.					answ := Smalltalk at: newName asSymbol								ifAbsent: ['conversion method needed'].					answ class == String 						ifFalse: [renamed at: oldName asSymbol put: answ name]]].	answ = 3 | (answ = 0) 		ifTrue: [			self close.			^'conversion method needed'].	answ = 2 ifTrue: [answ := 'conversion method needed'].	answ = 'conversion method needed' 		ifTrue: [			self close.			newName := 'PutNewClassHere'].	answ class == String 		ifFalse: 			[oldVer := self versionSymbol: (structures at: oldName).			newList := (Array with: answ classVersion) , answ allInstVarNames.			newVer := self versionSymbol: newList.			instSel := 'convert' , oldVer , ':' , newVer , ':'].	code := WriteStream on: (String new: 500).	code		nextPutAll: sel;		newLine.	answ class == String 		ifFalse: [			code				newLine;				tab;				nextPutAll: 'reshaped at: #' , oldName , ' put: #' , instSel , '.'.			code				newLine;				tab;				tab;				nextPutAll: '"Be sure to define that conversion method in class ' 							, answ name , '"'].	code		newLine;		tab;		nextPutAll: '^ ' , newName.	"Return new class"	self class compile: code contents classified: 'conversion'.	newName = 'PutNewClassHere' 		ifTrue: [			self 				inform: 'Please complete the following method and then read-in the object file again.'.			Smalltalk browseAllImplementorsOf: sel asSymbol].	self flag: #violateBasicLayerPrinciples.	"SmartRefStream should not refer to UI!!!!!!!!!! (sd)"	"The class version number only needs to change under one specific circumstance.  That is when the first letters of the instance variables have stayed the same, but their meaning has changed.  A conversion method is needed, but this system does not know it.  	If this is true for class Foo, define classVersion in Foo class.  	Beware of previous object fileouts already written after the change in meaning, but before bumping the version number.  They have the old (wrong) version number, say 2.  If this is true, your method must be able to test the data and successfully read files that say version 2 but are really 3."	^answ! !

!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 3/13/2012 23:14'!
 writeConversionMethod: sel class: newClass was: oldName fromInstVars: oldList to: newList
	"The method convertToCurrentVersion:refStream: was not found in newClass.  Write a default conversion method for the author to modify."

	| code newOthers oldOthers copied |

	code _ WriteStream on: (String new: 500).
	code nextPutAll: 'convertToCurrentVersion: varDict refStream: smartRefStrm'; newLine; tab.
	newOthers _ newList asOrderedCollection "copy".
	oldOthers _ oldList asOrderedCollection "copy".
	copied _ OrderedCollection new.
	newList do: [:instVar |
		(oldList includes: instVar) ifTrue: [
			instVar isInteger ifFalse: [copied add: instVar].
			newOthers remove: instVar.
			oldOthers remove: instVar]].
	code nextPutAll: '"These variables are automatically stored into the new instance '.
	code nextPutAll: copied asArray printString; nextPut: $. .
	code newLine; tab; nextPutAll: 'This method is for additional changes.'; 
		nextPutAll: ' Use statements like (foo _ varDict at: ''foo'')."'; newLine; newLine; tab.
	(newOthers size = 0) & (oldOthers size = 0) ifTrue: [^ self].
		"Instance variables are the same.  Only the order changed.  No conversion needed."
	(newOthers size > 0) ifTrue: [code nextPutAll: '"New variables: ', newOthers asArray printString, '  If a non-nil value is needed, please assign it."\' withNewLines].
	(oldOthers size > 0) ifTrue: [code nextPutAll: '	"These are going away ', oldOthers asArray printString, '.  Possibly store their info in some other variable?"'].

	code newLine; tab.
	code nextPutAll: '^ super convertToCurrentVersion: varDict refStream: smartRefStrm'.
	newClass compile: code contents classified: 'object fileIn'.


	"If you write a conversion method beware that the class may need a version number change.  This only happens when two conversion methods in the same class have the same selector name.  (A) The inst var lists of the new and old versions intials as some older set of new and old inst var lists.  or (B) Twice in a row, the class needs a conversion method, but the inst vars stay the same the whole time.  (For an internal format change.)
	If either is the case, fileouts already written with the old (wrong) version number, say 2.  Your method must be able to read files that say version 2 but are really 3, until you expunge the erroneous version 2 files from the universe."

 ! !

!SmartRefStream methodsFor: 'class changed shape' stamp: 'jmv 3/13/2012 12:46'!
                           writeConversionMethodIn: newClass fromInstVars: oldList to: newList renamedFrom: oldName	"The method convertToCurrentVersion:refStream: was not found in newClass.  Write a default conversion method for the author to modify.  If method exists, append new info into the end."	| code newOthers oldOthers copied newCode |	newOthers _ newList asOrderedCollection "copy".	oldOthers _ oldList asOrderedCollection "copy".	copied _ OrderedCollection new.	newList do: [:instVar |		(oldList includes: instVar) ifTrue: [			instVar isInteger ifFalse: [copied add: instVar].			newOthers remove: instVar.			oldOthers remove: instVar]].	code _ WriteStream on: (String new: 500).	code newLine; newLine; tab; nextPutAll: '"From ', SystemVersion current version, ' [', Smalltalk lastUpdateString;			nextPutAll: '] on ', Date today printString, '"'; newLine.	code tab; nextPutAll: '"These variables are automatically stored into the new instance: '.	code nextPutAll: copied asArray printString; nextPut: $.; newLine.	code tab; nextPutAll: 'Test for this particular conversion.'; 		nextPutAll: '  Get values using expressions like (varDict at: ''foo'')."'; newLine; newLine.	(newOthers size = 0) & (oldOthers size = 0) & (oldName == nil) ifTrue: [^ self].		"Instance variables are the same.  Only the order changed.  No conversion needed."	(newOthers size > 0) ifTrue: [		code tab; nextPutAll: '"New variables: ', newOthers asArray printString, 			'.  If a non-nil value is needed, please assign it."'; newLine].	(oldOthers size > 0) ifTrue: [		code tab; nextPutAll: '"These are going away ', oldOthers asArray printString, 			'.  Possibly store their info in some other variable?"'; newLine].	oldName ifNotNil: [		code tab; nextPutAll: '"Test for instances of class ', oldName, '.'; newLine.		code tab; nextPutAll: 'Instance vars with the same name have been moved here."'; newLine.		].	code tab; nextPutAll: '"Move your code above the ^ super...  Delete extra comments."'; newLine. 	(newClass includesSelector: #convertToCurrentVersion:refStream:) 		ifTrue: ["append to old methods"			newCode _ (newClass sourceCodeAt: #convertToCurrentVersion:refStream:),				code contents]		ifFalse: ["new method"			newCode _ 'convertToCurrentVersion: varDict refStream: smartRefStrm',				code contents, 				'	^ super convertToCurrentVersion: varDict refStream: smartRefStrm'].	newClass compile: newCode classified: 'object fileIn'.	"If you write a conversion method beware that the class may need a version number change.  This only happens when two conversion methods in the same class have the same selector name.  (A) The inst var lists of the new and old versions intials as some older set of new and old inst var lists.  or (B) Twice in a row, the class needs a conversion method, but the inst vars stay the same the whole time.  (For an internal format change.)	If either is the case, fileouts already written with the old (wrong) version number, say 2.  Your method must be able to read files that say version 2 but are really 3, until you expunge the erroneous version 2 files from the universe." ! !

!SmartRefStream methodsFor: 'import image segment' stamp: 'jmv 3/13/2012 23:14'!
                          checkFatalReshape: setOfClasses
	| suspects oldInstVars newInstVars bad className |
	"Inform the user if any of these classes were reshaped.  A block has a method from the old system whose receiver is of this class.  The method's inst var references might be wrong.  OK if inst vars were only added."

	self flag: #bobconv.	

	setOfClasses isEmpty ifTrue: [^ self].
	suspects _ OrderedCollection new.
	setOfClasses do: [:aClass |
		className _ renamed keyAtValue: aClass name ifAbsent: [aClass name].
		oldInstVars _ (structures at: className ifAbsent: [#(0)]) allButFirst.		"should be there"
		newInstVars _ aClass allInstVarNames.
		oldInstVars size > newInstVars size ifTrue: [bad _ true].
		oldInstVars size = newInstVars size ifTrue: [
			bad _ oldInstVars ~= newInstVars].
		oldInstVars size < newInstVars size ifTrue: [
			bad _ oldInstVars ~= (newInstVars copyFrom: 1 to: oldInstVars size)].
		bad ifTrue: [suspects add: aClass]].

	suspects isEmpty ifFalse: [
		self inform: ('Imported foreign methods will run on instances of:\',
			suspects asArray printString, 
			'\whose shape has changed.  Errors may occur.') withNewLines].! !


!SmartRefStream class methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 12:47'!
                 cleanUpCategories	| list valid removed newList newVers |	"Look for all conversion methods that can't be used any longer.  Delete them."	" SmartRefStream cleanUpCategories "	"Two part selectors that begin with convert and end with a digit."	"convertasossfe0: varDict asossfeu0: smartRefStrm"	list _ Symbol selectorsContaining: 'convert'.	list _ list select: [:symb | (symb beginsWith: 'convert') & (symb allButLast last isDigit)				ifTrue: [(symb numArgs = 2)]				ifFalse: [false]].	valid _ 0.  removed _ 0.	list do: [:symb |		(Smalltalk allClassesImplementing: symb) do: [:newClass |			newList _ (Array with: newClass classVersion), (newClass allInstVarNames).			newVers _ self new versionSymbol: newList.			(symb endsWith: (':',newVers,':')) 				ifFalse: [					"method is useless because can't convert to current shape"					newClass removeSelector: symb.	"get rid of it"					removed _ removed + 1]				ifTrue: [valid _ valid + 1]]].	Transcript		newLine;		show: 'Removed: ';		print: removed; 		show: '		Kept: ';		print: valid;		show: ' '! !


!Socket class methodsFor: 'tests' stamp: 'jmv 3/13/2012 12:51'!
                  loopbackTest	"Send data from one socket to another on the local machine.	Tests most of the socket primitives."	"100 timesRepeat: [Socket loopbackTest]"	| sock1 sock2 bytesToSend sendBuf receiveBuf done bytesSent bytesReceived t extraBytes packetsSent packetsRead |	Transcript		newLine;		show: 'starting loopback test';		newLine.	Transcript		show: '---------- Connecting ----------';		newLine.	self initializeNetwork.	sock1 := self new.	sock2 := self new.	sock1 listenOn: 54321.	sock2 connectTo: NetNameResolver localHostAddress port: 54321.	sock1 waitForConnectionFor: self standardTimeout.	sock2 waitForConnectionFor: self standardTimeout.	sock1 isConnected ifFalse: [self error: 'sock1 not connected'].	sock2 isConnected ifFalse: [self error: 'sock2 not connected'].	Transcript		show: 'connection established';		newLine.	bytesToSend := 5000000.	sendBuf := String new: 5000 withAll: $x.	receiveBuf := String new: 50000.	done := false.	packetsSent := packetsRead := bytesSent := bytesReceived := 0.	t := Time millisecondsToRun: 					[[done] whileFalse: 							[(sock1 sendDone and: [bytesSent < bytesToSend]) 								ifTrue: 									[packetsSent := packetsSent + 1.									bytesSent := bytesSent + (sock1 sendSomeData: sendBuf)].							sock2 dataAvailable 								ifTrue: 									[packetsRead := packetsRead + 1.									bytesReceived := bytesReceived + (sock2 receiveDataInto: receiveBuf)].							done := bytesSent >= bytesToSend and: [bytesReceived = bytesSent]]].	Transcript		show: 'closing connection';		newLine.	sock1 waitForSendDoneFor: self standardTimeout.	sock1 close.	sock2 waitForDisconnectionFor: self standardTimeout.	extraBytes := sock2 discardReceivedData.	extraBytes > 0 		ifTrue: [			Transcript				show: ' *** received ' , extraBytes size printString , ' extra bytes ***';				newLine].	sock2 close.	sock1 waitForDisconnectionFor: self standardTimeout.	sock1 isUnconnectedOrInvalid ifFalse: [self error: 'sock1 not closed'].	sock2 isUnconnectedOrInvalid ifFalse: [self error: 'sock2 not closed'].	Transcript		show: '---------- Connection Closed ----------';		newLine.	sock1 destroy.	sock2 destroy.	Transcript		show: 'loopback test done; time = ' , t printString;		newLine.	Transcript		show: (bytesToSend asFloat / t roundTo: 0.01) printString 					, '* 1000 bytes/sec';		newLine.	Transcript endEntry! !

!Socket class methodsFor: 'tests' stamp: 'jmv 3/13/2012 12:51'!
       sendTest	"Send data to the 'discard' socket of the given host.	Tests the speed of one-way data transfers across the	network to the given host. Note that most hosts	do not run a discard server."	"Socket sendTest"	| sock bytesToSend sendBuf bytesSent t serverName serverAddr |	Transcript newLine; show: 'starting send test'; newLine.	self initializeNetwork.	serverName := FillInTheBlank request: 'What is the destination server?' initialAnswer: 'create.ucsb.edu'.	serverAddr := NetNameResolver addressForName: serverName timeout: 10.	serverAddr ifNil: [		^self inform: 'Could not find an address for ' , serverName].	sock := self new.	Transcript show: '---------- Connecting ----------';newLine.	sock connectTo: serverAddr port: 9.	sock isConnected ifFalse: [		sock destroy.		^self inform: 'could not connect'].	Transcript show: 'connection established; sending data'; newLine.	bytesToSend := 1000000.	sendBuf := String new: 64 * 1024 withAll: $x.	bytesSent := 0.	t := Time millisecondsToRun: 					[[bytesSent < bytesToSend] whileTrue: 							[sock sendDone 								ifTrue: [bytesSent := bytesSent + (sock sendSomeData: sendBuf)]]].	sock waitForSendDoneFor: self standardTimeout.	sock destroy.	Transcript show: '---------- Connection Closed ----------'; newLine;		show: 'send test done; time = ' , t printString; newLine;		show: (bytesToSend asFloat / t roundTo: 0.01) printString, ' * 1000 bytes/sec'; newLine; endEntry! !


!SocketStream methodsFor: 'stream in' stamp: 'jmv 3/13/2012 11:46'!
           nextLineCrLf	^self upToAll: String crlfString! !

!SocketStream methodsFor: 'stream in' stamp: 'jmv 3/13/2012 11:55'!
         nextLineLf	^self upToAll: String lfString! !

!SocketStream methodsFor: 'stream out' stamp: 'jmv 3/13/2012 11:46'!
           sendCommand: aString	"Sends a String ending it with CR LF and then flush	causing it to block until sent."	self nextPutAll: aString, String crlfString; flush! !

!SocketStream methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:47'!
                        debug	"Display debug info."	| data |	data := self inBufferSize.	^String streamContents: [:s |		s			nextPutAll: 'Buffer size: ', inBuffer size asString; newLine;			nextPutAll: 'InBuffer data size: ', data asString; newLine;			nextPutAll: 'In data (20):', (inBuffer copyFrom: lastRead + 1 to: lastRead + (data min: 20)); newLine;			nextPutAll: 'OutBuffer data size: ', (outNextToWrite - 1) asString; newLine;			nextPutAll: 'Out data (20):', (outBuffer copyFrom: 1 to: ((outNextToWrite - 1) min: 20)); newLine]! !


!SocketStream class methodsFor: 'example' stamp: 'jmv 3/13/2012 12:47'!
            finger: userName	"SocketStream finger: 'stp'"	| addr s |	addr := NetNameResolver promptUserForHostAddress.	s := SocketStream openConnectionToHost: addr port: 79.  "finger port number"	Transcript show: '---------- Connecting ----------'; newLine.	s sendCommand: userName.	Transcript show: s nextLine.	s close.	Transcript show: '---------- Connection Closed ----------'; newLine; endEntry.! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 3/13/2012 12:47'!
            compareTallyIn: beforeFileName to: afterFileName	"SpaceTally new compareTallyIn: 'tally' to: 'tally2'"	| answer s beforeDict a afterDict allKeys before after diff |	beforeDict _ Dictionary new.	s _ FileDirectory default fileNamed: beforeFileName.	 [s atEnd ] whileFalse: [		a _ Array readFrom: s nextLine.		beforeDict at: a first put: a allButFirst.	].	s close.	afterDict _ Dictionary new.	s _ FileDirectory default fileNamed: afterFileName.	[ s atEnd ] whileFalse: [		a _ Array readFrom: s nextLine.		afterDict at: a first put: a allButFirst.	].	s close.	answer _ WriteStream on: String new.	allKeys _ (Set new addAll: beforeDict keys; addAll: afterDict keys; yourself) asArray sort.	allKeys do: [ :each |		before _ beforeDict at: each ifAbsent: [#(0 0 0)].		after _ afterDict at: each ifAbsent: [#(0 0 0)].		diff _ before with: after collect: [ :vBefore :vAfter | vAfter - vBefore].		diff = #(0 0 0) ifFalse: [			answer nextPutAll: each, '  ', diff printString; newLine.		].	].	TextModel new contents: answer contents; openLabel: 'space diffs'! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 3/13/2012 12:49'!
          printSpaceAnalysis: threshold on: aStream	"	SpaceTally new printSpaceAnalysis: 1 on:(FileStream forceNewFileNamed: 'STspace.text')	"	"sd-This method should be rewrote to be more coherent within the rest of the class 	ie using preAllocate and spaceForInstanceOf:"	"If threshold > 0, then only those classes with more than that number	of instances will be shown, and they will be sorted by total instance space.	If threshold = 0, then all classes will appear, sorted by name."	| codeSpace instCount instSpace totalCodeSpace totalInstCount totalInstSpace eltSize n totalPercent percent |	Smalltalk garbageCollect.	totalCodeSpace _ totalInstCount _ totalInstSpace _ n _ 0.	results _ OrderedCollection new: Smalltalk classNames size.	'Taking statistics...'		displayProgressAt: Sensor mousePoint		from: 0 to: Smalltalk classNames size		during: [ :bar |			Smalltalk allClassesDo: [ :cl |				codeSpace _ cl spaceUsed.				bar value: (n _ n+1).				Smalltalk garbageCollectMost.				instCount _ cl instanceCount.				instSpace _ (cl indexIfCompact > 0 ifTrue: [4] ifFalse: [8]) * instCount. "Object headers""Warning: The 3rd header word for big objects is not considered!!"				cl isVariable					ifTrue: [						eltSize _ cl isBytes ifTrue: [1] ifFalse: [4].						cl allInstancesDo: [ :x |							instSpace _ instSpace + (x basicSize * eltSize)]]					ifFalse: [instSpace _ instSpace + (cl instSize * instCount * 4)].				results add: (SpaceTallyItem analyzedClassName: cl name codeSize: codeSpace instanceCount:  instCount spaceForInstances: instSpace).				totalCodeSpace _ totalCodeSpace + codeSpace.				totalInstCount _ totalInstCount + instCount.				totalInstSpace _ totalInstSpace + instSpace]].	totalPercent _ 0.0.	aStream timeStamp.	aStream		nextPutAll: ('Class' padded: #right to: 30 with: $ );		nextPutAll: ('code space' padded: #left to: 12 with: $ );		nextPutAll: ('# instances' padded: #left to: 12 with: $ );		nextPutAll: ('inst space' padded: #left to: 12 with: $ );		nextPutAll: ('percent' padded: #left to: 8 with: $ ); newLine.	threshold > 0 ifTrue: [		"If inst count threshold > 0, then sort by space"		results _ (results select: [:s | s instanceCount >= threshold or: [s spaceForInstances > (totalInstSpace // 500)]])			asArray sort: [:s :s2 | s spaceForInstances > s2 spaceForInstances]].	results do: [:s |		aStream			nextPutAll: (s analyzedClassName padded: #right to: 30 with: $ );			nextPutAll: (s codeSize printString padded: #left to: 12 with: $ );			nextPutAll: (s instanceCount printString padded: #left to: 12 with: $ );			nextPutAll: (s spaceForInstances printString padded: #left to: 14 with: $ ).		percent _ s spaceForInstances*100.0/totalInstSpace roundTo: 0.1.		totalPercent _ totalPercent + percent.		percent >= 0.1 ifTrue: [			aStream nextPutAll: (percent printString padded: #left to: 8 with: $ )].		aStream newLine].	aStream		newLine; nextPutAll: ('Total' padded: #right to: 30 with: $ );		nextPutAll: (totalCodeSpace printString padded: #left to: 12 with: $ );		nextPutAll: (totalInstCount printString padded: #left to: 12 with: $ );		nextPutAll: (totalInstSpace printString padded: #left to: 14 with: $ );		nextPutAll: ((totalPercent roundTo: 0.1) printString padded: #left to: 8 with: $ ).! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 3/13/2012 22:56'!
             printSpaceDifferenceFrom: fileName1 to: fileName2
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		SpaceTally new printSpaceAnalysis: 0 on: 'STspace.text1'.
			--- do something that uses space here ---
		SpaceTally new printSpaceAnalysis: 0 on: 'STspace.text2'.
		SpaceTally new printSpaceDifferenceFrom: 'STspace.text1' to: 'STspace.text2'
"
	| f coll1 coll2 item |
	f _ FileStream readOnlyFileNamed: fileName1.
	coll1 _ OrderedCollection new.
	[f atEnd] whileFalse: [coll1 add: f crLfNextLine].
	f close.
	f _ FileStream readOnlyFileNamed: fileName2.
	coll2 _ OrderedCollection new.
	[f atEnd] whileFalse: [
		item _ f crLfNextLine.
		((coll1 includes: item) and: [(item endsWith: 'percent') not])
			ifTrue: [coll1 remove: item]
			ifFalse: [coll2 add: item]].
	f close.
	(TextModel new contents: (String streamContents: 
			[ :s | 
			s nextPutAll: fileName1; newLine.
			coll1 do: [:x | s nextPutAll: x; newLine].
			s newLine; newLine.
			s nextPutAll: fileName2; newLine.
			coll2 do: [:x | s nextPutAll: x; newLine]]))
		openLabel: 'Differential Space Analysis'.
! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 3/13/2012 12:48'!
               saveTo: aFileName	"| st |	st := SpaceTally new.	st spaceTally: (Array with: TextMorph with: Point).	st saveTo: 'spaceTally2'"	| s |	(FileDirectory default fileExists: aFileName) ifTrue: [		FileDirectory default deleteFileNamed: aFileName].	s _ FileDirectory default fileNamed: aFileName.	results do: [:each | s nextPutAll: each analyzedClassName asString ; 						nextPutAll: ' '; nextPutAll: each codeSize printString; 						nextPutAll: ' '; nextPutAll: each instanceCount printString; 						nextPutAll: ' '; nextPutAll: each spaceForInstances printString; newLine].	s close! !


!StrikeFont methodsFor: 'character shapes' stamp: 'jmv 3/13/2012 21:50'!
      makeControlCharsVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self characterFormAt: (Character space).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
	self characterFormAt: (Character value: 134) put: glyph.
	
	"Keep tab(9), lf(10), cr(13) and space(32) transparent or whatever the user chose"
	#(0 1 2 3 4 5 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31)
		do: [ :ascii |
			characterToGlyphMap at: ascii + 1 put: 134 ]! !


!String methodsFor: 'accessing' stamp: 'jmv 3/14/2012 08:14'!
         indexOfAnyOf: aCharacterSet  startingAt: start ifAbsent: aBlock	"returns the index of the first character in the given set, starting from start"	| answer |	answer _ String findFirstInString: self inSet: aCharacterSet byteArrayMap startingAt: start.	^answer = 0 		ifTrue: [ aBlock value ]		ifFalse: [ answer]! !

!String methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:06'!
  lineCount
	"Answer the number of lines represented by the receiver, where every cr adds one line.  5/10/96 sw"


	| i |
	i _ 0.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		i _ i + 1 ].
	^i

"
'Fred
the
Bear' lineCount
"! !

!String methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:18'!
                 lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| start end endWithoutDelimiters |
	start _ 1.
	[ start <= self size ] whileTrue: [
		end _ self indexOfAnyOf: CSLineEnders startingAt: start ifAbsent: [ 0 ].
		end = 0
			ifTrue: [ endWithoutDelimiters _ end _ self size ]
			ifFalse: [ 
				endWithoutDelimiters _ end - 1.
				(end < self size
					and: [(self at: end + 1) = Character lfCharacter
					and: [(self at: end)	= Character crCharacter ]])
						ifTrue: [ end _ end + 1]].
		aBlock value: start value: endWithoutDelimiters value: end.
		start _ end + 1 ]! !

!String methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:05'!
          lineNumber: anIndex
	"Answer a string containing the characters in the given line number. "
	| i |
	i _ 1.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		i = anIndex ifTrue: [
			^ self copyFrom: start to: endWithoutDelimiters ].
		i _ i + 1 ].
	^nil
	
"
'Fred
the
Bear' lineNumber: 3
".! !

!String methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:08'!
                lines
	"Answer an array of lines composing this receiver without the line ending delimiters."

	^Array
		streamContents: [ :lines | self linesDo: [ :aLine | lines nextPut: aLine ]]
		estimatedSize: (self size // 60 max: 16)! !

!String methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:08'!
                              linesDo: aBlock
	"execute aBlock with each line in this string.  The terminating CR's are not included in what is passed to aBlock"
	
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		aBlock value: (self copyFrom: start  to: endWithoutDelimiters) ]
		! !

!String methodsFor: 'converting' stamp: 'jmv 3/13/2012 16:28'!
                      findSelector	"Dan's code for hunting down selectors with keyword parts; while this doesn't give a true parse, in most cases it does what we want, in where it doesn't, we're none the worse for it."	| sel possibleParens level n |	sel _ self withBlanksTrimmed.	(sel includes: $:) ifTrue: [		sel _ sel copyReplaceAll: ':' with: ': '.	"for the style (aa max:bb) with no space"		possibleParens _ sel findTokens: Character separators.		sel _ String streamContents:			[:s | level _ 0.			possibleParens do:				[:token |				(level = 0 and: [token endsWith: ':'])					ifTrue: [s nextPutAll: token]					ifFalse: [(n _ token occurrencesOf: $( ) > 0 ifTrue: [level _ level + n].							(n _ token occurrencesOf: $[ ) > 0 ifTrue: [level _ level + n].							(n _ token occurrencesOf: $] ) > 0 ifTrue: [level _ level - n].							(n _ token occurrencesOf: $) ) > 0 ifTrue: [level _ level - n]]]]].	sel isEmpty ifTrue: [^ nil].	Symbol hasInterned: sel ifTrue: [ :aSymbol |		^ aSymbol].	^ nil! !

!String methodsFor: 'converting' stamp: 'jmv 3/13/2012 16:10'!
                            withBlanksCondensed	"Return a copy of the receiver with leading/trailing blanks removed	 and consecutive white spaces condensed as a single space."	| trimmed lastWasBlank |	trimmed _ self withBlanksTrimmed.	^String streamContents: [ :stream |		lastWasBlank _ false.		trimmed do: [ :c |			c isSeparator				ifTrue: [ lastWasBlank ifFalse: [ stream space ]]				ifFalse: [ stream nextPut: c ].			lastWasBlank _ c isSeparator ]].	" 	' abc  d   ' withBlanksCondensed	' abc  d	s	as   zz 	q 			q' withBlanksCondensed	"! !

!String methodsFor: 'converting' stamp: 'jmv 3/13/2012 11:35'!
           withLineEndings: lineEndingString	"assume the string is textual, and that CR, LF, and CRLF are all 	valid line endings.  Replace each occurence with lineEndingString"	| cr lf crlf inPos outPos outString lineEndPos newOutPos lineEndingSize |	cr _ Character crCharacter.	lf _ Character lfCharacter.	crlf _ CharacterSet new.	crlf add: cr; add: lf.	inPos _ 1.	outPos _ 1.	lineEndingSize _ lineEndingString size.	"This could be extremely inefficient if lineEndingString size > 1"	outString _ String new: self size * lineEndingSize.	[		lineEndPos _ self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [0].		lineEndPos ~= 0 ] whileTrue: [			newOutPos _ outPos + (lineEndPos - inPos).			outString replaceFrom: outPos to: newOutPos - 1 with: self startingAt: inPos.			1 to: lineEndingSize do: [ :i |				outString at: newOutPos put: (lineEndingString at: i).				newOutPos _ newOutPos + 1 ].			outPos _ newOutPos.			((self at: lineEndPos) = cr and: [ lineEndPos < self size and: [ (self at: lineEndPos+1) = lf ] ]) ifTrue: [				"CRLF ending"				inPos _ lineEndPos + 2 ]			ifFalse: [ 				"CR or LF ending"				inPos _ lineEndPos + 1 ]. ].	"no more line endings.  copy the rest"	newOutPos _ outPos + (self size - inPos).	outString replaceFrom: outPos to: newOutPos with: self startingAt: inPos.	^outString copyFrom: 1 to: newOutPos! !

!String methodsFor: 'converting' stamp: 'jmv 3/13/2012 12:48'!
                    withNoLineLongerThan: aNumber	"Answer a string with the same content as receiver, but rewrapped so that no line has more characters than the given number"	aNumber isNumber not | (aNumber < 1) ifTrue: [self error: 'too narrow'].	^self class		streamContents: [ :stream |			self lineIndicesDo: [ :start :endWithoutDelimiters :end |				| pastEnd lineStart |				pastEnd := endWithoutDelimiters + 1.				"eliminate spaces at beginning of line"				lineStart := (self indexOfAnyOf: CSNonSeparators startingAt: start ifAbsent: [pastEnd]) min: pastEnd.				[| lineStop lineEnd spacePosition |				lineEnd := lineStop  := lineStart + aNumber min: pastEnd..				spacePosition := lineStart.				[spacePosition < lineStop] whileTrue: [					spacePosition := self indexOfAnyOf: CSSeparators startingAt: spacePosition + 1 ifAbsent: [pastEnd].					spacePosition <= lineStop ifTrue: [lineEnd := spacePosition].				].				"split before space or before lineStop if no space"				stream nextPutAll: (self copyFrom: lineStart to: lineEnd - 1).				"eliminate spaces at beginning of next line"				lineStart := self indexOfAnyOf: CSNonSeparators startingAt: lineEnd ifAbsent: [pastEnd].				lineStart <= endWithoutDelimiters ]					whileTrue: [stream newLine].				stream nextPutAll: (self copyFrom: pastEnd to: end) ] ]		estimatedSize: self size * (aNumber + 1) // aNumber "provision for supplementary line breaks"! !

!String methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:48'!
   print	Transcript show: self; newLine! !

!String methodsFor: 'paragraph support' stamp: 'jmv 3/14/2012 08:22'!
                encompassLine: anInterval	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.	Answer starts at the position following a newLine (or eventually 1) and ends before a newLine (or eventually at self size)	See also encompassParagraph:"	| left rightCr rightNull |	left _ (self lastIndexOf: Character newLineCharacter startingAt: anInterval first - 1 ifAbsent:[0]) + 1.	rightCr _ (self indexOf: Character newLineCharacter startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.	rightNull _ (self indexOf: Character null startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.	^left to: (rightCr min: rightNull)! !

!String methodsFor: 'paragraph support' stamp: 'jmv 3/14/2012 08:22'!
    encompassParagraph: anInterval	"Return an interval that includes anInterval, and that comprises one or several whole paragraphs in the receiver.	Answer starts at the position following a newLine (or eventually 1) and ends at a newLine (or eventually at self size).	Look also for null characters. Never include null characters in the answer.	See also #encompassLine:"	| left rightCr rightNull |	left _ (self lastIndexOf: Character newLineCharacter startingAt: anInterval first - 1 ifAbsent:[0]) + 1.	rightCr _ (self indexOf: Character newLineCharacter startingAt: (anInterval last max: anInterval first) ifAbsent: [self size]).	rightNull _ (self indexOf: Character null startingAt: (anInterval last max: anInterval first) ifAbsent: [self size+1])-1.	^left to: (rightCr min: rightNull)! !

!String methodsFor: 'paragraph support' stamp: 'jmv 3/14/2012 08:34'!
                             endOfParagraphBefore: aNumber	"Return the index of the last Character newLineCharacter before position aNumber, or zero if this is the first paragraph.	'ddd' endOfParagraphBefore: 3	'dd	d' endOfParagraphBefore: 4	"	^ self lastIndexOf: Character newLineCharacter startingAt: aNumber - 1 ifAbsent: [ 0 ]! !

!String methodsFor: 'paragraph support' stamp: 'jmv 3/14/2012 08:23'!
  indentationIfBlank: aBlock
	"Answer the number of leading tabs in the receiver.  If there are	 no visible characters, pass the number of tabs to aBlock and return its value."
	| reader leadingTabs lastSeparator tab ch |
	tab _ Character tab.
	reader _ ReadStream on: self.
	leadingTabs _ 0.
	[
		reader atEnd not and: [ (ch _ reader next) == tab ]] whileTrue: [
			leadingTabs _ leadingTabs + 1 ].
	lastSeparator _ leadingTabs + 1.
	[
		reader atEnd not and: [
			ch isSeparator and: [ ch isLineSeparator not ]]] whileTrue: [
		lastSeparator _ lastSeparator + 1.
		ch _ reader next ].
	lastSeparator = self size | ch isLineSeparator ifTrue: [
		^ aBlock value: leadingTabs ].
	^ leadingTabs! !


!String class methodsFor: 'initialization' stamp: 'jmv 3/13/2012 11:23'!
                              initialize	"	String initialize	"	| order newOrder lowercase |	"Case insensitive compare sorts null, space, digits, letters, all the rest..."	newOrder _ Array new: 256.	order _ -1.	newOrder at: 0+1 put:  (order _ order+1).	32 to: 63 do: [ :c |		newOrder at: c + 1 put: (order _ order+1)].	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |		order _ order+1.		newOrder at: upperAndLowercase first asciiValue + 1 put: order.		upperAndLowercase size > 1 ifTrue: [			newOrder at: upperAndLowercase second asciiValue + 1 put: order ]].	1 to: newOrder size do: [ :i |		(newOrder at: i) ifNil: [			newOrder at: i put: (order _ order+1)]].	CaseInsensitiveOrder _ newOrder asByteArray.		"Case sensitive compare sorts null, space, digits, letters, all the rest..."	newOrder _ Array new: 256.	order _ -1.	newOrder at: 0+1 put:  (order _ order+1).	32 to: 63 do: [ :c |		newOrder at: c + 1 put: (order _ order+1)].	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |		upperAndLowercase size > 1 ifTrue: [			newOrder at: upperAndLowercase first asciiValue + 1 put: (order _ order+1) ]].	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |		lowercase _ upperAndLowercase size = 1			ifTrue: [ upperAndLowercase first ]			ifFalse: [ upperAndLowercase second ].		newOrder at: lowercase asciiValue + 1 put: (order _ order+1) ].	1 to: newOrder size do: [ :i |		(newOrder at: i) ifNil: [			newOrder at: i put: (order _ order+1)]].	order = 255 ifFalse: [self error: 'order problem'].	CaseSensitiveOrder _ newOrder asByteArray.	"a table for translating to lower case"	LowercasingTable _ String withAll: (Character allCharacters collect: [:c | c asLowercase]).	"a table for translating to upper case"	UppercasingTable _ String withAll: (Character allCharacters collect: [:c | c asUppercase]).	"a table for testing tokenish (for fast numArgs)"	Tokenish _ String withAll: (Character allCharacters collect:									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).	"CR and LF--characters that terminate a line"	CSLineEnders _ CharacterSet new.	CSLineEnders add: Character crCharacter.	CSLineEnders add: Character lfCharacter. 	"separators and non-separators"	CSSeparators _ CharacterSet separators.	CSNonSeparators _ CSSeparators complement! !


!Symbol methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:48'!
                        print	Transcript show: self printString; newLine! !


!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/13/2012 12:50'!
                               removeClassNamed: aName	"Invoked from fileouts:  if there is currently a class in the system named aName, then remove it.  If anything untoward happens, report it in the Transcript.  "	| oldClass |	(oldClass _ self at: aName asSymbol ifAbsent: nil)		ifNil: [			Transcript newLine; show: 'Removal of class named ', aName, ' ignored because ', aName, ' does not exist.'.			^ self].	oldClass removeFromSystem! !

!SystemDictionary methodsFor: 'class names' stamp: 'jmv 3/13/2012 12:50'!
                   renameClassNamed: oldName as: newName	"Invoked from fileouts:  if there is currently a class in the system named oldName, then rename it to newName.  If anything untoward happens, report it in the Transcript.  "	| oldClass |	(oldClass _ self at: oldName asSymbol ifAbsent: nil)		ifNil: [			Transcript newLine; show: 'Class-rename for ', oldName, ' ignored because ', oldName, ' does not exist.'.			^ self].	oldClass rename: newName! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/13/2012 12:50'!
                        recompileAllFrom: firstName 	"Recompile all classes, starting with given name."	self allClassesDo: [ :class |		class name >= firstName			ifTrue: [				Transcript show: class name; newLine.				class compileAll]]	"Smalltalk recompileAllFrom: 'AAABodyShop'."! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/13/2012 12:50'!
        removeTextCode    "Smalltalk removeTextCode"	"Scan all methods for source code that is Text (i.e. with attributes)	Replace it with simpleStrings"	 | oldCodeString n newCodeString oldStamp oldCategory m classComment c o stamp |	'Scanning sources for Text.This will take a few moments...'		displayProgressAt: Sensor mousePoint		from: 0 		to: CompiledMethod instanceCount		during: [ :bar | 			n _ 0.			m _ 0.			c _ 0.			Smalltalk allBehaviorsDo: [ :cls | 				cls selectors do: [ :selector  | 					(n _ n+1) \\ 100 = 0 ifTrue: [bar value: n].					oldCodeString _ cls sourceCodeAt: selector.					oldCodeString class = String ifFalse: [						newCodeString _ oldCodeString asString.						oldStamp _ Utilities timeStampForMethod: (cls compiledMethodAt: selector).						oldCategory _ cls whichCategoryIncludesSelector: selector.						cls compile: newCodeString classified: oldCategory withStamp: oldStamp notifying: nil.						m _ m + 1]].				cls isMeta ifFalse: [					o _ cls organization.					classComment _ o classComment.					stamp _ o commentStamp.					classComment class == String ifFalse: [						classComment hasAnyAttribute ifTrue: [							self halt "review" ].						cls classComment: classComment asString stamp: stamp.						c _ c + 1 ]].			].		].		Transcript newLine; show: m printString , ' text methods converted to strings.'.		Transcript newLine; show: c printString , ' text class comments converted to strings.'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/13/2012 12:51'!
    testDecompiler	"Smalltalk testDecompiler"	"Decompiles the source for every method in the system, and then compiles that source and verifies that it generates (and decompiles to) identical code.  This currently fails in a number of places because some different patterns (esp involving conditionals where the first branch returns) decompile the same."	| methodNode oldMethod newMethod badOnes oldCodeString n |	badOnes _ OrderedCollection new.	'Decompiling all classes...'		displayProgressAt: Sensor mousePoint		from: 0		to: CompiledMethod instanceCount		during: [ :bar |			n _ 0.			Smalltalk allBehaviorsDo: [ :cls |				"Transcript cr; show: cls name."				cls selectors do: [ :selector |					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].					oldMethod _ cls compiledMethodAt: selector.					oldCodeString _ (cls decompilerClass new						decompile: selector						in: cls						method: oldMethod) decompileString.					methodNode _ cls compilerClass new						compile: oldCodeString						in: cls						notifying: nil						ifFail: nil.					newMethod _ methodNode generate: #(0 0 0 0 ).					oldCodeString =						(cls decompilerClass new							decompile: selector							in: cls							method: newMethod) decompileString ifFalse: [						Transcript							 newLine;							 show: '***' , cls name , ' ' , selector.						badOnes add: cls name , ' ' , selector ]]]].	Smalltalk		browseMessageList: badOnes asArray sort		name: 'Decompiler Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/13/2012 12:51'!
             testFormatter	"Smalltalk testFormatter"	"Reformats the source for every method in the system, and then compiles that source and verifies that it generates identical code.	The formatting used will be classic monochrome."	| newCodeString methodNode oldMethod newMethod badOnes n |	badOnes _ OrderedCollection new.	'Formatting all classes...'		displayProgressAt: Sensor mousePoint		from: 0		to: CompiledMethod instanceCount		during: [ :bar |			n _ 0.			Smalltalk allBehaviorsDo: [ :cls |				"Transcript cr; show: cls name."				cls selectors do: [ :selector |					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].					newCodeString _ cls compilerClass new						format: (cls sourceCodeAt: selector)						in: cls						notifying: nil.					methodNode _ cls compilerClass new						compile: newCodeString						in: cls						notifying: nil						ifFail: nil.					newMethod _ methodNode generate: #(0 0 0 0 ).					oldMethod _ cls compiledMethodAt: selector.					oldMethod = newMethod ifFalse: [						Transcript							 newLine;							 show: '***' , cls name , ' ' , selector.						badOnes add: cls name , ' ' , selector ]]]].	Smalltalk		browseMessageList: badOnes asArray sort		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 3/13/2012 12:51'!
    testFormatter2	"Smalltalk testFormatter2"	"Reformats the source for every method in the system, and then verifies that the order of source tokens is unchanged.	The formatting used will be classic monochrome"	| newCodeString badOnes n oldCodeString oldTokens newTokens |	badOnes _ OrderedCollection new.	'Formatting all classes...'		displayProgressAt: Sensor mousePoint		from: 0		to: CompiledMethod instanceCount		during: [ :bar |			n _ 0.			Smalltalk allBehaviorsDo: [ :cls |				"Transcript cr; show: cls name."				cls selectors do: [ :selector |					(n _ n + 1) \\ 100 = 0 ifTrue: [ bar value: n ].					oldCodeString _ (cls sourceCodeAt: selector) asString.					newCodeString _ cls compilerClass new						format: oldCodeString						in: cls						notifying: nil.					oldTokens _ oldCodeString findTokens: Character separators.					newTokens _ newCodeString findTokens: Character separators.					oldTokens = newTokens ifFalse: [						Transcript							 newLine;							 show: '***' , cls name , ' ' , selector.						badOnes add: cls name , ' ' , selector ]]]].	Smalltalk		browseMessageList: badOnes asArray sort		name: 'Formatter Discrepancies'.! !

!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 3/13/2012 12:49'!
                             bytesLeftString	"Return a string describing the amount of memory available"	| availInternal availPhysical availTotal |	self garbageCollect.	availInternal _ self primBytesLeft.	availPhysical _ self bytesLeft: false.	availTotal _ self bytesLeft: true.	(availTotal > (availInternal + 10000)) "compensate for mini allocations inbetween"		ifFalse:[^availInternal asStringWithCommas, ' bytes available'].	^String streamContents:[:s|		s nextPutAll: availInternal asStringWithCommas, 	' bytes (internal) '; newLine.		s nextPutAll: availPhysical asStringWithCommas,	' bytes (physical) '; newLine.		s nextPutAll: availTotal asStringWithCommas, 	' bytes (total)     '].! !

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 3/13/2012 12:50'!
                 logError: errMsg inContext: aContext to: aFilename	"Log the error message and a stack trace to the given file."	| ff |	FileDirectory default deleteFileNamed: aFilename ifAbsent: nil.	(ff _ FileStream fileNamed: aFilename) ifNil: [^ self "avoid recursive errors"].  	ff nextPutAll: errMsg; newLine.	aContext errorReportOn: ff.	ff close.! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 3/13/2012 17:02'!
                           unusedClassesAndMethodsWithout: classesAndMessagesPair	| classRemovals messageRemovals nClasses nMessages |	"Accepts and returns a pair: {set of class names. set of selectors}.	It is expected these results will be diff'd with the normally unused results."	(classRemovals _ IdentitySet new) addAll: classesAndMessagesPair first.	(messageRemovals _ IdentitySet new) addAll: classesAndMessagesPair second.	nClasses _ nMessages _ -1.	["As long as we keep making progress..."	classRemovals size > nClasses or: [messageRemovals size > nMessages]]		whileTrue:			["...keep trying for bigger sets of unused classes and selectors."			nClasses _ classRemovals size.			nMessages _ messageRemovals size.			Utilities informUser: 'Iterating removals ' ,					(classesAndMessagesPair first isEmpty						ifTrue: ['for baseline...']						ifFalse: ['for ', classesAndMessagesPair first first, ' etc...']) , String newLineString ,					nClasses printString , ' classes, ' , nMessages printString , ' messages.||' "spacers move menu off cursor"				during:				[classRemovals addAll: (self allUnusedClassesWithout: {classRemovals. messageRemovals}).				messageRemovals addAll: (self allUnSentMessagesWithout: {classRemovals. messageRemovals})]].	^ {classRemovals. self allUnSentMessagesWithout: {classRemovals. messageRemovals}}! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/13/2012 12:48'!
                          assureStartupStampLogged	"If there is a startup stamp not yet actually logged to disk, do it now."	| changesFile |	StartupStamp ifNil: [^ self].	(SourceFiles isNil or: [(changesFile _ SourceFiles at: 2) == nil]) ifTrue: [^ self].	changesFile isReadOnly ifTrue:[^self].	changesFile setToEnd; newLine; newLine.	changesFile nextChunkPut: StartupStamp asString; newLine.	StartupStamp _ nil.	self forceChangesToDisk! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/13/2012 12:50'!
      logChange: aStringOrText 	"Write the argument, aString, onto the changes file."	| aString changesFile |	(SourceFiles isNil or: [(SourceFiles at: 2) == nil]) ifTrue: [^ self].	self assureStartupStampLogged.	aString _ aStringOrText asString.	(aString findFirst: [:char | char isSeparator not]) = 0		ifTrue: [^ self].  "null doits confuse replay"	(changesFile _ SourceFiles at: 2).	changesFile isReadOnly ifTrue:[^self].	changesFile setToEnd; newLine; newLine.	changesFile nextChunkPut: aString.		"If want style changes in DoIt, use nextChunkPutWithStyle:, and allow Texts to get here"	self forceChangesToDisk.! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/13/2012 12:50'!
                            logChange: aStringOrText preamble: preamble	"Write the argument, aString, onto the changes file."	| aString changesFile |	(SourceFiles isNil or: [(SourceFiles at: 2) == nil]) ifTrue: [^ self].	self assureStartupStampLogged.	aString _ aStringOrText asString.	(aString findFirst: [:char | char isSeparator not]) = 0		ifTrue: [^ self].  "null doits confuse replay"	(changesFile _ SourceFiles at: 2).	changesFile isReadOnly ifTrue:[^self].	changesFile setToEnd; newLine; newLine.	changesFile nextPut: $!!; nextChunkPut: preamble; newLine.	changesFile nextChunkPut: aString.		"If want style changes in DoIt, use nextChunkPutWithStyle:, and allow Texts to get here"	self forceChangesToDisk.! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 3/13/2012 17:03'!
              systemInformationString	"Identify software version"	^ SystemVersion current version, String newLineString, self lastUpdateString, String newLineString, self currentChangeSetString! !

!SystemDictionary methodsFor: 'toDeprecate' stamp: 'jmv 3/13/2012 12:51'!
                            snapshot: save andQuit: quit embedded: embeddedFlag 	"Mark the changes file and close all files as part of #processShutdownList.	If save is true, save the current state of this Smalltalk in the image file.	If quit is true, then exit to the outer OS shell.	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."	| resuming msg |	ActiveModel flushEventSystem.	(SourceFiles at: 2) ifNotNil: 			[msg := String streamContents: 							[:s | 							s								nextPutAll: '----';								nextPutAll: (save 											ifTrue: [quit ifTrue: ['QUIT'] ifFalse: ['SNAPSHOT']]											ifFalse: [quit ifTrue: ['QUIT/NOSAVE'] ifFalse: ['NOP']]);								nextPutAll: '----';								print: Date dateAndTimeNow;								space;								nextPutAll: (FileDirectory default localNameFor: self imageName);								nextPutAll: ' priorSource: ';								print: LastQuitLogPosition].			self assureStartupStampLogged.			save 				ifTrue: 					[LastQuitLogPosition _ ((SourceFiles at: 2)								setToEnd;								position)].			self logChange: msg.			Transcript				newLine;				show: msg;				newLine].	self processShutDownList: quit.	Cursor write show.	save 		ifTrue: 			[resuming := embeddedFlag 						ifTrue: [self snapshotEmbeddedPrimitive]						ifFalse: [self snapshotPrimitive].	"<-- PC frozen here on image file"			]		ifFalse: [resuming := false].	quit & (resuming == false) ifTrue: [self quitPrimitive].	Cursor normal show.	self setGCParameters.	resuming == true ifTrue: [self clearExternalObjects].	self processStartUpList: resuming == true.	resuming == true 		ifTrue: [			self setPlatformPreferences.			self readDocumentFile].	"Now it's time to raise an error"	resuming		ifNil: [ self error: 'Failed to write image file (disk full?)'].	^resuming! !

!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 3/13/2012 12:51'!
          showMenuOf: selectorCollection withFirstItem: firstItem ifChosenDo: choiceBlock withCaption: aCaption	"Show a sorted menu of the given selectors, preceded by firstItem, and all abbreviated to 40 characters.  Use aCaption as the menu title, if it is not nil.  Evaluate choiceBlock if a message is chosen."	| index menuLabels sortedList aMenu |	sortedList _ selectorCollection asArray sort.	menuLabels _ String streamContents: [ :strm |		strm nextPutAll: (firstItem contractTo: 40).		sortedList do: [ :sel |			strm				 newLine;				 nextPutAll: (sel contractTo: 40) ]].	aMenu _ PopUpMenu		labels: menuLabels		lines: #(1 ).	index _ aCaption		ifNil: [ aMenu startUp ]		ifNotNil: [ aMenu startUpWithCaption: aCaption ].	index = 1 ifTrue: [ choiceBlock value: firstItem ].	index > 1 ifTrue: [ choiceBlock value: (sortedList at: index - 1) ].! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:52'!
                               fileOut  "SystemOrganization fileOut"	(FileStream newFileNamed:		(FileDirectory default nextNameFor: 'SystemOrganization' extension: 'st'))			nextPutAll: 'SystemOrganization changeFromCategorySpecs: #(';			newLine;			print: SystemOrganization;  "ends with a cr"			nextPutAll: ')!!';			newLine;			close.! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:54'!
                            fileOutCategory: category on: aFileStream initializing: aBool	"Store on the file associated with aFileStream, all the classes associated 	with the category and any requested shared pools."	| first poolSet tempClass classes |	classes _ (self superclassOrder: category).	poolSet _ Set new.	classes do: 		[:class | class sharedPools do: [:eachPool | poolSet add: eachPool]].	poolSet size > 0 ifTrue:		[tempClass _ Class new.		tempClass shouldFileOutPools ifTrue:			[poolSet _ poolSet select: [:aPool | tempClass shouldFileOutPool: (Smalltalk keyAtIdentityValue: aPool)].			poolSet do: [:aPool | tempClass fileOutPool: aPool onFileStream: aFileStream]]].	first _ true.	classes do: 		[:class | 		first			ifTrue: [first _ false]			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].		class			fileOutOn: aFileStream			moveSource: false			toFile: 0			initializing: false].	aBool ifTrue:[classes do:[:cls| cls fileOutInitializerOn: aFileStream]].! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:54'!
fileOutCategoryNoPoolsNoInit: category on: aFileStream	"Store on the file associated with aFileStream, all the classes associated 	with the category and any requested shared pools."	| first classes |	classes _ self superclassOrder: category.	first _ true.	classes do: [ :class | 		first			ifTrue: [first _ false]			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].		class			fileOutOn: aFileStream			moveSource: false			toFile: 0			initializing: false]! !


!SystemWindow methodsFor: 'panes' stamp: 'jmv 3/13/2012 12:55'!
                     setUpdatablePanesFrom: getSelectors	| aList aPane possibles |	"Set my updatablePanes inst var to the list of panes which are list panes with the given get-list selectors.  Order is important here!!  Note that the method is robust in the face of panes not found, but a warning is printed in the transcript in each such case"	aList _ OrderedCollection new.	possibles _ OrderedCollection new.	self allMorphsDo: [ :pane | 		(pane isKindOf: PluggableListMorph) ifTrue: [			possibles add: pane.		].	].	getSelectors do: [:sel | 		aPane _ possibles detect: [ :pane | pane getListSelector == sel] ifNone: nil.		aPane			ifNotNil:				[aList add: aPane]			ifNil:				[Transcript newLine; show: 'Warning: pane ', sel, ' not found.']].	updatablePanes _ aList asArray! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 3/13/2012 12:27'!
                  showUnreferencedClassVars	"Search for all class variables known to the selected class, and put up a 	list of those that have no references anywhere in the system. The 	search includes superclasses, so that you don't need to navigate your 	way to the class that defines each class variable in order to determine 	whether it is unreferenced"	| cls aList |	(cls _ model selectedClass)		ifNil: [^ self].	aList _ cls allUnreferencedClassVariables.	aList size = 0		ifTrue: [^ self inform: 'There are no unreferencedclass variables in' , cls name].	Transcript newLine; nextPutAll: 'Unreferenced class variable(s) in ', cls name; newLine.	aList do: [:el | Transcript tab; nextPutAll: el; newLine].	(SelectionMenu labels: aList selections: aList)		startUpWithCaption: 'Unreferencedclass variables in ' , cls name! !

!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 3/13/2012 12:27'!
                               showUnreferencedInstVars	"Search for all instance variables known to the selected class, and put up a list of those that have no references anywhere in the system.  The search includes superclasses, so that you don't need to navigate your way to the class that defines each inst variable in order to determine whether it is unreferenced"	| cls aList |	(cls _ model selectedClassOrMetaClass) ifNil: [^ self].	aList _ cls allUnreferencedInstanceVariables.	aList size = 0 ifTrue: [^ self inform: 'There are no unreferencedinstance variables in', cls name].	Transcript newLine; nextPutAll: 'Unreferenced instance variable(s) in ', cls name; newLine.	aList do: [ :el | Transcript tab; nextPutAll: el; newLine ].	(SelectionMenu labels: aList selections: aList) startUpWithCaption: 'Unreferencedinstance variables in ', cls name! !


!TestCase methodsFor: 'Running' stamp: 'jmv 3/13/2012 12:55'!
                          logFailure: aString	self isLogging ifTrue: [		self failureLog 			newLine; 			nextPutAll: aString ]! !


!ClosureCompilerTest methodsFor: 'source' stamp: 'jmv 3/13/2012 11:29'!
            closureCases	^#('| n |n := 1.^n + n''| i |i := 0.[i := i + 1. i <= 10] whileTrue.^i''[:c :s| | mn |mn := Compiler new		compile: (c sourceCodeAt: s)		in: c		notifying: nil		ifFail: [self halt].mn generate: #(0 0 0 0).{mn blockExtentsToTempsMap.  mn encoder schematicTempNames}]			value: ArrayLiteralTest			value: #testSymbols''inject: thisValue into: binaryBlock	| nextValue |	nextValue := thisValue.	self do: [:each | nextValue := binaryBlock value: nextValue value: each].	^nextValue''runBinaryConditionalJumps: assertPrintBar	"CogIA32CompilerTests new runBinaryConditionalJumps: false"	| mask reg1 reg2 reg3 |	mask := 1 << self processor bitsInWord - 1.	self concreteCompilerClass dataRegistersWithAccessorsDo:		[:n :get :set|		n = 0 ifTrue: [reg1 := get].		n = 1 ifTrue: [reg2 := set].		n = 2 ifTrue: [reg3 := set]].	#(	(JumpAbove > unsigned)			(JumpBelowOrEqual <= unsigned)		(JumpBelow < unsigned)			(JumpAboveOrEqual >= unsigned)		(JumpGreater > signed)			(JumpLessOrEqual <= signed)		(JumpLess < signed)				(JumpGreaterOrEqual >= signed)		(JumpZero = signed)				(JumpNonZero ~= signed)) do:		[:triple|		[:opName :relation :signednessOrResult| | opcode jumpNotTaken jumpTaken nop memory bogus |		self resetGen.		opcode := CogRTLOpcodes classPool at: opName.		self gen: CmpRR operand: 2 operand: 1.		jumpTaken := self gen: opcode.		self gen: MoveCqR operand: 0 operand: 0.		jumpNotTaken := self gen: Jump.		jumpTaken jmpTarget: (self gen: MoveCqR operand: 1 operand: 0).		jumpNotTaken jmpTarget: (nop := self gen: Nop).		memory := self generateInstructions.		bogus := false.		self pairs: (-2 to: 2)  do:			[:a :b| | taken |			self processor				reset;				perform: reg2 with: a signedIntToLong;				perform: reg3 with: b signedIntToLong.			[self processor singleStepIn: memory.			 self processor pc ~= nop address] whileTrue.			taken := (self processor perform: reg1) = 1.			assertPrintBar				ifTrue:					[self assert: taken = (signednessOrResult == #unsigned											ifTrue: [(a bitAnd: mask) perform: relation with: (b bitAnd: mask)]											ifFalse: [a perform: relation with: b])]				ifFalse:					[Transcript						nextPutAll: reg2; nextPut: $(; print: a; nextPutAll: '') ''; nextPutAll: relation; space;						nextPutAll: reg3; nextPut: $(; print: b; nextPutAll: '') = '';						print: taken; cr; flush.					 taken = (signednessOrResult == #unsigned											ifTrue: [(a bitAnd: mask) perform: relation with: (b bitAnd: mask)]											ifFalse: [a perform: relation with: b]) ifFalse:						[bogus := true]]].			 bogus ifTrue:				[self processor printRegistersOn: Transcript.				 Transcript show: (self processor disassembleInstructionAt: jumpTaken address In: memory); cr]]					valueWithArguments: triple]''mapFromBlockStartsIn: aMethod toTempVarsFrom: schematicTempNamesString constructor: aDecompilerConstructor	| map |	map := aMethod				mapFromBlockKeys: aMethod startpcsToBlockExtents keys asSortedCollection				toSchematicTemps: schematicTempNamesString.	map keysAndValuesDo:		[:startpc :tempNameTupleVector| | subMap tempVector numTemps |		subMap := Dictionary new.		"Find how many temp slots there are (direct & indirect temp vectors)		 and for each indirect temp vector find how big it is."		tempNameTupleVector do:			[:tuple|			tuple last isArray				ifTrue:					[subMap at: tuple last first put: tuple last last.					 numTemps := tuple last first]				ifFalse:					[numTemps := tuple last]].		"create the temp vector for this scope level."		tempVector := Array new: numTemps.		"fill it in with any indirect temp vectors"		subMap keysAndValuesDo:			[:index :size|			tempVector at: index put: (Array new: size)].		"fill it in with temp nodes."		tempNameTupleVector do:			[:tuple| | itv |			tuple last isArray				ifTrue:					[itv := tempVector at: tuple last first.					 itv at: tuple last last						put: (aDecompilerConstructor								codeTemp: tuple last last - 1								named: tuple first)]				ifFalse:					[tempVector						at: tuple last						put: (aDecompilerConstructor								codeTemp: tuple last - 1								named: tuple first)]].		"replace any indirect temp vectors with proper RemoteTempVectorNodes"		subMap keysAndValuesDo:			[:index :size|			tempVector				at: index				put: (aDecompilerConstructor						codeRemoteTemp: index						remoteTemps: (tempVector at: index))].		"and update the entry in the map"		map at: startpc put: tempVector].	^map' 'gnuifyFrom: inFileStream to: outFileStream"convert interp.c to use GNU features"	| inData beforeInterpret inInterpret inInterpretVars beforePrimitiveResponse inPrimitiveResponse |	inData := inFileStream upToEnd withSqueakLineEndings.	inFileStream close.	"print a header"	outFileStream		nextPutAll: ''/* This file has been post-processed for GNU C */'';		cr; cr; cr.	beforeInterpret := true.    "whether we are before the beginning of interpret()"	inInterpret := false.     "whether we are in the middle of interpret"	inInterpretVars := false.    "whether we are in the variables of interpret"	beforePrimitiveResponse := true.  "whether we are before the beginning of primitiveResponse()"	inPrimitiveResponse := false.   "whether we are inside of primitiveResponse"	''Gnuifying''		displayProgressAt: Sensor mousePoint		from: 1 to: (inData occurrencesOf: Character crCharacter)		during:			[:bar | | lineNumber |			lineNumber := 0.			inData linesDo:				[ :inLine | | outLine extraOutLine caseLabel |				bar value: (lineNumber := lineNumber + 1).				outLine := inLine. 	"print out one line for each input line; by default, print out the line that was input, but some rules modify it"				extraOutLine := nil.   "occasionally print a second output line..."				beforeInterpret ifTrue: [					inLine = ''#include "sq.h"'' ifTrue: [						outLine := ''#include "sqGnu.h"'' ].					inLine = ''interpret(void) {'' ifTrue: [						"reached the beginning of interpret"						beforeInterpret := false.						inInterpret := true.						inInterpretVars := true ] ]				ifFalse: [				inInterpretVars ifTrue: [					(inLine findString: ''register struct foo * foo = &fum;'') > 0 ifTrue: [						outLine := ''register struct foo * foo FOO_REG = &fum;'' ].					(inLine findString: '' localIP;'') > 0 ifTrue: [						outLine := ''    char* localIP IP_REG;'' ].					(inLine findString: '' localFP;'') > 0 ifTrue: [						outLine := ''    char* localFP FP_REG;'' ].					(inLine findString: '' localSP;'') > 0 ifTrue: [						outLine := ''    char* localSP SP_REG;'' ].					(inLine findString: '' currentBytecode;'') > 0 ifTrue: [						outLine := ''    sqInt currentBytecode CB_REG;'' ].					inLine isEmpty ifTrue: [						"reached end of variables"						inInterpretVars := false.						outLine := ''    JUMP_TABLE;''.						extraOutLine := inLine ] ]				ifFalse: [				inInterpret ifTrue: [					"working inside interpret(); translate the switch statement"					(inLine beginsWith: ''		case '') ifTrue: [						caseLabel := (inLine findTokens: ''	 :'') second.						outLine := ''		CASE('', caseLabel, '')'' ].					inLine = ''			break;'' ifTrue: [						outLine := ''			BREAK;'' ].					inLine = ''}'' ifTrue: [						"all finished with interpret()"						inInterpret := false ] ]				ifFalse: [				beforePrimitiveResponse ifTrue: [					(inLine beginsWith: ''primitiveResponse('') ifTrue: [						"into primitiveResponse we go"						beforePrimitiveResponse := false.						inPrimitiveResponse := true.						extraOutLine := ''    PRIM_TABLE;'' ] ]				ifFalse: [				inPrimitiveResponse ifTrue: [					inLine = ''	switch (primitiveIndex) {'' ifTrue: [						extraOutLine := outLine.						outLine := ''	PRIM_DISPATCH;'' ].					inLine = ''	switch (GIV(primitiveIndex)) {'' ifTrue: [						extraOutLine := outLine.						outLine := ''	PRIM_DISPATCH;'' ].					(inLine beginsWith: ''	case '') ifTrue: [						caseLabel := (inLine findTokens: ''	 :'') second.						outLine := ''	CASE('', caseLabel, '')'' ].					inLine = ''}'' ifTrue: [						inPrimitiveResponse := false ] ]				] ] ] ].				outFileStream nextPutAll: outLine; cr.				extraOutLine ifNotNil: [					outFileStream nextPutAll: extraOutLine; cr ]]].	outFileStream close' )! !


!FloatTest methodsFor: 'NaN behavior' stamp: 'jmv 3/13/2012 12:34'!
            testNaNCompare	"IEEE 754 states that NaN cannot be ordered.	As a consequence, every arithmetic comparison involving a NaN SHOULD return false.	Except the is different test (~=).	This test does verify this rule"		| compareSelectors theNaN anotherNaN comparand brokenMethods warningMessage |	compareSelectors := #(#< #<= #> #>= #=).	theNaN := Float nan.	anotherNaN := Float infinity - Float infinity.	comparand := {1. 2.3. Float infinity. 2/3. 1.25. 2 raisedTo: 50}.	comparand := comparand , (comparand collect: [:e | e negated]).	comparand := comparand , {theNaN. anotherNaN}."do a first pass to collect all broken methods"	brokenMethods := Set new.	comparand do: [:comp |		compareSelectors do: [:op |			(theNaN perform: op with: comp) ifTrue: [brokenMethods add: (theNaN class lookupSelector: op)].			(comp perform: op with: theNaN) ifTrue: [brokenMethods add: (comp class lookupSelector: op)]].		(theNaN ~= comp) ifFalse: [brokenMethods add: (theNaN class lookupSelector: #~=)].		(comp ~= theNaN) ifFalse: [brokenMethods add: (comp class lookupSelector: #~=)]].	"build a warning message to tell about all broken methods at once"	warningMessage := String streamContents: [:s |			s nextPutAll: 'According to IEEE 754 comparing with a NaN should always return false, except ~= that should return true.'; newLine.			s nextPutAll: 'All these methods failed to do so. They are either broken or call a broken one'.			brokenMethods do: [:e | s newLine; print: e methodClass; nextPutAll: '>>'; print: e selector]].		"Redo the tests so as to eventually open a debugger on one of the failures"	brokenMethods := Set new.	comparand do: [:comp2 |		compareSelectors do: [:op2 |			self deny: (theNaN perform: op2 with: comp2) description: warningMessage.			self deny: (comp2 perform: op2 with: theNaN) description: warningMessage].		self assert: (theNaN ~= comp2) description: warningMessage.		self assert: (comp2 ~= theNaN) description: warningMessage].! !


!IntegerTest methodsFor: 'tests - basic' stamp: 'jmv 3/14/2012 09:05'!
                            testDegreeCos
	"
	self run: #testDegreeCos
	"
	self
		shouldnt: [ 45 degreeCos ]
		raise: Error.
	self assert: (45 degreeCos roundTo: 1e-14) = ((2 sqrt / 2) asFloat roundTo: 1e-14).! !


!TestRunner methodsFor: 'test processing' stamp: 'jmv 3/13/2012 12:55'!
                             showResult	self errorLog newLine; newLine; show: '==== SUnit ======== Start ===='.	self		showResultSummary;		showResultDefects.	self errorLog newLine; show: '==== SUnit ========== End ===='; newLine.! !

!TestRunner methodsFor: 'test processing' stamp: 'jmv 3/13/2012 15:33'!
      showResultDefects	(self result failureCount > 0)		ifTrue: [			self errorLog newLine; show: '---- SUnit ----- Failures ----'.			self result failures do: [:failure |				self errorLog newLine; tab; show: failure printString]].	(self result errorCount > 0)		ifTrue: [			self errorLog newLine; show: '---- SUnit ------- Errors ----'.			self result errors do: [:error |				self errorLog newLine; tab; show: error printString]].! !

!TestRunner methodsFor: 'test processing' stamp: 'jmv 3/13/2012 15:34'!
   showResultSummary	| message summary |	message := (self result runCount = self result correctCount)		ifTrue: ['succeeded']		ifFalse: ['failed'].	Transcript newLine; tab; show: message.	summary :=		self result runCount printString, ' run, ',		self result failureCount printString, ' failed, ',		self result errorCount printString, ' errors'.	Transcript newLine; tab; show: summary.! !


!Text methodsFor: 'accessing' stamp: 'jmv 3/13/2012 22:46'!
                        at: index put: character

	| answer prevChar |
	prevChar _ string at: index.
	answer _ string at: index put: character.

	"Only fix ParagraphAttributes if there is real danger of breaking the invariant"
	(prevChar isLineSeparator and: [
			(self attributesAt: index) anySatisfy: [ :attr | attr isParagraphAttribute]]) ifTrue: [
		self fixParagraphAttributesFrom: index to: index ].
	
	^answer! !

!Text methodsFor: 'private' stamp: 'jmv 3/13/2012 22:46'!
                         fixParagraphAttributesFrom: start to: end
	"Helper method to ensure the invariant that TextAttributes that answer true to
	 #isParagraphAttribute are only applied to whole paragraphs.
	See senders"
	
	| paragraphEnd paragraphInterval paragraphStart paragraphAttributes|
	
	paragraphEnd _ end max: start.	"end could be start-1 when new text is empty, for example, when backspacing"
	[
		paragraphInterval _ self encompassParagraph: (paragraphEnd to: paragraphEnd).
		paragraphStart _ paragraphInterval first.
		paragraphEnd _ paragraphInterval last.
	
		"We must honor the paragraph attributes as defined in the newline (Lf) Character that ends the paragraph"
		paragraphAttributes _ (self attributesAt: paragraphEnd) select: [ :attr | attr isParagraphAttribute ].

		"if paragraphEnd is inside the interval just modified, and it doesn't bring any paragraph attributes (i.e., it doesn't end in CR),
		then try to keep the paragraph attributes previously in use in this paragraph..
		This is needed, for example, when pasting an image  or a plain text at the end of the document"
		(paragraphEnd = end and: [ paragraphStart < start and: [ end > 0 and: [ (string at: end) isLineSeparator not ]]]) ifTrue: [
			paragraphAttributes _ (self attributesAt: paragraphStart) select: [ :attr | attr isParagraphAttribute ]].

		self privateSetParagraphAttributes: paragraphAttributes from: paragraphStart to: paragraphEnd.
		paragraphEnd _ paragraphStart - 1.
		paragraphStart > start ] whileTrue.
	runs coalesce! !


!Text class methodsFor: 'instance creation' stamp: 'jmv 3/13/2012 11:01'!
                    fromUser	"Answer an instance of me obtained by requesting the user to type a string."	"Text fromUser"	^ self fromString:		(FillInTheBlank request: 'Enter text followed by [Return]')! !


!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 22:46'!
                 composeAllRectangles: rectangles

	| charIndexBeforeLine numberOfLinesBefore reasonForStopping |

	actualHeight _ 1.
	charIndexBeforeLine _ currCharIndex.
	numberOfLinesBefore _ lines size.
	reasonForStopping _ self composeEachRectangleIn: rectangles.

	currentY _ currentY + actualHeight.
	currentY > extentForComposing y ifTrue: [
		"Oops -- the line is really too high to fit -- back out"
		currCharIndex _ charIndexBeforeLine.
		lines size - numberOfLinesBefore timesRepeat: [lines removeLast].
		^self
	].
	
	"It's OK -- the line still fits."
	maxRightX _ maxRightX max: scanner rightX.
	1 to: rectangles size - 1 do: [ :i |
		"Adjust heights across rectangles if necessary"
		(lines at: lines size - rectangles size + i)
			lineHeight: lines last lineHeight
			baseline: lines last baseline
	].

	anythingComposed _ true.
	isFirstLine _ currCharIndex = 1 or: [
		 (theText at: currCharIndex-1) isLineSeparator].

	reasonForStopping == #columnBreak ifTrue: [^nil].
	currCharIndex > theText size ifTrue: [
		^nil		"we are finished composing"
	]! !

!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 22:45'!
                     composeEachRectangleIn: rectangles 
	| myLine lastChar |
	1 to: rectangles size
		do: 
			[:i | 
			currCharIndex <= theText size ifFalse: [^false].
			myLine _ scanner 
						composeFrom: currCharIndex
						inRectangle: (rectangles at: i)
						firstLine: isFirstLine
						leftSide: i = 1
						rightSide: i = rectangles size.
			lines addLast: myLine.
			actualHeight := actualHeight max: myLine lineHeight.	"includes font changes"
			currCharIndex := myLine last + 1.
			lastChar _ theText at: myLine last.
			lastChar isLineSeparator ifTrue: [^#newLine]].
	^false! !

!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 22:45'!
                    composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY text: argText extentForComposing: argExtentForComposing

	lines _ argLinesCollection.
	theText _ argText.
	extentForComposing _ argExtentForComposing.
	deltaCharIndex _ argDelta.
	currCharIndex _ startCharIndex _ argStart.
	stopCharIndex _ argStop.
	prevLines _ argPriorLines.
	currentY _ argStartY.
	maxRightX _ 0.
	possibleSlide _ stopCharIndex < theText size.
	nowSliding _ false.
	prevIndex _ 1.
	scanner _ CompositionScanner new text: theText.
	isFirstLine _ currCharIndex = 1 or: [
		 (theText at: currCharIndex-1) isLineSeparator ].
	anythingComposed _ false.
	self composeAllLines.
	(anythingComposed not or: [ theText last isLineSeparator ])
		ifTrue: [
			self addEmptyTrailingLine ].
	^{lines asArray. maxRightX}

! !


!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 3/13/2012 12:56'!
selection	"Answer the text that is currently selected."	| t |	t _ model actualContents.	^Text streamContents: [ :strm |		"Multiple selection"		selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock | | toAppend |			toAppend _ t copyFrom: startBlock stringIndex to: stopBlock stringIndex - 1.			toAppend size > 0 ifTrue: [				strm nextPutAll: toAppend.				strm withAttributes: (toAppend attributesAt: toAppend size) do: [ strm newLine ]]].		"Regular selection"		strm nextPutAll: ( t copyFrom: self startIndex to: self stopIndex - 1 ) ]! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 3/14/2012 08:30'!
                              inOutdent: aKeyboardEvent delta: delta	"Add/remove a tab at the front of every line occupied by the selection. 	Derived from work by Larry Tesler back in December 1985.  Now triggered by Cmd-L and Cmd-R.  2/29/96 sw"	"This is a user command, and generates undo"	| realStart realStop lines startLine stopLine start stop adjustStart indentation size numLines inStream newString outStream |	"Operate on entire lines, but remember the real selection for re-highlighting later"	realStart _ self startIndex.	realStop _ self stopIndex - 1.	"Special case a caret on a line of its own, including weird case at end of paragraph"	(realStart > realStop and: [				realStart < 2 or: [(self privateCurrentString at: realStart - 1) isLineSeparator ]])		ifTrue: [			delta < 0				ifTrue: [					morph flash]				ifFalse: [					self replaceSelectionWith: Character tab asSymbol.					self deselectAndPlaceCursorAt: realStart + 1].			^true].	lines _ paragraph lines.	startLine _ paragraph lineIndexFor: realStart.	stopLine _ paragraph lineIndexFor: (realStart max: realStop).	start _ (lines at: startLine) first.	stop _ (lines at: stopLine) last.		"Pin the start of highlighting unless the selection starts a line"	adjustStart _ realStart > start.	"Find the indentation of the least-indented non-blank line; never outdent more"	indentation _ (startLine to: stopLine) inject: 1000 into: [ :previousValue :each |		previousValue min: (paragraph indentationOfLineIndex: each ifBlank: [ :tabs | 1000 ])].				size _  stop + 1 - start.	numLines _ stopLine + 1 - startLine.	inStream _ ReadStream on: self privateCurrentString from: start to: stop.	newString _ String new: size + ((numLines * delta) max: 0).	outStream _ ReadWriteStream on: newString.	"This subroutine does the actual work"	self indent: delta fromStream: inStream toStream: outStream.	"Adjust the range that will be highlighted later"	adjustStart ifTrue: [realStart _ (realStart + delta) max: start].	realStop _ realStop + outStream position - size.	"Prepare for another iteration"	indentation _ indentation + delta.	size _ outStream position.	inStream _ outStream setFrom: 1 to: size.	outStream		ifNil: [ 	"tried to outdent but some line(s) were already left flush"			morph flash]		ifNotNil: [			self selectInvisiblyFrom: start to: stop.			size = newString size ifFalse: [newString _ outStream contents].			self replaceSelectionWith: newString].	self selectFrom: realStart to: realStop. 	"highlight only the original range"	^ true! !

!TextEditor methodsFor: 'private' stamp: 'jmv 3/14/2012 08:33'!
                          indent: delta fromStream: inStream toStream: outStream
	"Append the contents of inStream to outStream, adding or deleting delta or -delta	 tabs at the beginning, and after every NewLine except a final NewLine.  Do not add tabs	 to totally empty lines, and be sure nothing but tabs are removed from lines."

	| ch skip tab prev atEnd |
	tab _ Character tab.
	delta > 0
		ifTrue: [
			"shift right"
			prev _ Character newLineCharacter.
			[
			ch _ (atEnd _ inStream atEnd)
				ifTrue: [ Character newLineCharacter ]
				ifFalse: [ inStream next ].
			(prev isLineSeparator and: [ ch isLineSeparator not ]) ifTrue: [ delta timesRepeat: [ outStream nextPut: tab ]].
			atEnd ] whileFalse: [
				outStream nextPut: ch.
				prev _ ch ]]
		ifFalse: [
			"shift left"
			skip _ delta.
			"a negative number"
			[ inStream atEnd ] whileFalse: [
				((ch _ inStream next) == tab and: [ skip < 0 ]) ifFalse: [ outStream nextPut: ch ].
				skip _ ch isLineSeparator
					ifTrue: [ delta ]
					ifFalse: [ skip + 1 ]]]! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 3/13/2012 23:07'!
                              browseClassFromIt
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."

	| aClass |
	self wordSelectAndEmptyCheck: [^ self].

	aClass _ Utilities
		classFromPattern: self selection string withBlanksCondensed
		withCaption: 'choose a class to browse...'.
	aClass ifNil: [^ morph flash].

	HierarchyBrowserWindow
		onClass: aClass
		selector: nil! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 3/13/2012 23:14'!
          explain
	"Try to shed some light on what kind of entity the current selection is. 
	The selection must be a single token or construct. Insert the answer after 
	the selection. Send private messages whose names begin with 'explain' 
	that return a string if they recognize the selection, else nil."

	| string tiVars cgVars selectors delimitors numbers sorry reply symbol provider |
Cursor execute showWhile: [
			sorry _ '"Sorry, I can''t explain that.  Please select a single
token, construct, or special character.'.
			sorry _ sorry , (morph canDiscardEdits
							ifFalse: ['  Also, please cancel or accept."']
							ifTrue: ['"']).
			(string _ self selection asString) isEmpty
				ifTrue: [reply _ '']
				ifFalse: [
					string _ string withBlanksTrimmed.
					"Remove space, tab, cr"
					"Temps and Instance vars need only test strings that are all letters"
					(string detect: [:char | char isGenerallyValidInIdentifiers not]
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
			self afterSelectionInsertAndSelect: reply]! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 3/13/2012 23:08'!
              selectedSymbol
	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"

	| aString |
	self hasCaret ifTrue: [^ nil].
	aString _ self selection string reject: [ :a | a isSeparator ].
	aString size = 0 ifTrue: [^ nil].
	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].

	^ nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 3/13/2012 23:14'!
                         explainAnySel: symbol 
	"Is this any message selector?"

	| list reply |
	list _ Smalltalk allClassesImplementing: symbol.
	list size = 0 ifTrue: [^nil].
	list size < 12
		ifTrue: [reply _ ' is a message selector which is defined in these classes ' , list printString]
		ifFalse: [reply _ ' is a message selector which is defined in many classes'].
	^'"' , symbol , reply , '."' , '\' withNewLines, 'Smalltalk browseAllImplementorsOf: #' , symbol! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 3/13/2012 23:14'!
                        explainChar: string
	"Does string start with a special character?"

	| char |
	char _ string at: 1.
	char = $. ifTrue: [^'"Period marks the end of a Smalltalk statement.  A period in the middle of a number means a decimal point.  (The number is an instance of class Float)."'].
	char = $' ifTrue: [^'"The characters between two single quotes are made into an instance of class String"'].
	char = $" ifTrue: [^'"Double quotes enclose a comment.  Smalltalk ignores everything between double quotes."'].
	char = $# ifTrue: [^'"The characters following a hash mark are made into an instance of class Symbol.  If parenthesis follow a hash mark, an instance of class Array is made.  It contains literal constants."'].
	(char = $( or: [char = $)]) ifTrue: [^'"Expressions enclosed in parenthesis are evaluated first"'].
	(char = $[ or: [char = $]]) ifTrue: [^'"The code inside square brackets is an unevaluated block of code.  It becomes an instance of BlockContext and is usually passed as an argument."'].
	(char = ${ or: [char = $}]) ifTrue: [^ '"A sequence of expressions separated by periods, when enclosed in curly braces, are evaluated to yield the elements of a new Array"'].
	(char = $< or: [char = $>]) ifTrue: [^'"<primitive: xx> means that this method is usually preformed directly by the virtual machine.  If this method is primitive, its Smalltalk code is executed only when the primitive fails."'].
	char = $^ ifTrue: [^'"Uparrow means return from this method.  The value returned is the expression following the ^"'].
	char = $| ifTrue: [^'"Vertical bars enclose the names of the temporary variables used in this method.  In a block, the vertical bar separates the argument names from the rest of the code."'].
	char = $_ ifTrue: [^'"Left arrow means assignment.  The value of the expression after the left arrow is stored into the variable before it."'].
	char = $; ifTrue: [^'"Semicolon means cascading.  The message after the semicolon is sent to the same object which received the message before the semicolon."'].
	char = $: ifTrue: [^'"A colon at the end of a keyword means that an argument is expected to follow.  Methods which take more than one argument have selectors with more than one keyword.  (One keyword, ending with a colon, appears before each argument).', '\\' withNewLines, 'A colon before a variable name just inside a block means that the block takes an agrument.  (When the block is evaluated, the argument will be assigned to the variable whose name appears after the colon)."'].
	char = $$ ifTrue: [^'"The single character following a dollar sign is made into an instance of class Character"'].
	char = $- ifTrue: [^'"A minus sign in front of a number means a negative number."'].
	char = $e ifTrue: [^'"An e in the middle of a number means that the exponent follows."'].
	char = $r ifTrue: [^'"An r in the middle of a bunch of digits is an instance of Integer expressed in a certain radix.  The digits before the r denote the base and the digits after it express a number in that base."'].
	char = Character space ifTrue: [^'"the space Character"'].
	char = Character tab ifTrue: [^'"the tab Character"'].
	char = Character crCharacter ifTrue: [^'"the carriage return Character"'].
	char = Character lfCharacter ifTrue: [^'"the line feed Character"'].
	^nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 3/13/2012 23:14'!
                          explainClass: symbol 
	"Is symbol a class variable or a pool variable?"
	| provider class reply classes |
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass)
		ifFalse: [^ nil].
	(class _ provider selectedClassOrMetaClass) ifNil: [^ nil].
	"no class is selected"
	(class isKindOf: Metaclass)
		ifTrue: [class _ class soleInstance].
	classes _ (Array with: class)
				, class allSuperclasses.
	"class variables"
	reply _ classes detect: [:each | (each classVarNames detect: [:name | symbol = name]
					ifNone: nil)
					notNil]
				ifNone: nil.
	reply ifNotNil: [
		^ '"is a class variable, defined in class ' , reply printString , '"\' withNewLines , 
		'SystemNavigation new browseAllCallsOn: (' , reply printString , ' classPool associationAt: #' , symbol , ').'].
	"pool variables"
	classes do: [:each | (each sharedPools
			detect: [:pool | (pool includesKey: symbol)
					and: 
						[reply _ pool.
						true]]
			ifNone: nil)
			notNil].
	reply
		ifNil: [(Undeclared includesKey: symbol)
				ifTrue: [
					^ '"is an undeclared variable.' , '"\' withNewLines , 
					'SystemNavigation new browseAllCallsOn: (Undeclared associationAt: #' , symbol , ').']]
		ifNotNil: 
			[classes _ WriteStream on: Array new.
			Smalltalk
				allBehaviorsDo: [:each | (each sharedPools
						detect: 
							[:pool | 
							pool == reply]
						ifNone: nil)
						notNil ifTrue: [classes nextPut: each]].
			"Perhaps not print whole list of classes if too long. (unlikely)"
			^ '"is a pool variable from the pool ' , (Smalltalk keyAtIdentityValue: reply) asString , 
			', which is used by the following classes ' , classes contents printString , '"\' withNewLines , 
			'SystemNavigation new browseAllCallsOn: (' , (Smalltalk keyAtIdentityValue: reply) asString , 
			' bindingOf: #' , symbol , ').'].
	^ nil! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 3/13/2012 23:14'!
               explainGlobal: symbol 
	"Is symbol a global variable?"
	| reply classes |
	reply _ Smalltalk at: symbol ifAbsent: [^nil].
	(reply class == Dictionary or:[reply isKindOf: SharedPool class])
		ifTrue: 
			[classes _ Set new.
			Smalltalk allBehaviorsDo: [:each | (each sharedPools detect: [:pool | pool == reply]
					ifNone: nil)
					ifNotNil: [classes add: each]].
			classes _ classes printString.
			^'"is a global variable.  It is a pool which is used by the following classes ' , (classes allButFirst: 5) , '"'].
	(reply isKindOf: Behavior)
		ifTrue: [^'"is a global variable.  ' , symbol , ' is a class in category ', reply category,
			'."', '\' withNewLines, 'Browser newOnClass: ' , symbol , '.'].
	symbol == #Smalltalk ifTrue: [^'"is a global.  Smalltalk is the only instance of SystemDictionary and holds all global variables."'].
	^'"is a global variable.  ' , symbol , ' is ' , reply printString , '"'! !

!SmalltalkEditor methodsFor: 'explain' stamp: 'jmv 3/13/2012 23:14'!
    explainInst: string 
	"Is string an instance variable of this class?"
	| classes cls provider |
	provider _ self codeProvider.
	(provider respondsTo: #selectedClassOrMetaClass) ifTrue: [
		cls _ provider selectedClassOrMetaClass].
	cls ifNil: [^ nil].	  "no class known"
	classes _ (Array with: cls)
				, cls allSuperclasses.
	classes _ classes detect: [:each | (each instVarNames
			detect: [:name | name = string] ifNone: nil)
			notNil] ifNone: [^nil].
	classes _ classes printString.
	^ '"is an instance variable of the receiver; defined in class ' , classes , 
		'"\' withNewLines , classes , ' systemNavigation browseAllAccessesTo: ''' , string , ''' from: ', classes, '.'! !

!SmalltalkEditor methodsFor: 'new selection' stamp: 'jmv 3/13/2012 11:39'!
         selectWord	"Select delimited text or word--the result of double-clicking."	| leftDelimiters rightDelimiters |	"Warning. Once me (jmv) added Character crCharacter to the delimiters, to make double-click at and of line select whole line.	This had the bad effect that if a class name is the last word of a line, double-click would correctly select it, but after that,	doing ctrl-b to browse it would select the whole line..."	leftDelimiters _ '([{<|''"'.	rightDelimiters _ ')]}>|''"'.	^self selectWordLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters! !

!SmalltalkEditor methodsFor: 'accessing-selection' stamp: 'jmv 3/13/2012 12:45'!
                selection	"Answer the text that is currently selected.	Redefined for Smalltalk code: if there's no regular selection, and all the selectionBlocks contain the same string,	answer that string."	| t regularSelection allPartsEqual samePart |	t _ model actualContents.	regularSelection _ ( t copyFrom: self startIndex to: self stopIndex - 1 ).	allPartsEqual _ true.	samePart _ nil.	^Text streamContents: [ :strm |		"Multiple selection"		selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock | | toAppend |			toAppend _ t copyFrom: startBlock stringIndex to: stopBlock stringIndex - 1.			toAppend size > 0 ifTrue: [				samePart					ifNil: [ samePart _ toAppend ]					ifNotNil: [						allPartsEqual _ allPartsEqual and: [ samePart = toAppend ]].				strm nextPutAll: toAppend.				strm withAttributes: (toAppend attributesAt: toAppend size) do: [ strm newLine ]].			].		(allPartsEqual and: [ regularSelection isEmpty ]) ifTrue: [			^samePart ifNil: ['']].		"Regular selection"		strm nextPutAll: regularSelection ]! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 3/13/2012 16:17'!
         initializeBasicCmdKeyShortcuts	"Initialize the (unshifted) command-key (or alt-key) shortcut table."	"NOTE: if you don't know what your keyboard generates, use Sensor test"	"	Editor initialize	"	| cmdMap cmds |	cmdMap := Array new: 256 withAll: #noop:.		"use temp in case of a crash"	cmdMap at: 32 + 1 put: #selectWord:.			"space bar key"			'([{''"<' do: [:char | cmdMap at: char asciiValue + 1 put: #enclose:].	"arranged in QWERTY keyboard order"	cmds _ #(		$w #backWord:		$a #selectAll:		$f #find:		$g #findAgain:		$h #setSearchString:		$z #undo:		$x #cut:		$c #copySelection:		$v #paste:		$R	#indent:		$Y	#makeUppercase:		$U	#changeLineEndsToLf:		$H	#cursorTopHome:		$L	#outdent:"		$Z	#makeCapitalized:"		$Z	#redo:		$X	#makeLowercase:		$C	#compareToClipboard:	).	1 to: cmds size		by: 2		do: [ :i | cmdMap at: (cmds at: i) asciiValue + 1 put: (cmds at: i + 1)].	cmdActions _ cmdMap! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 3/13/2012 12:56'!
                         worldMenuHelp	| aList aMenu cnts explanation |	"self currentWorld primaryHand worldMenuHelp"	aList _ OrderedCollection new.	#(helpMenu changesMenu openMenu debugMenu windowsMenu appearanceMenu) 		with:	#('help' 'changes' 'open' 'debug' 'windows' 'appearance' ) do:		[:sel :title | aMenu _ self perform: sel.			aMenu items do:				[:it | (((cnts _ it contents) = 'keep this menu up') or: [cnts isEmpty])					ifFalse: [aList add: (cnts, ' - ', title)]]].	aList _ aList asArray sort: [:a :b | a asLowercase < b asLowercase].	explanation _ String streamContents: [:aStream | aList do:		[ :anItem | aStream nextPutAll: anItem; newLine ]].	(TextModel new contents: explanation)		openLabel: 'Where in the world menu is...'! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 3/13/2012 12:04'!
             clear	| stream |	accessSemaphore critical: [		"Having at least one entry simplifies handling of the entries circular collection"		firstIndex _ 1.		lastIndex _ 1.		entries at: 1 put: 'Transcript'.			unfinishedEntry reset.				logToFile ifTrue: [			stream _ StandardFileStream forceNewFileNamed: self filename.			[				stream nextPutAll: 'Transcript log started: '.				DateAndTime now printOn: stream.				stream					newLine;					nextPutAll: '------------------------------------------------------------------------';					newLine			] ensure: [ stream close ]]].	self display! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 3/13/2012 12:04'!
                              clearFile	| stream |	accessSemaphore critical: [		stream _ StandardFileStream forceNewFileNamed: self filename.		[			stream nextPutAll: 'Transcript log started: '.			DateAndTime now printOn: stream.			stream				newLine;				nextPutAll: '------------------------------------------------------------------------';				newLine		] ensure: [ stream close ]]! !

!Transcript class methodsFor: 'private' stamp: 'jmv 3/13/2012 12:04'!
            addEntry: aString	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."	| msg now |	logToFile ifTrue: [		now _ DateAndTime now.		msg _ String streamContents: [ :strm |			now printWithMsOn: strm.			strm				nextPutAll: ' process:';				nextPutAll: Processor activeProcess priority printString;				nextPut: $ ;				nextPutAll: Processor activeProcess hash printString;				nextPut: $ ;				nextPutAll: aString;				newLine ]].	self addEntry: aString logToFile: msg! !

!Transcript class methodsFor: 'private' stamp: 'jmv 3/13/2012 12:56'!
                contents	^String streamContents: [ :strm |		self entriesDo: [ :e |			strm nextPutAll: e; newLine ]]! !


!UndeclaredVariableWarning methodsFor: 'exceptionDescription' stamp: 'jmv 3/13/2012 12:59'!
                        defaultAction	"The user should be notified of the occurrence of an exceptional occurrence and	 given an option of continuing or aborting the computation. The description of the	 occurrence should include any text specified as the argument of the #signal: message."		selector		ifNotNil: [Transcript newLine; nextPutAll: class name, '>>', selector, ' ']		ifNil: [Transcript newLine ].	Transcript show: '(' , name , ' is Undeclared) '.	^true! !


!UndefinedObject methodsFor: 'class hierarchy' stamp: 'jmv 3/13/2012 12:57'!
              subclass: nameOfClass  	instanceVariableNames: instVarNames	classVariableNames: classVarNames	poolDictionaries: poolDictnames	category: category	"Calling this method is now considered an accident.  If you really want to create a class with a nil superclass, then create the class and then set the superclass using #superclass:"	Transcript show: ('Attempt to create ', nameOfClass, ' as a subclass of nil.  Possibly a class is being loaded before its superclass.'); newLine.	^ProtoObject		subclass: nameOfClass		instanceVariableNames: instVarNames		classVariableNames: classVarNames		poolDictionaries: poolDictnames		category: category! !


!UnhandledError methodsFor: 'priv handling' stamp: 'jmv 3/13/2012 12:59'!
         runtimeDefaultAction	"Dump the stack trace to a log file, then exit the program (image)."	| file |	file := FileStream newFileNamed: ('error', Utilities dateTimeSuffix, FileDirectory dot, 'log') asFileName.	Smalltalk timeStamp: file.	(thisContext sender stackOfSize: 20) do: [:ctx | file newLine. ctx printOn: file].	file close.	Smalltalk snapshot: false andQuit: true! !


!UnknownSelector methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 23:14'!
                 openMenuIn: aBlock
	| alternatives labels lines caption choice |
	alternatives := Symbol possibleSelectorsFor: name.
	labels := Array streamContents:
				[:s | s nextPut: name; nextPutAll: alternatives; nextPut: 'cancel'].
	lines := {1. alternatives size + 1}.
	caption := 'Unknown selector, please\confirm, correct, or cancel' withNewLines.
	
	choice := aBlock value: labels value: lines value: caption.
	choice = 0 ifTrue: [self resume: nil].
	choice = 1 ifTrue: [self resume: name asSymbol].
	choice = labels size ifTrue: [self resume: nil].
	self resume: (alternatives at: choice - 1)! !


!UnusedVariable methodsFor: 'as yet unclassified' stamp: 'jmv 3/13/2012 23:14'!
                              openMenuIn: aBlock
	| index |
	index := aBlock value: #('yes' 'no')
					value: #()
					value: name, ' appears to be\unused in this method.\OK to remove it?' withNewLines.
	self resume: index = 1! !


!Utilities class methodsFor: 'common requests' stamp: 'jmv 3/13/2012 17:02'!
         appendToCommonRequests: aString	self commonRequestStrings: (CommonRequestStrings contents, String newLineString, aString)"Utilities appendToCommonRequests: 'self beep'"! !

!Utilities class methodsFor: 'common requests' stamp: 'jmv 3/13/2012 13:00'!
  eval: aString	"Evaluate the string in a neutral context, and under certain circumstances print the 	result in the transcript"	| result |	result _ Smalltalk actualCompilerClass new evaluate: aString in: nil to: nil.	(result isKindOf: Number) | (result isKindOf: String)		ifTrue: [			Transcript newLine; nextPutAll: result printString]! !

!Utilities class methodsFor: 'identification' stamp: 'jmv 3/13/2012 12:57'!
                          methodsWithInitials: targetInitials	"Based on a do-it contributed to the Squeak mailing list by Gšran Hultgen:¥ Browse methods whose initials (in the time-stamp, as logged to disk) match the given initials.¥ Print out the complete time-stamp table to the Transcript.¥ Answer a list of (initials -> count) associations.CAUTION: It may take several minutes for this to complete."	"Time millisecondsToRun: [Utilities methodsWithInitials: 'bf']"	| initials timeStamp  allSubmitters |	initials _ ''.	timeStamp _ ''.	allSubmitters _ Bag new.	Smalltalk		browseAllSelect:			[:cm |				timeStamp _ Utilities timeStampForMethod: cm.				initials _ timeStamp isEmpty					ifTrue:						['']					ifFalse:						[timeStamp substrings first].				initials _ initials isEmpty					ifTrue:						['<no time stamp>']					ifFalse:						[initials first isDigit							ifTrue:								['<date>']							ifFalse:								[initials]].				allSubmitters add: initials.				(initials = targetInitials)]		name: ('Methods with initials ', targetInitials)		autoSelect: nil.	allSubmitters sortedCounts do: [:elem | Transcript newLine; show: elem asString].	^ allSubmitters! !

!Utilities class methodsFor: 'investigations' stamp: 'jmv 3/13/2012 17:03'!
                              reportSenderCountsFor: selectorList	"Produce a report on the number of senders of each of the selectors in the list.  1/27/96 sw"	| total report thisSize |	total _ 0.	report _ ''.	selectorList do: [ :selector |		thisSize _ (Smalltalk allCallsOn: selector) size.		report _ report, thisSize printString, String tab, selector printString, String newLineString.		total _ total + thisSize].	report _ report, '--- ------------------'.	report _ report, total printString, String tab, 'TOTAL'.	^ report! !

!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 3/13/2012 12:59'!
                   cleanseOtherworldlySteppers	"If the current project is a morphic one, then remove from its steplist 	those morphs that are not really in the world"	| old delta |	old _ self currentWorld stepListSize.	self currentWorld steppingMorphsNotInWorld		do: [:m | m delete].	self currentWorld cleanseStepList.	(delta _ old - self currentWorld stepListSize) > 0		ifTrue: [Transcript newLine; show: delta asString , ' morphs removed from steplist'			"Utilities cleanseOtherworldlySteppers"]! !

!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 3/13/2012 12:57'!
        createPageTestWorkspace	"Used to generate a workspace window for testing page up and page down stuff."	"Utilities createPageTestWorkspace"	| numberOfLines maxStringLength minLineCounterSize lineCounterSize offsetSize stream headerConstant prevStart prevStrLen prevLineNumber stringLen lineNumber start log pad charIndex char |	numberOfLines _ 400.	maxStringLength _ 22.	minLineCounterSize _ 3.	lineCounterSize _ (numberOfLines log asInteger + 1) max: minLineCounterSize.	offsetSize _ 5.	stream _ ReadWriteStream on: ''.	headerConstant _ lineCounterSize + 1 + offsetSize + 1.	prevStart _ headerConstant negated.	prevStrLen _ 0.	prevLineNumber _ 0.	numberOfLines timesRepeat: [		stringLen _ maxStringLength atRandom max: 1.		lineNumber _ prevLineNumber + 1.		start _ prevStart + prevStrLen + headerConstant + 1.		prevStart _ start.		prevStrLen _ stringLen.		prevLineNumber _ lineNumber.		log _ lineNumber log asInteger.		pad _ lineCounterSize - log - 1.		pad timesRepeat: [stream nextPutAll: '0'].		stream nextPutAll: lineNumber printString.		stream space.		log _ start log asInteger.		pad _ offsetSize - log - 1.		pad timesRepeat: [stream nextPutAll: '0'].		stream nextPutAll: start printString.		stream space.		charIndex _ 'a' first asInteger.		stringLen timesRepeat: [			char _ Character value: charIndex.			charIndex _ charIndex + 1.			stream nextPut: char].		lineNumber = numberOfLines ifFalse: [stream newLine]		].	(Workspace new contents: stream contents) openLabel: 'Test Data'.! !

!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 3/13/2012 13:00'!
                            decommissionTheAllCategory	"Utilities decommissionTheAllCategory"	"Moves all methods that are in a category named 'all' into the default 'as yet unclassified' category"	| org aCategory methodCount classCount any |	methodCount _ 0.	classCount _ 0.	Smalltalk allBehaviorsDo: [ :aClass |		org _ aClass organization.		any _ false.		aClass selectorsDo: [ :aSelector |			aCategory _ org categoryOfElement: aSelector.			aCategory = #all ifTrue: [				org					classify: aSelector					under: ClassOrganizer default					suppressIfDefault: false.				methodCount _ methodCount + 1.				any _ true ]].		any ifTrue: [ classCount _ classCount + 1 ].		org removeEmptyCategories ].	Transcript		 newLine;		 show: methodCount printString , ' methods in ' , classCount printString , ' classes movedfrom "all" to "as yet unclassified"'.! !

!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 3/13/2012 13:00'!
            garbageCollectAndReport	"Do a garbage collection, and report results to the user."	| cc reportString |	reportString _ String streamContents: [ :aStream |		aStream nextPutAll: Smalltalk bytesLeftString.		Smalltalk			at: #Command			ifPresent: [ :cmdClass |				(cc _ cmdClass instanceCount) > 0 ifTrue: [					aStream						 newLine;						 nextPutAll: '(note: there are ' , cc printString , ' undo record(s) present in yoursystem; purging them may free up more space.)' ]]].	self inform: reportString! !

!Utilities class methodsFor: 'miscellaneous' stamp: 'jmv 3/13/2012 22:57'!
                    instanceComparisonsBetween: fileName1 and: fileName2
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		Smalltalk printSpaceAnalysis: 0 on: 'STspace.text1'.
			--- do something that uses space here ---
		Smalltalk printSpaceAnalysis: 0 on: 'STspace.text2'.
		Smalltalk instanceComparisonsBetween: 'STspace.text1' and 'STspace.text2'"

	| instCountDict report f aString items className newInstCount oldInstCount newSpace oldPair oldSpace |
	instCountDict _ Dictionary new.
	report _ ReadWriteStream on: ''.
	f _ FileStream readOnlyFileNamed: fileName1.
	[f atEnd] whileFalse: [
		aString _ f crLfNextLine.
		items _ aString findTokens: ' '.
		(items size = 4 or: [items size = 5]) ifTrue:
			[instCountDict at: items first put: (Array with: items third asNumber with: items fourth asNumber)]].
	f close.

	f _ FileStream readOnlyFileNamed: fileName2.
	[f atEnd] whileFalse: [
		aString _ f crLfNextLine.
		items _ aString findTokens: ' '.
		(items size = 4 or: [items size = 5]) ifTrue:
			[className _ items first.
			newInstCount _ items third asNumber.
			newSpace _ items fourth asNumber.
			oldPair _ instCountDict at: className ifAbsent: nil.
			oldInstCount _ oldPair ifNil: [0] ifNotNil: [oldPair first].
			oldSpace _ oldPair ifNil: [0] ifNotNil: [oldPair second].
			oldInstCount ~= newInstCount ifTrue:
				[report nextPutAll: (newInstCount - oldInstCount) printString; tab; nextPutAll: (newSpace - oldSpace) printString; tab; nextPutAll: className asString; newLine]]].
	f close.

	(TextModel new contents: report contents)
		openLabel: 'Instance count differentials between ', fileName1, ' and ', fileName2! !

!Utilities class methodsFor: 'summer97 additions' stamp: 'jmv 3/13/2012 11:37'!
                              classFromPattern: pattern withCaption: aCaption	"If there is a class whose name exactly given by pattern, return it.	If there is only one class in the system whose name matches pattern, return it.	Otherwise, put up a menu offering the names of all classes that match pattern, and return the class chosen, else nil if nothing chosen.	This method ignores tab, space, & cr characters in the pattern"	| toMatch potentialClassNames classNames exactMatch index |	(toMatch _  pattern copyWithoutAll:			{Character space.  Character crCharacter.  Character tab})		isEmpty ifTrue: [^ nil].	Symbol hasInterned: toMatch ifTrue:		[:patternSymbol | Smalltalk at: patternSymbol ifPresent:			[:maybeClass | (maybeClass isKindOf: Class) ifTrue: [^ maybeClass]]].	toMatch _ (toMatch copyWithout: $.) asLowercase.	potentialClassNames _ Smalltalk classNames asOrderedCollection.	classNames _ pattern last = $. 		ifTrue: [potentialClassNames select:					[:nm |  nm asLowercase = toMatch]]		ifFalse: [potentialClassNames select: 					[:n | n includesSubstring: toMatch caseSensitive: false]].	classNames isEmpty ifTrue: [^ nil].	exactMatch _ classNames detect: [:each | each asLowercase = toMatch] ifNone: nil.	index _ classNames size = 1		ifTrue:	[1]		ifFalse:	[exactMatch			ifNil: [(PopUpMenu labelArray: classNames lines: #()) startUpWithCaption: aCaption]			ifNotNil: [classNames addFirst: exactMatch.				(PopUpMenu labelArray: classNames lines: #(1)) startUpWithCaption: aCaption]].	index = 0 ifTrue: [^ nil].	^ Smalltalk at: (classNames at: index) asSymbol"	Utilities classFromPattern: 'CharRecog'	Utilities classFromPattern: 'rRecog'	Utilities classFromPattern: 'znak'	Utilities classFromPattern: 'orph'"! !

!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 3/13/2012 13:00'!
                reportCPUandRAM	"Write several text files with useful analysis for profiling purposes.	Overwrites any existing report.	Utilities reportCPUandRAM	"		| stream tally |		"VM statistics (Memory use and GC, mainly)"	stream _ FileStream forceNewFileNamed: 'MemoryStats.txt'.	[ stream nextPutAll: Utilities vmStatisticsReportString ] 		ensure: [ stream close ].		"Process list"	stream _ FileStream forceNewFileNamed: 'ProcessList.txt'.	[		ProcessBrowser new processNameList 			do: [ :each | 				stream nextPutAll: each; newLine ]	] ensure: [ stream close ]."Fork all these, so they run in sequence, as the system is back running"[		"Process taking most CPU"	stream _ FileStream forceNewFileNamed: 'ThePig.txt'.	ProcessBrowser dumpPigStackOn: stream andClose: true.		"Tally of all processes"	stream _ FileStream forceNewFileNamed: 'FullTally.txt'.	[		tally _ MessageTally new.		tally reportOtherProcesses: true.	"actually irrelevant"		tally spyAllEvery: 1 on: [ (Delay forMilliseconds: 1000) wait ].		tally report: stream ] ensure: [ stream close ].		"Tally of UI (perhaps not useful when having all processes....)"	stream _ FileStream forceNewFileNamed: 'UITally.txt'.	[		tally _ MessageTally new.		tally reportOtherProcesses: false.		tally spyEvery: 1 onProcess: ProjectX uiProcessX forMilliseconds: 1000.		tally report: stream ] ensure: [ stream close ].		"Memory Analysis"	stream _ FileStream forceNewFileNamed: 'MemoryAnalysis.txt'.	[ SpaceTally new printSpaceAnalysis: 1 on: stream ]		ensure: [ stream close ]] forkNamed: 'CPU usage analysis'! !

!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 3/13/2012 13:02'!
                           vmStatisticsReportString	"	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'	"	| params oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime upTime2 fullGCs2 fullGCTime2 incrGCs2 incrGCTime2 tenureCount2 str |	params := Smalltalk getVMParameters.	oldSpaceEnd			:= params at: 1.	youngSpaceEnd		:= params at: 2.	memoryEnd			:= params at: 3.	fullGCs				:= params at: 7.	fullGCTime			:= params at: 8.	incrGCs				:= params at: 9.	incrGCTime			:= params at: 10.	tenureCount			:= params at: 11.	upTime := Time millisecondClockValue.	str := WriteStream on: (String new: 1000).	str	nextPutAll: 'uptime			';		print: (upTime / 1000 / 60 // 60); nextPut: $h;		print: (upTime / 1000 / 60 \\ 60) asInteger; nextPut: $m;		print: (upTime / 1000 \\ 60) asInteger; nextPut: $s; newLine.	str	nextPutAll: 'memory			';		nextPutAll: memoryEnd asStringWithCommas; nextPutAll: ' bytes'; newLine.	str	nextPutAll:	'	old			';		nextPutAll: oldSpaceEnd asStringWithCommas; nextPutAll: ' bytes (';		print: ((oldSpaceEnd / memoryEnd * 100) roundTo: 0.1); nextPutAll: '%)'; newLine.	str	nextPutAll: '	young		';		nextPutAll: (youngSpaceEnd - oldSpaceEnd) asStringWithCommas; nextPutAll: ' bytes (';		print: ((youngSpaceEnd - oldSpaceEnd / memoryEnd * 100) roundTo: 0.1); nextPutAll: '%)'; newLine.	str	nextPutAll: '	used		';		nextPutAll: youngSpaceEnd asStringWithCommas; nextPutAll: ' bytes (';		print: ((youngSpaceEnd / memoryEnd * 100) roundTo: 0.1); nextPutAll: '%)'; newLine.	str	nextPutAll: '	free		';		nextPutAll: (memoryEnd - youngSpaceEnd) asStringWithCommas; nextPutAll: ' bytes (';		print: ((memoryEnd - youngSpaceEnd / memoryEnd * 100) roundTo: 0.1); nextPutAll: '%)'; newLine.	str	nextPutAll: 'GCs				';		nextPutAll: (fullGCs + incrGCs) asStringWithCommas.	fullGCs + incrGCs > 0 ifTrue: [		str			nextPutAll: ' ('; 			print: ((upTime / (fullGCs + incrGCs)) roundTo: 1); 			nextPutAll: 'ms between GCs)'	].	str newLine.	str	nextPutAll: '	full			';		print: fullGCs; nextPutAll: ' totalling '; nextPutAll: fullGCTime asStringWithCommas; nextPutAll: 'ms (';		print: ((fullGCTime / upTime * 100) roundTo: 1.0);		nextPutAll: '% uptime)'.	fullGCs = 0 ifFalse: [		str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) roundTo: 1.0); nextPutAll: 'ms'].	str	newLine.	str	nextPutAll: '	incr			';		print: incrGCs; nextPutAll: ' totalling '; nextPutAll: incrGCTime asStringWithCommas; nextPutAll: 'ms (';		print: ((incrGCTime / upTime * 100) roundTo: 1.0);		nextPutAll: '% uptime), avg '; print: ((incrGCTime / incrGCs) roundTo: 1.0); nextPutAll: 'ms'; newLine.	str	nextPutAll: '	tenures		';		nextPutAll: tenureCount asStringWithCommas.	tenureCount = 0 ifFalse: [		str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].	str	newLine.LastStats ifNil: [LastStats := Array new: 6]ifNotNil: [	upTime2 := upTime - (LastStats at: 1).	fullGCs2 := fullGCs - (LastStats at: 2).	fullGCTime2 := fullGCTime - (LastStats at: 3).	incrGCs2 := incrGCs - (LastStats at: 4).	incrGCTime2 := incrGCTime - (LastStats at: 5).	tenureCount2 := tenureCount - (LastStats at: 6).	str	nextPutAll: self textMarkerForShortReport ;		nextPutAll: (fullGCs2 + incrGCs2) asStringWithCommas.	fullGCs2 + incrGCs2 > 0 ifTrue: [		str			nextPutAll: ' ('; 			print: ((upTime2 / (fullGCs2 + incrGCs2)) roundTo: 1); 			nextPutAll: 'ms between GCs)'.	].	str newLine.	str	nextPutAll: '	uptime		'; print: ((upTime2 / 1000.0) roundTo: 0.1); nextPutAll: 's'; newLine.	str	nextPutAll: '	full			';		print: fullGCs2; nextPutAll: ' totalling '; nextPutAll: fullGCTime2 asStringWithCommas; nextPutAll: 'ms (';		print: ((fullGCTime2 / upTime2 * 100) roundTo: 1.0);		nextPutAll: '% uptime)'.	fullGCs2 = 0 ifFalse:		[str	nextPutAll: ', avg '; print: ((fullGCTime2 / fullGCs2) roundTo: 1.0); nextPutAll: 'ms'].	str	newLine.	str	nextPutAll: '	incr			';		print: incrGCs2; nextPutAll: ' totalling '; nextPutAll: incrGCTime2 asStringWithCommas; nextPutAll: 'ms (';		print: ((incrGCTime2 / upTime2 * 100) roundTo: 1.0);		nextPutAll: '% uptime), avg '.	incrGCs2 > 0 ifTrue: [		 str print: ((incrGCTime2 / incrGCs2) roundTo: 1.0); nextPutAll: 'ms'	].	str newLine.	str	nextPutAll: '	tenures		';		nextPutAll: tenureCount2 asStringWithCommas.	tenureCount2 = 0 ifFalse:		[str nextPutAll: ' (avg '; print: (incrGCs2 / tenureCount2) asInteger; nextPutAll: ' GCs/tenure)'].	str	newLine.].	LastStats at: 1 put: upTime.	LastStats at: 2 put: fullGCs.	LastStats at: 3 put: fullGCTime.	LastStats at: 4 put: incrGCs.	LastStats at: 5 put: incrGCTime.	LastStats at: 6 put: tenureCount.	^ str contents! !

!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 3/13/2012 22:57'!
                               vmStatisticsShortString
	"Convenience item for access to recent statistics only"
	"
	(TextModel new contents: Utilities vmStatisticsShortString) openLabel: 'VM Recent Statistics'
	"

	^ (ReadStream on: self vmStatisticsReportString) upToAll: 'Since'; crLfNextLine; upToEnd
! !

!Utilities class methodsFor: 'closure support' stamp: 'jmv 3/13/2012 12:59'!
                             compileUsingClosures	"Utilities compileUsingClosures"	"Recompile the system and do some minimal clean-ups"	| classes compilationErrors |	Preferences setPreference: #allowBlockArgumentAssignment toValue: false.	compilationErrors := Set new.	classes := Smalltalk allClasses reject: [:c| c name == #GeniePlugin].	'Recompiling The System' displayProgressAt: Sensor mousePoint		from: 0 to: classes size during:[:bar |			classes withIndexDo:[:c :i|				bar value: i.				{ c. c class } do:[:b|					"Transcript cr; print: b; endEntry."					b selectors "asArray sort" do: [ :s | 						"Transcript cr; show: b asString, '>>', s."						[b recompile: s from: b] on: Error do:[:ex|							Transcript								newLine; nextPutAll: 'COMPILATION ERROR: ';								print: b; nextPutAll: '>>'; nextPutAll: s.							compilationErrors add: (MethodReference class: b selector: s)]]]]].	compilationErrors notEmpty ifTrue:[		Smalltalk			browseMessageList: compilationErrors asArray sort			name: 'Compilation Errors' ]! !


!VersionsBrowser methodsFor: 'menu' stamp: 'jmv 3/13/2012 11:01'!
   versionsHelpString	^ 'Each entry in the list pane represents a version of the source code for the same method; the topmost entry is the current version, the next entry is the next most recent, etc.To revert to an earlier version, select it (in the list pane) and then do any of the following:  *  Choose "revert to this version" from the list pane menu.  *  Hit the "revert" button,  *  Type Return in the code pane  *  Type cmd-s (alt-s) in the code pane.The code pane shows the source for the selected version.  If "diffing" is in effect, then differences betwen the selected version and the version before it are pointed out in the pane.  Turn diffing on and off by choosing "toggle diffing" from the list pane menu, or hitting the "diffs" button, or hitting cmd-D when the cursor is over the list pane.To get a comparison between the selected version and the current version, choose "compare to current" from the list pane menu or hit the "compare to current" button.  (This is meaningless if the current version is selected, and is unnecessary if you''re interested in diffs from between the current version and the next-most-recent version, since the standard in-pane "diff" feature will give you that.)You can also compare the selected version with any other version using the "compare to version..." menu choice.If further versions of the method in question have been submitted elsewhere since you launched a particular Versions Browser, it will still stay nicely up-to-date if you''re in Morphic and have asked that smart updating be maintained; if you''re in mvc or in morphic but with smart-updating turned off, a versions browser is only brought up to date when you activate its window (and when you issue "revert" from within it, of course,) and you can also use the "update list" command to make certain the versions list is up to date.Hit the "remove from changes" button, or choose the corresponding command in the list pane menu, to have the method in question deleted from the current change set.  This is useful if you''ve put debugging code into a method, and now want to strip it out and cleanse your current change set of all memory of the excursion.Note:  the annotation pane in versions browsers shows information about the *current* version of the method in the image, not about the selected version.'! !


!WeakActionSequence methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:58'!
        printOn: aStream	self size < 2 ifTrue: [^super printOn: aStream].	aStream nextPutAll: '#('.	self		do: [:each | each printOn: aStream]		separatedBy: [aStream newLine].	aStream nextPut: $)! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 3/13/2012 17:03'!
                    handleFatalDrawingError: errMsg	"Handle a fatal drawing error."		Display deferUpdates: false. "Just in case"	self primitiveError: 		'Fatal Morphic drawing error', String newLineString,		errMsg.	"Hm... we should jump into a 'safe' worldState here, but how do we find it?!!"! !


!WriteStream methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 13:00'!
                              timeStamp	"Append the current time to the receiver as a String."	self nextChunkPut:	"double string quotes and !!s"		(String streamContents: [:s | Smalltalk timeStamp: s]) printString.	self newLine! !


!FileStream methodsFor: 'printing' stamp: 'jmv 3/13/2012 12:34'!
                 longPrintOn: aStream limitedTo: sizeLimit indent: indent	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."	aStream newLine! !

!FileStream methodsFor: 'editing' stamp: 'jmv 3/13/2012 16:23'!
                          viewGZipContents	"View the contents of a gzipped file"	| stringContents |	self binary.	stringContents := self contentsOfEntireFile.	stringContents := Cursor wait showWhile: [(GZipReadStream on: stringContents) upToEnd].	stringContents := stringContents asString withCuisLineEndings.	Workspace new		contents: stringContents;		openLabel: 'Decompressed contents of: ', self localName! !


!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 3/13/2012 23:14'!
                              fileExistsUserHandling: fullFileName
	| dir localName choice newName newFullFileName |
	dir _ FileDirectory forFileName: fullFileName.
	localName _ FileDirectory localNameFor: fullFileName.
	choice _ (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir deleteFileNamed: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName _ FillInTheBlank request: 'Enter a new file name' initialAnswer: fullFileName.
		newFullFileName _ self fullName: newName.
		^ self newFileNamed: newFullFileName].

	self error: 'Please close this to abort file opening'! !


!Transcripter methodsFor: 'command line' stamp: 'jmv 3/13/2012 12:57'!
                 readEvalPrint	| line okToRevert |	okToRevert _ true.	[#('quit' 'exit' 'done' ) includes: (line _ self request: '>')] whileFalse: [		line = 'revert' ifTrue: [			okToRevert				ifTrue: [					Utilities revertLastMethodSubmission.					self newLine; show: 'reverted: ' , Utilities mostRecentlySubmittedMessage.					okToRevert _ false]				ifFalse: [self newLine; show: 'Only one level of revert currently supported']]		ifFalse: [self newLine; show: ([Smalltalk actualCompilerClass evaluate: line] ifError: [:err :ex | err])]]! !

!Transcripter methodsFor: 'command line' stamp: 'jmv 3/13/2012 22:43'!
   request: prompt
	| startPos char contents return |
	self
		newLine;
		show: prompt.
	startPos _ position.
	[
		[ Sensor keyboardPressed ] whileFalse.
		return _ Character value: InputSensor returnKey.
		(char _ Sensor keyboard) = return ] whileFalse: [
			char = Character backspace
				ifTrue: [ readLimit _ position _ position - 1 max: startPos ]
				ifFalse: [ self nextPut: char ].
			self endEntry ].
	contents _ self contents.
	^ contents
		copyFrom: startPos + 1
		to: contents size! !


!ZipWriteStream methodsFor: 'encoding' stamp: 'jmv 3/13/2012 12:58'!
            flushBlock: lastBlock	"Send the current block"	| lastFlag bitsRequired method bitsSent	storedLength fixedLength dynamicLength 	blTree lTree dTree blBits blFreq |	lastFlag _ lastBlock ifTrue:[1] ifFalse:[0].	"Compute the literal/length and distance tree"	lTree _ ZipEncoderTree buildTreeFrom: literalFreq maxDepth: MaxBits.	dTree _ ZipEncoderTree buildTreeFrom: distanceFreq maxDepth: MaxBits.	"Compute the bit length tree"	blBits _ lTree bitLengths, dTree bitLengths.	blFreq _ WordArray new: MaxBitLengthCodes.	self scanBitLengths: blBits into: blFreq.	blTree _ ZipEncoderTree buildTreeFrom: blFreq maxDepth: MaxBitLengthBits.	"Compute the bit length for the current block.	Note: Most of this could be computed on the fly but it's getting	really ugly in this case so we do it afterwards."	storedLength _ self storedBlockSize.	fixedLength _ self fixedBlockSizeFor: lTree and: dTree.	dynamicLength _ self dynamicBlockSizeFor: lTree and: dTree 							using: blTree and: blFreq.	VerboseLevel > 1 ifTrue:[		Transcript newLine; show:'Block sizes (S/F/D):';			space; print: storedLength // 8; 			nextPut:$/; print: fixedLength // 8; 			nextPut:$/; print: dynamicLength // 8; space; endEntry].	"Check which method to use"	method _ self forcedMethod.	method ifNil: [ 		method _ (storedLength < fixedLength and:[storedLength < dynamicLength]) 			ifTrue:[#stored]			ifFalse:[fixedLength < dynamicLength ifTrue:[#fixed] ifFalse:[#dynamic]]].	(method == #stored and:[blockStart < 0]) ifTrue:[		"Cannot use #stored if the block is not available"		method _ fixedLength < dynamicLength ifTrue:[#fixed] ifFalse:[#dynamic]].	bitsSent _ encoder bitPosition. "# of bits sent before this block"	bitsRequired _ nil.	(method == #stored) ifTrue:[		VerboseLevel > 0 ifTrue:[Transcript show:'S'].		bitsRequired _ storedLength.		encoder nextBits: 3 put: StoredBlock << 1 + lastFlag.		self sendStoredBlock].	(method == #fixed) ifTrue:[		VerboseLevel > 0 ifTrue:[Transcript show:'F'].		bitsRequired _ fixedLength.		encoder nextBits: 3 put: FixedBlock << 1 + lastFlag.		self sendFixedBlock].	(method == #dynamic) ifTrue:[		VerboseLevel > 0 ifTrue:[Transcript show:'D'].		bitsRequired _ dynamicLength.		encoder nextBits: 3 put: DynamicBlock << 1 + lastFlag.		self sendDynamicBlock: blTree 			literalTree: lTree 			distanceTree: dTree 			bitLengths: blBits].	bitsRequired = (encoder bitPosition - bitsSent)		ifFalse:[self error:'Bits size mismatch'].	lastBlock 		ifTrue:[self release]		ifFalse:[self initializeNewBlock].! !


!ZipWriteStream class methodsFor: 'regression test' stamp: 'jmv 3/13/2012 13:00'!
          compressAndDecompress: aFile using: tempName stats: stats	| fileSize tempFile result |	aFile		ifNil: [^ nil].	fileSize _ aFile size.	(fileSize < 1"00000" "or:[fileSize > 1000000]") ifTrue:[aFile close. ^nil].	Transcript newLine; show:'Testing ', aFile name,' ... '.	tempFile _ StandardFileStream new open: tempName forWrite: true.	'Compressing ', aFile name,'...' displayProgressAt: Sensor mousePoint		from: 1 to: aFile size during:[:bar|			result _ self regressionCompress: aFile into: tempFile notifiying: bar stats: stats].	result ifTrue:[		'Validating ', aFile name,'...' displayProgressAt: Sensor mousePoint			from: 0 to: aFile size during:[:bar|				result _ self regressionDecompress: aFile from: tempFile notifying: bar stats: stats]].	aFile close.	tempFile close.	FileDirectory default deleteFileNamed: tempName ifAbsent: nil.	result ~~ false ifTrue:[		Transcript show:' ok (', (result * 100 truncateTo: 0.01) printString,')'].	^result! !

!ZipWriteStream class methodsFor: 'regression test' stamp: 'jmv 3/13/2012 12:58'!
      logProblem: reason for: aFile	| errFile |	errFile _ FileStream fileNamed:'problems.log'.	errFile position: errFile size.	errFile		newLine;		nextPutAll: aFile name;		newLine;		nextPutAll: reason.	errFile close.	Transcript show:' failed (', reason,')'.	aFile close.	^false! !

!ZipWriteStream class methodsFor: 'regression test' stamp: 'jmv 3/13/2012 15:34'!
               printRegressionStats: stats from: fd	| raw compressed numFiles |	raw _ stats at: #rawSize ifAbsent:[0].	raw = 0 ifTrue:[^self].	compressed _ stats at: #compressedSize ifAbsent:[0].	numFiles _ stats at: #numFiles ifAbsent:[0].	Transcript newLine; nextPutAll: fd pathName.	Transcript newLine; tab; nextPutAll:'Files compressed: ', numFiles asStringWithCommas.	Transcript newLine; tab; nextPutAll:'Bytes compressed: ', raw asStringWithCommas.	Transcript newLine; tab; nextPutAll:'Avg. compression ratio: ';		print: ((compressed / raw asFloat * 100.0) truncateTo: 0.01).	Transcript endEntry.! !

String initialize!
Parser initialize!
CharacterScanner initialize!
