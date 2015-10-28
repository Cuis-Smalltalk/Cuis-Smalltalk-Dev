'From Cuis 4.2 of 25 July 2013 [latest update: #2558] on 28 October 2015 at 9:59:13.66378 am'!

!StandardFileStream methodsFor: 'access' stamp: 'jmv 10/27/2015 17:07'!
fileEntry
	^ self fullName asFileEntry! !


!StrikeFont class methodsFor: 'instance creation' stamp: 'jmv 10/27/2015 17:25'!
create: fontName size: pointSize bold: includeBold italic: includeItalic boldItalic: includeBoldItalic
	"
	self create: 'DejaVu Sans Mono' size: 12 bold: true italic: true boldItalic: true
	"
	| base bold oblique boldOblique point prefix |
	prefix _ 'AdditionalFontData/', fontName.
	point _ pointSize asString.
	base _ [ (StrikeFont new
		buildFromForm: (Form fromFileNamed: prefix, '-0-', point, '.bmp')
		data: (prefix, '-0-', point, '.txt') asFileEntry fileContents substrings
		name: fontName, ' ', point)
			pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ].
	includeBold ifTrue: [
		bold _ [ (StrikeFont new
			buildFromForm: (Form fromFileNamed: prefix, '-1-', point, '.bmp')
			data: (prefix, '-1-', point, '.txt') asFileEntry fileContents substrings
			name: fontName, ' ', point, 'B')
				emphasis: 1;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	includeItalic ifTrue: [
		oblique _ [ (StrikeFont new
			buildFromForm: (Form fromFileNamed: prefix, '-2-', point, '.bmp')
			data: (prefix, '-2-', point, '.txt') asFileEntry fileContents substrings
			name: fontName, ' ', point, 'I')
				emphasis: 2;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	includeBoldItalic ifTrue: [
		boldOblique _ [ (StrikeFont new
			buildFromForm: (Form fromFileNamed: prefix, '-3-', point, '.bmp')
			data: (prefix, '-3-', point, '.txt') asFileEntry fileContents substrings
			name: fontName, ' ', point, 'BI')
				emphasis: 3;
				pointSize: pointSize ] on: FileDoesNotExistException do: [ : ex | ex print. nil ]].
	"We have a regular, base font. Make others derivatives of it"
	base ifNotNil: [
		bold ifNotNil: [
			base derivativeFont: bold at: 1 ].
		oblique ifNotNil: [
			base derivativeFont: oblique at: 2].
		boldOblique ifNotNil: [
			base derivativeFont: boldOblique at: 3 ].
		^base ].
	"We don't have a base, regular font."
	oblique ifNotNil: [
		oblique emphasis: 0.	"Hacky. Non regular fonts can not have derivatives. Should change this?"
		bold ifNotNil: [
			oblique derivativeFont: bold at: 1 ].
		boldOblique ifNotNil: [
			oblique derivativeFont: boldOblique at: 3 ].
		^oblique ].
	bold ifNotNil: [
		bold emphasis: 0.	"Hacky. Non regular fonts can not have derivatives. Should change this?"
		boldOblique ifNotNil: [
			bold derivativeFont: boldOblique at: 3 ].
		^bold ].
	boldOblique ifNotNil: [
		^boldOblique ].
	^nil! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 10/27/2015 17:40'!
browseRecent: charCount on: origChangesFileName 
	"Opens a changeList on the end of the specified changes log file"
	
	| changeList end |
	origChangesFileName asFileEntry readStream: [ :changesFile |
		end _ changesFile size.
		changeList _ self new
			scanFile: changesFile
			from: (0 max: end - charCount)
			to: end.
	].
	ChangeListWindow open: changeList label: 'Recent changes'! !

!ChangeList class methodsFor: 'public access' stamp: 'jmv 10/27/2015 17:38'!
browseRecentLog
	"ChangeList browseRecentLog"
	"Prompt with a menu of how far back to go to browse the current image's changes log file"
	^ self
		browseRecentLogOn: Smalltalk currentChangesName
		startingFrom: Smalltalk lastQuitLogPosition! !

!ChangeList class methodsFor: 'public access' stamp: 'jmv 10/27/2015 17:42'!
browseRecentLogOn: origChangesFileName 
	"figure out where the last snapshot or quit was, then browse the recent entries."

	| end done block positions prevBlock |
	origChangesFileName asFileEntry readStream: [ :changesFile |
		positions _ SortedCollection new.
		end _ changesFile size.
		prevBlock _ end.
		block _ end - 1024 max: 0.
		done _ false.
		[ done or: [ positions size > 0 ]]
			whileFalse: [
				changesFile position: block.
				"ignore first fragment"
				changesFile nextChunk.
				[ changesFile position < prevBlock ]
					whileTrue: [
						| pos chunk |
						pos _ changesFile position.
						chunk _ changesFile nextChunk.
						((chunk indexOfSubCollection: '----' startingAt: 1) = 1) ifTrue: [
							({ '----QUIT'. '----SNAPSHOT' } anySatisfy: [ :str |
								chunk beginsWith: str ])
									ifTrue: [ positions add: pos ]]].
				block = 0
					ifTrue: [done _ true]
					ifFalse: [
						prevBlock _ block.
						block _ block - 1024 max: 0]].
	].
	positions isEmpty
		ifTrue: [self inform: 'File ' , origChangesFileName , ' does not appear to be a changes file']
		ifFalse: [self browseRecentLogOn: origChangesFileName startingFrom: positions last]! !

!ChangeList class methodsFor: 'public access' stamp: 'jmv 10/27/2015 17:39'!
browseRecentLogOn: origChangesFileName startingFrom: initialPos 
	"Prompt with a menu of how far back to go when browsing a changes file."

	| end banners positions pos chunk i |
	origChangesFileName asFileEntry readStream: [ :changesFile |
		banners _ OrderedCollection new.
		positions _ OrderedCollection new.
		end _ changesFile size.
		pos _ initialPos.
		[pos = 0
			or: [banners size > 20]]
			whileFalse: [changesFile position: pos.
				chunk _ changesFile nextChunk.
				i _ chunk indexOfSubCollection: 'priorSource: ' startingAt: 1.
				i > 0
					ifTrue: [positions addLast: pos.
						banners
							addLast: (chunk copyFrom: 5 to: i - 2).
						pos _ Number
									readFrom: (chunk copyFrom: i + 13 to: chunk size)]
					ifFalse: [pos _ 0]].
	].
	banners size = 0 ifTrue: [^ self inform: 
'this image has never been saved
since changes were compressed'].
	pos _ (SelectionMenu labelList: banners selections: positions)
				startUpWithCaption: 'Browse as far back as...'.
	pos
		ifNil: [^ self].
	self browseRecent: end - pos on: origChangesFileName! !

!ChangeList class methodsFor: 'public access' stamp: 'jmv 10/27/2015 17:42'!
browseRecentLogOnPath: fullName 
	"figure out where the last snapshot or quit was, then browse the recent  entries."

	fullName
		ifNotNil:
			[self browseRecentLogOn: fullName ]
		ifNil:
			[Smalltalk beep]
	! !


!VersionsBrowser methodsFor: 'init & update' stamp: 'jmv 10/27/2015 17:57'!
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
		[:x | x ifNotNil: [ x name asFileEntry privateReadStream ]].
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


!ClassCommentVersionsBrowser methodsFor: 'basic function' stamp: 'jmv 10/27/2015 17:57'!
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

	sourceFilesCopy _ SourceFiles collect: [ :x | x ifNotNil: [x name asFileEntry privateReadStream]].
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


!CodePackage methodsFor: 'testing' stamp: 'jmv 10/27/2015 17:57'!
changeRecordForOverriddenMethod: aMethodReference
	| sourceFilesCopy method position |
	method := aMethodReference actualClass compiledMethodAt: aMethodReference methodSymbol.
	position := method filePosition.
	sourceFilesCopy := SourceFiles collect:
		[:x | x isNil ifTrue: [ nil ]
				ifFalse: [x name asFileEntry privateReadStream]].
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


!CompiledMethod methodsFor: 'time stamp' stamp: 'jmv 10/27/2015 17:57'!
timeStamp
	"Answer the authoring time-stamp for the given method, retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	"(CompiledMethod compiledMethodAt: #timeStamp) timeStamp"

	| file preamble stamp tokens tokenCount |
	self fileIndex = 0 ifTrue: [^ String new].  "no source pointer for this method"
	file _ SourceFiles at: self fileIndex.
	file ifNil: [^ String new].  "sources file not available"
	"file does not exist happens in secure mode"
	file _ [file name asFileEntry privateReadStream] on: FileDoesNotExistException do: [ :ex| nil ].
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


!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 10/27/2015 17:29'!
condenseChanges
	"Move all the changes onto a compacted sources file."
	"
	Smalltalk condenseChanges
	"

	| oldChanges classCount oldChangesLocalName oldChangesPathName |
	FmDirectoryEntry smalltalkImageDirectory // 'ST80.temp' forceWriteStream: [ :f |
		f timeStamp.
		'Condensing Changes File...'
			displayProgressAt: Sensor mousePoint
			from: 0 to: Smalltalk classNames size
			during: [ :bar |
				classCount _ 0.
				Smalltalk allClassesDo: [ :class | 
					bar value: (classCount _ classCount + 1).
					class moveChangesTo: f.
					class putClassCommentToCondensedChangesFile: f.
					class class moveChangesTo: f ]].
		LastQuitLogPosition _ f position ].

	CompiledMethod allInstancesDo: [ :e | 
		e isInstalled ifFalse: [ e destroySourcePointer ] ].

	oldChanges _ SourceFiles at: 2.
	oldChangesPathName _ oldChanges name.
	oldChangesLocalName _ oldChanges localName.
	oldChanges close.
	(oldChangesPathName, '.old') asFileEntry delete.
	oldChangesPathName asFileEntry rename: oldChangesLocalName, '.old'.
	FmDirectoryEntry smalltalkImageDirectory // 'ST80.temp' rename: oldChangesLocalName.
	
	SourceFiles
			at: 2 put: oldChangesPathName asFileEntry privateAppendStream.

	self inform: 'Changes file has been rewritten!!

Check that all is well, and then save/quit.
 
Otherwise, remove new changes,
replace it with the former one, and
exit without saving the image.
 '! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 10/27/2015 18:08'!
externalizeSources   
	"Write the sources and changes streams onto external files."
	"
	Smalltalk externalizeSources
	"

	| sourcesName changesName |

	sourcesName _ self defaultSourcesName.
	sourcesName asFileEntry writeStream: [ :stream |
		stream nextPutAll: SourceFiles first originalContents ].
	SourceFiles at: 1 put: sourcesName asFileEntry privateReadStream.

	changesName _ self defaultChangesName.
	changesName  asFileEntry writeStream: [ :stream |
		stream nextPutAll: SourceFiles last contents ].
	SourceFiles at: 2 put: changesName asFileEntry privateAppendStream.

	self inform: 'Sources successfully externalized'! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 10/27/2015 17:29'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sources changes msg wmsg entry |
	msg _ 'Cuis cannot locate XfileRef
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		entry _ Smalltalk defaultSourcesName asFileEntry.
		entry exists ifFalse: [
			entry _ Smalltalk alternativeSourcesName asFileEntry ].
		entry exists ifTrue: [
			sources _ entry privateReadStream ]].
	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , entry pathName) ].

	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ 
		entry _ Smalltalk defaultChangesName asFileEntry.
		entry privateAppendStream ].
	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (msg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , entry pathName)].

	((Preferences valueOfFlag: #warnIfNoChangesFile) and: [changes notNil]) ifTrue: [
		changes isReadOnly ifTrue: [
			self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , entry pathName)].

		((changes next: 200) includesSubString: String crlfString) ifTrue: [
			self inform: 'The changes file named ' , entry pathName , '
has been injured by an unpacking utility.  Lfs were changed to CrLfs.
Please set the preferences in your decompressing program to 
"do not convert text files" and unpack the system again.']].

	SourceFiles _ Array with: sources with: changes! !

!SystemDictionary methodsFor: 'startup' stamp: 'jmv 10/27/2015 17:33'!
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
					entry readStream: [ :stream |
						[ Compiler evaluate: stream contentsOfEntireFile ] on: Error do: []]].
				"Maybe we decide to clear them after startup script execution
				startUpScriptArguments _ nil" ]
		}
		otherwise: []
	]! !


!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 10/27/2015 16:37'!
printSpaceDifferenceFrom: fileName1 to: fileName2
	"For differential results, run printSpaceAnalysis twice with different fileNames,
	then run this method...
		'STspace.text1' asFileEntry writeStream: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ].
			--- do something that uses space here ---
		'STspace.text2' asFileEntry writeStream: [ :stream | SpaceTally new printSpaceAnalysis: 0 on: stream ].
		SpaceTally new printSpaceDifferenceFrom: 'STspace.text1' to: 'STspace.text2'
"
	| coll1 coll2 item |
	coll1 _ OrderedCollection new.
	FmDirectoryEntry smalltalkImageDirectory // fileName1 readStream: [ :stream |
		[stream atEnd] whileFalse: [coll1 add: stream crLfNextLine]].
	
	coll2 _ OrderedCollection new.
	FmDirectoryEntry smalltalkImageDirectory // fileName2 readStream: [ :stream |
		[stream atEnd] whileFalse: [
			item _ stream crLfNextLine.
			((coll1 includes: item) and: [(item endsWith: 'percent') not])
				ifTrue: [coll1 remove: item]
				ifFalse: [coll2 add: item]]].

	(TextModel new contents: (String streamContents: 
			[ :s | 
			s nextPutAll: fileName1; newLine.
			coll1 do: [:x | s nextPutAll: x; newLine].
			s newLine; newLine.
			s nextPutAll: fileName2; newLine.
			coll2 do: [:x | s nextPutAll: x; newLine]]))
		openLabel: 'Differential Space Analysis'.
! !


!StandardFileStream methodsFor: 'access' stamp: 'jmv 10/27/2015 17:07'!
directory
	"Return the directory containing this file."

	^ self fileEntry parent! !

!StandardFileStream methodsFor: 'access' stamp: 'jmv 10/27/2015 17:08'!
localName
	^ self fileEntry name! !


!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 10/27/2015 17:23'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection _ (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = 1 ifTrue:
		[^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue:
		[ newName _ FillInTheBlankMorph request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FmFileIOAccessor privateFileNamed: newName ].
	self halt! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 10/27/2015 17:16'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName entry |
	entry _ fullFileName asFileEntry.
	dir _ entry parent.
	localName _ entry name.
	choice _ (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir removeKey: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName _ FillInTheBlankMorph request: 'Enter a new file name' initialAnswer: fullFileName.
		^ FmFileIOAccessor privateNewFile: newName].

	self error: 'Please close this to abort file opening'! !

!StandardFileStream class methodsFor: 'error handling' stamp: 'jmv 10/27/2015 18:17'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ fullFileName asFileEntry parent.
	files _ dir fileNames.
	fileName _ fullFileName asFileEntry name.
	choices _ fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection _ (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName _ (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName _ FillInTheBlankMorph 
							request: 'Enter a new file name' 
							initialAnswer: fileName].
	newName = '' ifFalse: [^ FmFileIOAccessor privateReadOnlyFile: newName].
	^ self error: 'Could not open a file'! !

!methodRemoval: StandardFileStream class #newFileNamed:!
StandardFileStream class removeSelector: #newFileNamed:!
!methodRemoval: StandardFileStream class #oldFileNamed:!
StandardFileStream class removeSelector: #oldFileNamed:!
!methodRemoval: StandardFileStream class #oldFileOrNoneNamed:!
StandardFileStream class removeSelector: #oldFileOrNoneNamed:!
!methodRemoval: StandardFileStream class #readOnlyFileFullNamed:!
StandardFileStream class removeSelector: #readOnlyFileFullNamed:!
!methodRemoval: StandardFileStream class #readOnlyFileNamed:!
StandardFileStream class removeSelector: #readOnlyFileNamed:!
!methodRemoval: StandardFileStream #readOnlyCopy!
StandardFileStream removeSelector: #readOnlyCopy!
!methodRemoval: FileStream class #newFileNamed:!
FileStream class removeSelector: #newFileNamed:!
!methodRemoval: FileStream class #oldFileNamed:!
FileStream class removeSelector: #oldFileNamed:!
!methodRemoval: FileStream class #oldFileOrNoneNamed:!
FileStream class removeSelector: #oldFileOrNoneNamed:!
!methodRemoval: FileStream class #readOnlyFileNamed:!
FileStream class removeSelector: #readOnlyFileNamed:!
!methodRemoval: Stream #readOnlyCopy!
Stream removeSelector: #readOnlyCopy!
