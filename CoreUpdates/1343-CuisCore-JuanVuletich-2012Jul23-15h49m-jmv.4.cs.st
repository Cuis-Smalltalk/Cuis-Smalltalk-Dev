'From Cuis 4.0 of 21 April 2012 [latest update: #1342] on 23 July 2012 at 4:27:12 pm'!

!RemoteString commentStamp: 'jmv 7/23/2012 16:10' prior: 0!
My instances provide an external file reference to a piece of text.  It may be the sourceCode of a method, or the class comments of a class.

The changes file or file-in file usually has a chunk that is just the source string of a method:

max: aNumber
	^ self > aNumber ifTrue: [self] ifFalse: [aNumber]!!
!


!ChangeListElement methodsFor: 'accessing' stamp: 'jmv 7/23/2012 16:10'!
text
	"In Cuis, all source code is plain Strings"
	^self string! !


!Categorizer methodsFor: 'fileIn/Out' stamp: 'jmv 7/23/2012 15:55'!
scanFrom: aStream
	"Reads in the organization from the next chunk on aStream.
	Categories or elements not found in the definition are not affected.
	New elements are ignored."

	self changeFromString: aStream nextChunk! !


!BasicClassOrganizer methodsFor: 'accessing' stamp: 'NS 4/7/2004 16:02'!
classComment
	classComment
		ifNil: [^ ''].
	^ classComment text ifNil: ['']! !

!BasicClassOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 3/13/2012 12:14'!
fileOutCommentOn: aFileStream moveSource: moveSource toFile: fileIndex
	"Copy the class comment to aFileStream.  If moveSource is true (as in compressChanges or compressSources, then update classComment to point to the new file."
	| fileComment |
	classComment ifNotNil: [
			aFileStream newLine.
			fileComment _ RemoteString newString: classComment text
							onFileNumber: fileIndex toFile: aFileStream.
			moveSource ifTrue: [classComment _ fileComment]]! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 7/23/2012 15:57'!
scanCategory
	"Scan anything that involves more than one chunk; method name is historical only"

	| itemPosition item item2 tokens firstToken secondToken stamp isComment anIndex def isMeta name record methodReference doItOnlyIfInBaseSystem |
	itemPosition _ file position.
	item _ file nextChunk.
	isComment _ (item includesSubString: 'commentStamp:').
	((isComment
	or: [item includesSubString: 'methodsFor:']
	or: [item includesSubString: 'classDefinition:']
	or: [item includesSubString: 'classRemoval:']
	or: [item includesSubString: 'methodRemoval:'])
	or: [item includesSubString: 'methodMoveToSomePackage:']
	or: [item includesSubString: 'classMoveToSomePackage:'])
		ifFalse: [
			"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
			^ self addItem: (ChangeRecord new file: file position: itemPosition type: #preamble)
				 text: ('preamble: ' , item contractTo: 160)].

	tokens _ Smalltalk actualScannerClass new scanTokens: item.
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
			doItOnlyIfInBaseSystem_ firstToken == #methodMoveToSomePackage:.
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
							methodReference asStringOrText ].
			^ self ].
		
		secondToken == #methodsFor: ifTrue: [
			^ self scanCategory: tokens third class: firstToken meta: false stamp: stamp].

		tokens third == #methodsFor: ifTrue: [
			^ self scanCategory: tokens fourth class: firstToken meta: true stamp: stamp]].

	secondToken == #commentStamp:
		ifTrue: [
			stamp _ tokens third.
			self addItem:
					(ChangeRecord new file: file position: file position type: #classComment
									class: firstToken category: nil meta: false stamp: stamp)
					text: 'class comment for ' , firstToken, 
						  (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp]).
			file nextChunk.
			^ self]! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 7/23/2012 15:59'!
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
						, (Smalltalk actualParserClass new parseSelector: method)
						, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 7/23/2012 15:59'!
scanFile: aFile from: startPosition to: stopPosition

	file _ aFile.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	file position: startPosition.
	'Scanning ', aFile localName, '...'
		displayProgressAt: Sensor mousePoint
		from: startPosition to: stopPosition
		during: [ :bar |
			[file position < stopPosition] whileTrue: [ | prevChar |
				bar value: file position.
				[file atEnd not and: [file peek isSeparator]]
					whileTrue: [prevChar _ file next].
				(file peekFor: $!!)
					ifTrue: [
						(prevChar notNil and: [ prevChar isLineSeparator ])
							ifTrue: [self scanCategory]]
					ifFalse: [
						| itemPosition item |
						itemPosition _ file position.
						item _ file nextChunk.
						item size > 0 ifTrue: [
							self
								addItem: (ChangeRecord new file: file position: itemPosition type: #doIt)
								text: 'do it: ' , (item contractTo: 160)]]]].
	self clearSelections! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 7/23/2012 16:17'!
fileOutOn: aFileStream
	"File the receiver out on the given file stream"

	| aString |
	type == #method
		ifTrue: [
			aFileStream newLine; nextPut: $!!.
			aString _  class asString
							, (meta ifTrue: [' class methodsFor: ']
									ifFalse: [' methodsFor: '])
							, category asString printString.
			stamp ifNotNil: [
				aString _ aString, ' stamp: ''', stamp, ''''].
			aFileStream nextChunkPut: aString.
			aFileStream newLine ].

	type == #preamble ifTrue: [ aFileStream nextPut: $!! ].

	type == #classComment
		ifTrue: [
			aFileStream nextPut: $!!.
			aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.
			aFileStream newLine ].
		
	type == #classDefinition ifTrue: [
		aFileStream nextPut: $!!.
		aFileStream nextChunkPut: 
			'classDefinition: ', 
			(self isMetaClassChange ifTrue: [self methodClassName, ' class'] ifFalse: [self methodClassName]) printString,
			' category: ', self category printString.
		aFileStream newLine ].

	aFileStream nextChunkPut: self string.
	
	type == #method ifTrue: [ aFileStream nextChunkPut: ' '; newLine ].
	type == #classComment ifTrue: [ aFileStream newLine ].
	aFileStream newLine! !

!ChangeRecord methodsFor: 'access' stamp: 'jmv 7/23/2012 16:18'!
originalChangeSetForSelector: methodSelector
	"Returns the original changeset which contained this method version.  If it is contained in the .sources file, return #sources.  If it is in neither (e.g. its changeset was deleted), return nil.  (The selector is passed in purely as an optimization.)"

	| likelyChangeSets originalChangeSet |
	(file localName findTokens: '.') last = 'sources'
		ifTrue: [^ #sources].
	likelyChangeSets _ ChangeSorter allChangeSets select: [ :cs |
		(cs atSelector: methodSelector class: self methodClass) ~~ #none].
	originalChangeSet _ likelyChangeSets
		detect: [ :cs | cs containsMethodAtPosition: position ]
		ifNone: nil.
	^ originalChangeSet  "(still need to check for sources file)"! !

!ChangeRecord methodsFor: 'access' stamp: 'jmv 7/23/2012 16:26'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| string mustOpenAndClose |
	mustOpenAndClose _ file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	string _ file nextChunk.
	mustOpenAndClose ifTrue: [
		file close].
	^ string! !

!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 7/23/2012 16:22'!
file: f position: p type: t
	file _ f.
	position _ p.
	type _ t.
"
file closed ifFalse: [
	'' print.
	file print.
	self print.
	thisContext printStack: 10 ]
"! !


!ClassCategoryReader methodsFor: 'fileIn/Out' stamp: 'jmv 7/23/2012 16:01'!
scanFrom: aStream 
	"File in methods from the stream, aStream."
	| methodText |
	[
		methodText _ aStream nextChunk.
		methodText size > 0] whileTrue: [
		class compile: methodText classified: category
			withStamp: changeStamp
			notifying: nil ]! !


!ClassCommentReader methodsFor: 'as yet unclassified' stamp: 'jmv 7/23/2012 16:01'!
scanFrom: aStream 
	"File in the class comment from aStream.  Not string-i-fied, just a text, exactly as it is in the browser.  Move to changes file."

	class theNonMetaClass classComment: aStream nextChunk stamp: changeStamp
		"Writes it on the disk and saves a RemoteString ref"! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 7/23/2012 15:55'!
printMethodChunk: selector withPreamble: doPreamble on: outStream moveSource: moveSource toFile: fileIndex
	"Copy the source code for the method associated with selector onto the fileStream.  If moveSource true, then also set the source code pointer of the method."
	| preamble method oldPos newPos sourceFile endPos |
	doPreamble 
		ifTrue: [preamble _ self name , ' methodsFor: ' ,
					(self organization categoryOfElement: selector) asString printString]
		ifFalse: [preamble _ ''].
	method _ self methodDict at: selector ifAbsent: [
		outStream nextPutAll: selector; newLine.
		outStream tab; nextPutAll: '** ERROR!!  THIS SCRIPT IS MISSING ** '; newLine; newLine.
		outStream nextPutAll: '  '.
		^ outStream].

	((method fileIndex = 0
		or: [(SourceFiles at: method fileIndex) == nil])
		or: [(oldPos _ method filePosition) = 0])
	ifTrue: [
		"The source code is not accessible.  We must decompile..."
		preamble size > 0 ifTrue: [ outStream newLine; nextPut: $!!; nextChunkPut: preamble; newLine].
		outStream nextChunkPut: method decompileString]
	ifFalse: [
		sourceFile _ SourceFiles at: method fileIndex.
		preamble size > 0
			ifTrue:    "Copy the preamble"
				[outStream copyPreamble: preamble from: sourceFile at: oldPos]
			ifFalse:
				[sourceFile position: oldPos].
		"Copy the method chunk"
		fileIndex = 0 ifFalse: [
			outStream padTo: SourceFiles pointerScaleForWriting put: $  ].
		newPos _ outStream position.
		outStream copyMethodChunkFrom: sourceFile.
		moveSource ifTrue: [    "Set the new method source pointer"
			endPos _ outStream position.
			method checkOKToAdd: endPos - newPos at: newPos in: method fileIndex.
			method setSourcePosition: newPos inFile: fileIndex]].
	preamble size > 0 ifTrue: [ outStream nextChunkPut: ' ' ].
	^ outStream newLine! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 7/23/2012 16:02'!
copyMethodChunkFrom: aStream
	"Copy the next chunk from aStream (must be different from the receiver)."

	self nextChunkPut: aStream nextChunk! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 7/23/2012 16:00'!
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
										ifFalse: [(Smalltalk actualCompilerClass evaluate: chunk logged: false) scanFrom: self]]
								ifFalse: [
									chunk := self nextChunk.
									self checkForPreamble: chunk.
									[ Smalltalk actualCompilerClass evaluate: chunk logged: true ]
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


!RemoteString methodsFor: 'accessing' stamp: 'jmv 7/23/2012 16:10'!
text 
	"In Cuis, all source code is plain Strings"
	^self string! !

!methodRemoval: PositionableStream #nextChunkText!
PositionableStream removeSelector: #nextChunkText!
!methodRemoval: MethodDeletionChangeRecord #text!
MethodDeletionChangeRecord removeSelector: #text!
!methodRemoval: ClassDeletionChangeRecord #text!
ClassDeletionChangeRecord removeSelector: #text!
!methodRemoval: ChangeRecord #class:category:method:sourceFiles:!
ChangeRecord removeSelector: #class:category:method:sourceFiles:!
!methodRemoval: ChangeRecord #headerFor:!
ChangeRecord removeSelector: #headerFor:!
!methodRemoval: ChangeRecord #readStamp!
ChangeRecord removeSelector: #readStamp!
!methodRemoval: ChangeRecord #text!
ChangeRecord removeSelector: #text!

!ChangeListElement reorganize!
('testing' isClassDeletion)
('accessing' text)
!

