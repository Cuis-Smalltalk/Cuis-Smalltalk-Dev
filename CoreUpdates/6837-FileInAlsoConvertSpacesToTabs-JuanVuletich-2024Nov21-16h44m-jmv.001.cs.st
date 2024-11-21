'From Cuis7.1 [latest update: #6835] on 21 November 2024 at 4:47:26 pm'!

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 11/21/2024 16:46:28'!
withCuisLineEndingsAndTabs
	"Assume that CR, LF, and CRLF are all valid line endings.
	Answer a new instance, replacing each occurence with a single Lf.
	Also replace repeated spaces with tabs.
	('aLine', String crlfString, 'anotherOne    pp        qq') withCuisLineEndings
	"
	^ (self withLineEndings: `String newLineString`) withSpacesAsTabs: 4! !

!CharacterSequence methodsFor: 'converting' stamp: 'jmv 11/21/2024 16:37:59'!
withSpacesAsTabs: n
	"Return a copy of the receiver, but replace n consecutive spaces by a tab character.
	{'0ab'. '1a b'. '2a  b'. '3a   b'. '4a    b'. '5a     b'. '6a      b'} collect: [ :each | each withSpacesAsTabs: 2 ]
	{'0ab'. '1a b'. '2a  b'. '3a   b'. '4a    b'. '5a     b'. '6a      b'} collect: [ :each | each withSpacesAsTabs: 3 ]
	{'0ab'. '1a b'. '2a  b'. '3a   b'. '4a    b'. '5a     b'. '6a      b'} collect: [ :each | each withSpacesAsTabs: 4 ]
	"

	| consecutiveSpaces |
	consecutiveSpaces := 0.
	^self species streamContents: [ :stream |
		self do: [ :c |
			c = Character space
				ifTrue: [
					consecutiveSpaces := consecutiveSpaces + 1 ]
				ifFalse: [
					[ consecutiveSpaces >= n ] whileTrue: [
						consecutiveSpaces := consecutiveSpaces - n.
						stream tab ].
					stream space: consecutiveSpaces.
					consecutiveSpaces := 0.
					stream nextPut: c ]]].
! !


!ClassCategoryReader methodsFor: 'fileIn/Out' stamp: 'jmv 11/21/2024 16:46:38'!
scanFrom: aStream
	"File in methods from the stream, aStream.
	Convert line endings to Cuis convention."
	| methodSource |
	[
		methodSource := aStream nextChunk withCuisLineEndingsAndTabs.
		methodSource size > 0 ] whileTrue: [
			class compile: methodSource classified: category
				withStamp: changeStamp
				notifying: nil ]! !


!TextEditor methodsFor: 'editing keys' stamp: 'jmv 11/21/2024 16:46:41'!
changeLineEndsToLf
	"Replace all CRs and CrLfs by LFs.
	Triggered by Cmd-U -- useful when getting code from FTP sites"
	"This is a user command, and generates undo"
	
	self replaceSelectionWith: self selectedString withCuisLineEndingsAndTabs.! !


!ChangeRecord methodsFor: 'accessing' stamp: 'jmv 11/21/2024 16:46:44'!
string
	"The file is usually closed. But if it happens to be open, leave it like that."
	| string mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	string := file nextChunk withCuisLineEndingsAndTabs.
	mustOpenAndClose ifTrue: [
		file close].
	^ string! !

