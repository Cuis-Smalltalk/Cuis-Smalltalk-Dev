'From Cuis 4.2 of 25 July 2013 [latest update: #1774] on 29 July 2013 at 10:28:00.088875 pm'!

!Fraction commentStamp: '<historical>' prior: 0!
Fraction provides methods for dealing with fractions like 1/3 as fractions (not as 0.33333...).  All public arithmetic operations answer reduced fractions (see examples).

instance variables: 'numerator denominator '

Examples: (note the parentheses required to get the right answers in Smalltalk):

(2/3) + (2/3)
(2/3) + (1/2)		 "answers shows the reduced fraction" 
(2/3) raisedToInteger: 5		 "fractions also can have exponents"
!


!Object methodsFor: 'testing' stamp: 'jmv 7/29/2013 22:21'!
is: aSymbol
	"A means for cleanly replacing isXXX like methods.
	Please use judiciously!!
	aSymbol is ussually a class name (starting with uppercase) or a protocolo conformance question (starting with lowercase), such as #hasTextSelector, #hasTextProvider, etc.
	
	A few comments:
	
		- Good for kernel tests
		- Good for tests defined in the same package as the receiver
		- Overwriting this method in a different package is a bad idea. It will surely conflict with other package. Use the traditional isXXX in such cases
		
		- In any case, asking these kinds of questions is a sign of poor design. If possible, avoid the question altogether, using, for example, double dispatching.
		
		- if a class happens to answer true for several Symbols, consider implementing it like:
			^#(symbol1 symbol2 symbol3) pointsTo: aSymbol
		"
	
	"Enable this to log improper calls to the Transcript..."
	"
	aSymbol class == Symbol ifFalse: [ thisContext sender sender print. aSymbol print ].
	"
	^false! !


!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 7/29/2013 22:25'!
openSourcesAndChanges
	"Open the changes and sources files and install them in SourceFiles. Inform the user of problems regarding write permissions or Lf/CrLf mixups."
	"Note: SourcesName and imageName are full paths; changesName is a  
	local name."
	| sourcesPathName changesPathName sources changes msg wmsg |
	msg _ 'Cuis cannot locate 
Please check that the file is named properly and is in the
same directory as this image.'.
	wmsg _ 'Cuis cannot write to XfileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'.

	"Do not open source files if internalized (i.e. notNil)"
	sources _ SourceFiles at: 1.
	sources ifNil: [
		sourcesPathName _ Smalltalk defaultSourcesName.
		sources _ StandardFileStream oldFileOrNoneNamed: sourcesPathName.
		sources ifNil: [
			sourcesPathName _ Smalltalk alternativeSourcesName.
			sources _ StandardFileStream oldFileOrNoneNamed: sourcesPathName ]].
	(sources isNil and: [ Preferences valueOfFlag: #warnIfNoSourcesFile ])
		ifTrue: [
			Smalltalk platformName = 'Mac OS' ifTrue: [
				msg _ msg , String newLineString, 'Make sure the sources file is not an Alias.'].
			self inform: (msg copyReplaceAll: 'XfileRef' with: 'the sources file named ' , sourcesPathName) ].

	changesPathName _ Smalltalk defaultChangesName.
	"Do not open source files if internalized (i.e. notNil)"
	changes _ (SourceFiles at: 2) ifNil: [ StandardFileStream oldFileNamed: changesPathName ].
	(changes isNil and: [ Preferences valueOfFlag: #warnIfNoChangesFile ])
		ifTrue: [self inform: (msg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

	((Preferences valueOfFlag: #warnIfNoChangesFile) and: [changes notNil]) ifTrue: [
		changes isReadOnly ifTrue: [
			self inform: (wmsg copyReplaceAll: 'XfileRef' with: 'the changes file named ' , changesPathName)].

		((changes next: 200) includesSubString: String crlfString) ifTrue: [
			self inform: 'The changes file named ' , changesPathName , '
has been injured by an unpacking utility.  Lfs were changed to CrLfs.
Please set the preferences in your decompressing program to 
"do not convert text files" and unpack the system again.']].

	SourceFiles _ Array with: sources with: changes! !


!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 7/29/2013 22:21'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			aCanvas image: backgroundImage at: 0@0 ]
		ifNil: [
			"draw background fill"
			(self isWorldMorph and: [ aCanvas drawsOnDisplay ] and: [ color class == TranslucentColor ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Cuis Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: 0@0 in: nil
						fillColor: color rule: Form over]
				ifFalse: [ super drawOn: aCanvas ]]! !


!SHParserST80 methodsFor: 'parse' stamp: 'jmv 7/29/2013 22:23'!
parseSymbol
	| c |
	currentToken = '#' 
		ifTrue: [
			"if token is just the #, then scan whitespace and comments
			and then process the next character.
			Allows space between the # and the start of the symbol 
			e.g. # (),  #  a, #  'sym' "
			self rangeType: #symbol.
			self scanWhitespace].
	c _ self currentChar.
	self failWhen: (c isNil or: [c isSeparator]).
	c == $( 
		ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseArray].
	c == $' ifTrue: [^self parseSymbolString].
	c == $[ ifTrue: [
			self nextChar.
			self scanPast: #arrayStart start: currentTokenSourcePosition end: currentTokenSourcePosition + 1.
			^self parseByteArray].
	((self isSelectorCharacter: c) or: [c == $-]) 
		ifTrue: [^self parseSymbolSelector].
	((self isValidStartOfIdentifiers: c) or: [c == $:]) ifTrue: [^self parseSymbolIdentifier].
	^self parseCharSymbol! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 7/29/2013 22:27'!
methodsWithInitials: targetInitials
	"Based on a do-it contributed to the Squeak mailing list by Göran Hultgen:
- Browse methods whose initials (in the time-stamp, as logged to disk) match the given initials.
- Print out the complete time-stamp table to the Transcript.
- Answer a list of (initials -> count) associations.

CAUTION: It may take several minutes for this to complete."

	"Time millisecondsToRun: [Utilities methodsWithInitials: 'bf'] 2031"

	| initials timeStamp  allSubmitters |
	initials _ ''.
	timeStamp _ ''.
	allSubmitters _ Bag new.
	Smalltalk
		browseAllSelect:
			[:cm |
				timeStamp _ Utilities timeStampForMethod: cm.
				initials _ timeStamp isEmpty
					ifTrue:
						['']
					ifFalse:
						[timeStamp substrings first].
				initials _ initials isEmpty
					ifTrue:
						['<no time stamp>']
					ifFalse:
						[initials first isDigit
							ifTrue:
								['<date>']
							ifFalse:
								[initials]].
				allSubmitters add: initials.
				(initials = targetInitials)]
		name: ('Methods with initials ', targetInitials)
		autoSelect: nil.

	allSubmitters sortedCounts do: [:elem | Transcript newLine; show: elem asString].
	^ allSubmitters
! !

