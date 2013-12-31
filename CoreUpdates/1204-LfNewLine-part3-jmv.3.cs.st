'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 14 March 2012 at 12:16:58 pm'!

!AutoCompleterMorph commentStamp: 'jmv 3/13/2012 10:50' prior: 0!
 I show the possible completions in a menu like appearance. The user may choose an entry from my list and complete the word he was typing in the editor. I'm showed with the Tab key and will be deleted when with ESC key or when a successful completion occurs. The following keystrokes are supported:Ctrl-Space or Tab: Open a new morph. Tab requires at least one character in front of the cursor. When already open complete the selected entry. Esc: Close meArrow Up: Move one entry up.Arrow Down: Move one entry downReturn: (like Ctrl-Space and Tab): Complete with the selected item and close the morphany letter or digit: Narrow the completion further!

!classDefinition: #CharacterSet category: #'Kernel-Text'!
Collection subclass: #CharacterSet
	instanceVariableNames: 'map '
	classVariableNames: 'CrLf '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!String commentStamp: '<historical>' prior: 0!
                  A String is an indexed collection of Characters, compactly encoded as 8-bit bytes.

String support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:!


!TextAlignment commentStamp: 'jmv 3/13/2012 11:50' prior: 0!
    Warning: TextAlignment and ParagraphStyleReference should always be applied to whole 'paragraphs' in the text. See #isParagraphAttribute((Text string: 'This text has no tyle set', String crString),(Text string: 'This is centered', String crString attribute: TextAlignment centered),(Text string: 'This text has no tyle set', String crString)) edit!

!methodRemoval: StandardFileStream #insertLineFeeds!
StandardFileStream removeSelector: #insertLineFeeds!
!methodRemoval: WriteStream #cr!
WriteStream removeSelector: #cr!
!methodRemoval: WriteStream #crtab!
WriteStream removeSelector: #crtab!
!methodRemoval: WriteStream #crtab:!
WriteStream removeSelector: #crtab:!
!methodRemoval: WriteStream #lf!
WriteStream removeSelector: #lf!
!methodRemoval: Utilities class #convertCRtoLF:!
Utilities class removeSelector: #convertCRtoLF:!
!methodRemoval: Transcript class #cr!
Transcript class removeSelector: #cr!
!methodRemoval: Transcript class #crtab!
Transcript class removeSelector: #crtab!
!methodRemoval: SmalltalkEditor #crWithIndent:!
SmalltalkEditor removeSelector: #crWithIndent:!
!methodRemoval: SmalltalkEditor #explainScan:!
SmalltalkEditor removeSelector: #explainScan:!
!methodRemoval: TextEditor #changeLfToCr:!
TextEditor removeSelector: #changeLfToCr:!
!methodRemoval: Text #withSqueakLineEndings!
Text removeSelector: #withSqueakLineEndings!
!methodRemoval: SystemDictionary #removeAllLineFeeds!
SystemDictionary removeSelector: #removeAllLineFeeds!
!methodRemoval: String class #cr!
String class removeSelector: #cr!
!methodRemoval: String class #crlf!
String class removeSelector: #crlf!
!methodRemoval: String class #crlfcrlf!
String class removeSelector: #crlfcrlf!
!methodRemoval: String class #lf!
String class removeSelector: #lf!
!methodRemoval: String #lineCorrespondingToIndex:!
String removeSelector: #lineCorrespondingToIndex:!
!methodRemoval: String #withCRs!
String removeSelector: #withCRs!
!methodRemoval: String #withSeparatorsCompacted!
String removeSelector: #withSeparatorsCompacted!
!methodRemoval: String #withSqueakLineEndings!
String removeSelector: #withSqueakLineEndings!
!methodRemoval: StrikeFont #makeCarriageReturnsWhite!
StrikeFont removeSelector: #makeCarriageReturnsWhite!
!methodRemoval: SocketStream #cr!
SocketStream removeSelector: #cr!
!methodRemoval: SocketStream #crlf!
SocketStream removeSelector: #crlf!
!methodRemoval: SmartRefStream #checkCrLf!
SmartRefStream removeSelector: #checkCrLf!
!methodRemoval: MorphicScanner #cr!
MorphicScanner removeSelector: #cr!
!methodRemoval: ImageReadWriter #cr!
ImageReadWriter removeSelector: #cr!
!methodRemoval: ImageReadWriter #lf!
ImageReadWriter removeSelector: #lf!
!methodRemoval: GZipSurrogateStream #cr!
GZipSurrogateStream removeSelector: #cr!
!methodRemoval: Editor #cr:!
Editor removeSelector: #cr:!
!methodRemoval: Editor #crWithIndent:!
Editor removeSelector: #crWithIndent:!
!methodRemoval: Editor #crlf:!
Editor removeSelector: #crlf:!
!methodRemoval: Editor #lf:!
Editor removeSelector: #lf:!
!methodRemoval: DummyStream #cr!
DummyStream removeSelector: #cr!
!methodRemoval: CompositionScanner #cr!
CompositionScanner removeSelector: #cr!
!methodRemoval: CharacterSet class #crlf!
CharacterSet class removeSelector: #crlf!
!classDefinition: #CharacterSet category: #'Kernel-Text'!
Collection subclass: #CharacterSet
	instanceVariableNames: 'map'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Text'!
!methodRemoval: CharacterBlockScanner #cr!
CharacterBlockScanner removeSelector: #cr!
!methodRemoval: CharacterScanner #cr!
CharacterScanner removeSelector: #cr!
!methodRemoval: CharacterScanner #lf!
CharacterScanner removeSelector: #lf!
!methodRemoval: Character class #cr!
Character class removeSelector: #cr!
!methodRemoval: Character class #lf!
Character class removeSelector: #lf!
!methodRemoval: Character class #linefeed!
Character class removeSelector: #linefeed!
!methodRemoval: BitBlt class #benchDiffsFrom:to:!
BitBlt class removeSelector: #benchDiffsFrom:to:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Editor initialize.
	StrikeFont makeLfInvisible.
	StrikeFont makeCrVisible.
	Smalltalk fixSourceCodeLineEndings.
!

