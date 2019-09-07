'From Cuis 5.0 [latest update: #3854] on 3 September 2019 at 11:13:52 am'!

!TextFontFamilyAndSize class methodsFor: 'instance creation' stamp: 'jmv 9/3/2019 10:14:04'!
pointSize: aNumber
	"Reference only baseFonts. Any emphasis should be done with TextEmphasis.
	Store only familiName and pointSize"
	^ self new familyName: FontFamily defaultFamilyName pointSize: aNumber! !


!MessageNode methodsFor: 'private' stamp: 'jmv 9/3/2019 10:01:06'!
pvtCheckForPvtSelector: encoder
	"If the code being compiled is trying to send a private message (e.g. 'pvtCheckForPvtSelector:') to anyone other than self, then complain to encoder."

	selector isInitializePvtSelector ifTrue: [
		(receiver isSelfNewMessageSend or: [receiver isSelfBasicNewMessageSend]) ifFalse: [
			encoder notify: 'Private instance initialization messages may only be sent to ''self new'' or "self basicNew" (by class instance creation methods)']].
	selector isPvtSelector ifTrue: [
		(receiver isSelfPseudoVariable or: [ receiver isSuperPseudoVariable ]) ifFalse: [
			self halt.
			encoder notify: 'Private messages may only be sent to self or super']].! !


!Utilities class methodsFor: 'default desktop' stamp: 'jmv 9/3/2019 10:58:38'!
pvtDefaultTextEditorContents
	^ ('
', 
('Cuis Smalltalk

' centered blue pointSize: FontFamily defaultPointSize * 3),
('
"Yay, Juan.  You GO, guy!! ...a great example of malleable software (and a clever mind) at work."
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('Dan Ingalls
' italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('
"I like it... It''s nice and clean and simple and pretty. Nice stuff!!"
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('Alan Kay
' italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('
"I think you have a very elegant design aesthetic."
' rightFlush pointSize: FontFamily defaultPointSize * 0.6),
('John Maloney
'italic rightFlush pointSize: FontFamily defaultPointSize * 0.6),
'

',
'Cuis is a modern, Open Source, multiplatform, Smalltalk-80 system.


',
'Cuis is:

' bold,
'    - Small
    - Clean
    - Appropriable
' blue,
'

Additionally, Cuis is:

' bold, 
'    - Open Source
    - Multiplatform
' blue,
'
 
Like other Smalltalk systems, Cuis is also:

' bold, 
'    - A complete development environment written in itself
    - A pure, dynamic Object Oriented language
' blue,
'

Cuis assumes very little on the underlying platform, and this lets it run out-of-the-box on Windows, MacOS, Linux, ChromeOS and WebBrowsers. Cuis shares the [OpenSmalltalk Virtual Machine] (http://www.opensmalltalk.org) with Squeak, Pharo and Newspeak.

What sets Cuis apart from the other members of the Squeak family is the focus on Smalltalk-80 and an active attitude towards system complexity:

Unbound complexity growth, together with development strategies focused only in the short term, are the worst long term enemies of all software systems. As systems grow older, they usually become more complex. New features are added as layers on top of whatever is below, sometimes without really understanding it, and almost always without modifying it. Complexity and size grow without control. Evolution slows down. Understanding the system becomes harder every day. Bugs are harder to fix. Codebases become huge for no clear reason. At some point, the system can''t evolve anymore and becomes "legacy code".

Complexity puts a limit to the level of understanding of the system a person might reach, and therefore limits the things that can be done with it. Dan Ingalls says all this in ["Design Principles Behind Smalltalk"] (http://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html). Even if you have already done so, please go and read it again!!

Cuis Smalltalk is our attempt at this challenge. Furthermore, we believe we are doing something else that no other Smalltalk, commercial or open source, does. We attempt to give the true Smalltalk-80 experience, and keep Smalltalk-80 not as legacy software historic significance, but as a live, evolving system. We feel we are the keepers of the Smalltalk-80 heritage, and enablers of the Dynabook experience.

As Cuis evolves, we keep on these values. Every update, be it a bug fix or a feature enhancement, is reviewed carefully to avoid adding unneded complexity to the system. Every opportunity to remove unneded complexity is followed. As we go, features are enhanced, and any reported bugs fixed. We also adopt selected enhancements from Squeak and Pharo, and share our work with the wider Smalltalk community.


' justified,
'License
' bold,
'
Cuis is distributed subject to the MIT License, as in http://www.opensource.org/licenses/mit-license.php . Any contribution submitted for incorporation into or for distribution with Cuis shall be presumed subject to the same license.

Portions of Cuis are:
Copyright (c) Xerox Corp. 1981, 1982
Copyright (c) Apple Computer, Inc. 1985-1996
Copyright (c) Contributors to Squeak and Cuis projects. 1997-2019')! !

!Utilities class methodsFor: 'default desktop' stamp: 'jmv 9/3/2019 11:05:35'!
recreateDefaultDesktop
	| editor |
	Utilities runningWorld hideTaskbar.
	TranscriptWindow allInstancesDo: [ :t | t delete ].
	SystemWindow allInstancesDo: [ :t | t delete ].
	TranscriptWindow openTranscript
		morphPosition: 5 @ 290;
		morphExtent: 990 @ 400.
	editor _ TextEditor openTextEditor
		morphPosition: 456 @ 10;
		morphExtent: 900 @ 680.
	editor setLabel: 'About Cuis'.
	editor model actualContents: self pvtDefaultTextEditorContents.
	self runningWorld showTaskbar.! !


!FontFamily class methodsFor: 'initialization' stamp: 'jmv 9/3/2019 11:07:48'!
defaultFamilyName: aString
	| family |
	self revisar. "Please remove this hack meant to install larger DejaVu StrikeFonts on demand."
	aString =  'DejaVu' ifTrue: [
		family _ AvailableFamilies at: aString.
		(family pointSizes includes: DefaultPointSize) ifFalse: [ StrikeFont install: aString]].
	DefaultFamilyName _ aString.
	Utilities recreateDefaultDesktop.! !

!FontFamily class methodsFor: 'initialization' stamp: 'jmv 9/3/2019 11:07:44'!
defaultPointSize: aNumber
	DefaultPointSize _ aNumber.
	Utilities recreateDefaultDesktop.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Utilities recreateDefaultDesktop.!

