'From Cuis 4.0 of 21 April 2012 [latest update: #1307] on 13 June 2012 at 6:34:55 pm'!

!Editor methodsFor: 'accessing' stamp: 'jmv 6/13/2012 18:21'!
help
	^self class help! !


!Editor class methodsFor: 'help' stamp: 'jmv 6/13/2012 18:17'!
help
	"
	Editor help
	SimpleEditor help
	CellStyleEditor help
	TextEditor help
	SmalltalkEditor help
	"
	| allSpecs |
	allSpecs _ self cmdShortcutsSpec, self basicCmdShortcutsSpec.
	^String streamContents: [ :strm |
		allSpecs do: [ :triplet | | c |
			c _ triplet first = Character space
				ifFalse: [ triplet first asString, String tab ]
				ifTrue: [ 'Space'].
			strm nextPutAll: ('Cmd-', c, String tab, String tab, triplet third).
			strm newLine ]]! !


!TextEditor methodsFor: 'menu messages' stamp: 'jmv 6/13/2012 18:19'!
openHelp
	"Show help screen"
	TextModel new contents: self help; openLabel: self name, ' Help'.! !

!TextEditor methodsFor: 'nonediting/nontyping keys' stamp: 'jmv 6/13/2012 18:18'!
help: aKeyboardEvent
	"Show a help screen"

	self openHelp.
	^ true! !


!TextEditor class methodsFor: 'misc' stamp: 'jmv 6/13/2012 18:19'!
openHelp

	self new openHelp! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Editor initialize!

