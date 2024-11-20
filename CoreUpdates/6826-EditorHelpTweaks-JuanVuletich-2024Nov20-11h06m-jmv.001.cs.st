'From Cuis7.1 [latest update: #6825] on 20 November 2024 at 11:06:43 am'!

!Editor methodsFor: 'accessing' stamp: 'jmv 11/20/2024 09:36:43'!
help
	"
	TextEditor new help edit
	SmalltalkEditor new help edit
	"
	| commandKeyName tabs |
	commandKeyName := Smalltalk platformName = 'Mac OS' ifTrue: ['Command'] ifFalse: ['Ctrl'].
	tabs := String tab, String tab, String tab, String tab.
	^ String streamContents: [ :strm |
		self defaultMenuSpec do: [ :menuItemNshortcutSpec |
			menuItemNshortcutSpec size > 6 ifTrue: [
				menuItemNshortcutSpec third ifNotNil: [ :shortcutCharacter |
					strm nextPutAll: (commandKeyName, '-', shortcutCharacter asString, tabs, menuItemNshortcutSpec seventh).
				strm newLine ]]]].! !

