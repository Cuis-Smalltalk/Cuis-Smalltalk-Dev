'From Cuis7.1 [latest update: #6559] on 5 August 2024 at 11:30:59 am'!

!BrowserWindow class methodsFor: 'GUI building' stamp: 'jmv 8/5/2024 11:27:04'!
findClassFrom: potentialClassNames ifFound: aBlock

	self classNameRequester 
		request: 'Class name or fragment?'
		initialAnswer: '' 
		do: [ :pattern | self findClassFrom: potentialClassNames ifFound: aBlock with: pattern ]! !


!Editor methodsFor: 'accessing' stamp: 'jmv 8/5/2024 11:30:11'!
help
	"
	TextEditor new help edit
	SmalltalkEditor new help edit
	"
	| tabs |
	tabs := String tab, String tab, String tab, String tab, String tab.
	^ String streamContents: [ :strm |
		self defaultMenuSpec do: [ :menuItemNshortcutSpec |
			menuItemNshortcutSpec size > 6 ifTrue: [
				menuItemNshortcutSpec third ifNotNil: [ :shortcutCharacter |
					strm nextPutAll: ('Command', '-', shortcutCharacter asString, tabs, menuItemNshortcutSpec seventh).
				strm newLine ]]]].! !

!methodRemoval: Editor class #allShiftShortcutsSpec stamp: 'jmv 8/5/2024 11:30:26'!
Editor class removeSelector: #allShiftShortcutsSpec!
!methodRemoval: Editor class #shiftShortcutsSpec stamp: 'jmv 8/5/2024 11:30:43'!
Editor class removeSelector: #shiftShortcutsSpec!
!methodRemoval: BrowserWindow class #findClassDialogTitle stamp: 'jmv 8/5/2024 11:27:06'!
BrowserWindow class removeSelector: #findClassDialogTitle!
