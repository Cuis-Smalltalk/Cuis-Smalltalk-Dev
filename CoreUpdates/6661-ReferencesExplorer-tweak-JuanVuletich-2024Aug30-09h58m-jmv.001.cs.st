'From Cuis7.1 [latest update: #6624] on 30 August 2024 at 9:58:26 am'!

!ClosureScanner methodsFor: 'explorer window' stamp: 'jmv 8/30/2024 09:58:16'!
openExplorerWindow
	"Assumes closure scanning already done"

	| explorer window |
	explorer := self explorerClass finder: self.
	window := self explorerWindowClass
		open: explorer
		label: self explorerWindowPrefix, self target printString, '(', self target identityHash printString,  ')'.
	window expandAll! !

