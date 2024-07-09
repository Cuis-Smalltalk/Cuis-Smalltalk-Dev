'From Cuis7.1 [latest update: #6510] on 9 July 2024 at 6:47:45 pm'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 7/9/2024 18:43:12'!
explain
	"Try to shed some light on what kind of entity the current selection is. 
	The selection must be a single token or construct. Insert the answer after 
	the selection. Send private messages whose names begin with 'explain' 
	that return a string if they recognize the selection, else nil."

	| string tiVars cgVars selectors delimitors numbers sorry reply symbol provider |
	sorry := 'Sorry, I can''t explain that.  Please select a single
token, construct, or special character.'.
	(string := self selectedString) isEmpty
		ifTrue: [reply := '']
		ifFalse: [
			string := string withBlanksTrimmed.
			"Remove space, tab, cr"
			"Temps and Instance vars need only test strings that are all letters"
			(string detect: [:char | char isValidInIdentifiers not]
				ifNone: nil) ifNil: [
					tiVars := (self explainTemp: string)
						ifNil: [self explainInst: string]].
					
			provider := self codeProvider.
			(tiVars == nil and: [provider respondsTo: #explainSpecial:])
				ifTrue: [tiVars := provider explainSpecial: string].
			tiVars := tiVars
				ifNil: [ '']
				ifNotNil: [ tiVars , '\' withNewLines].
			"Context, Class, Pool, and Global vars, and Selectors need 
			only test symbols"
			(Symbol hasInterned: string ifTrue: [:s | symbol := s])
				ifTrue: [
					cgVars := (self explainCtxt: symbol) 
						ifNil: [ (self explainClass: symbol)
							ifNil: [ self explainGlobal: symbol]].
					"See if it is a Selector (sent here or not)"
					selectors := (self explainMySel: symbol)
						ifNil: [(self explainPartSel: string)
							ifNil: [ self explainAnySel: symbol]]]
				ifFalse: [selectors := self explainPartSel: string].
			cgVars := cgVars
				ifNil: [ '']
				ifNotNil: [cgVars , '\' withNewLines].
			selectors := selectors
				ifNil: [ '']
				ifNotNil: [ selectors , '\' withNewLines].
			delimitors := string size = 1
				ifTrue: ["single special characters"
					self explainChar: string]
				ifFalse: ["matched delimitors"
					self explainDelimitor: string].
			numbers := self explainNumber: string.
			numbers ifNil: [numbers := ''].
			delimitors ifNil: [delimitors := ''].
			reply := tiVars , cgVars , selectors , delimitors , numbers].
	reply isEmpty ifTrue: [reply := sorry].

	"morph showBalloon: reply.
	self runningWorld ifNotNil: [ :w | w findATranscript ].
	reply print."
	"This could be a preference."
	self afterSelectionInsertAndSelect: reply! !

