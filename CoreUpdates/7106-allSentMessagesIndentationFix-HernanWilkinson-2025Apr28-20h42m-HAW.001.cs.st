'From Cuis7.3 [latest update: #7102] on 28 April 2025 at 8:45:28 pm'!

!SystemDictionary methodsFor: 'retrieving' stamp: 'HAW 4/28/2025 20:44:22'!
allSentMessagesWithout: classesAndMessagesPair
	"Answer the set of selectors which are sent somewhere in the system, 
	computed in the absence of the supplied classes and messages."

	| sent absentClasses absentSelectors |
	sent := IdentitySet new.
	absentClasses := classesAndMessagesPair first.
	absentSelectors := classesAndMessagesPair second.
	self classNames do: [ :cName | 
		((absentClasses includes: cName)
				ifTrue: [#()]
				ifFalse: [{(self at: cName). (self at: cName) class}]) do: [ :cl |
			 (absentSelectors isEmpty
					ifTrue: [cl selectors]
					ifFalse: [cl selectors copyWithoutAll: absentSelectors]) do: [ :sel |
				 "Include all sels, but not if sent by self"
				self allSymbolsIn: (cl compiledMethodAt: sel) literals do: [ :m |
					m == sel ifFalse: [sent add: m] ]]]].
	"The following may be sent without being in any literal frame"
	1 to: self specialSelectorSize do: [ :index | 
		sent add: (self specialSelectorAt: index)].
	self presumedSentMessages do: [ :sel | sent add: sel].
	^ sent! !

