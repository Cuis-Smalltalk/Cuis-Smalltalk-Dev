'From Cuis 5.0 of 7 November 2016 [latest update: #3744] on 13 May 2019 at 11:42:23 pm'!

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 5/13/2019 23:33:44'!
syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((FontFamily familyName: self familyName aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative useShortUnderscore.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 5/13/2019 23:33:31'!
syntheticSuperscript
	"Build and answer a derivative that is Superscript."

	| derivative |
	derivative _ ((FontFamily familyName: self familyName aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative name: self name , 'Sup'.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !

