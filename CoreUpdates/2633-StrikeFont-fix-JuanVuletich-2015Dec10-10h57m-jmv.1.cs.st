'From Cuis 4.2 of 25 July 2013 [latest update: #2632] on 10 December 2015 at 11:09:49.896803 am'!

!StrikeFont methodsFor: 'testing' stamp: 'jmv 12/10/2015 10:57'!
isSubscript
	^emphasis allMask: 32! !

!StrikeFont methodsFor: 'testing' stamp: 'jmv 12/10/2015 10:57'!
isSuperscript
	^emphasis allMask: 16! !


!StrikeFont methodsFor: 'accessing' stamp: 'jmv 12/10/2015 10:57'!
ascent
	"Answer the receiver's maximum extent of characters above the baseline."

	self isSuperscript ifTrue: [ ^ ascent * 1.9 ].
	self isSubscript ifTrue: [ ^ ascent * 0.75 ].
	^ascent! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/10/2015 11:08'!
syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ ((AbstractFont familyName: 'DejaVu' aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative name: self name , 'Sub'.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.

que todos, menos estos, tengan superscript y subscript en cero. Y en estos, apropiado. y en 'aca' usarlo. y listo
"! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/10/2015 11:08'!
syntheticSuperscript
	"Build and answer a derivative that is Superscript."

	| derivative |
	derivative _ ((AbstractFont familyName: 'DejaVu' aroundPointSize: pointSize * 0.58)
		emphasized: emphasis)
			copy.
	derivative name: self name , 'Sup'.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
StrikeFont allInstances do: [ :a | a reset ].!

