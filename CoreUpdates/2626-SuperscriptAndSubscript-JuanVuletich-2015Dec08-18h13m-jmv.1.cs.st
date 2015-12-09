'From Cuis 4.2 of 25 July 2013 [latest update: #2625] on 8 December 2015 at 7:36:36.253611 pm'!

!TextEmphasis commentStamp: '<historical>' prior: 0!
A TextEmphasis, encodes a characteristic applicable to all fonts.  The encoding is as follows:
	1	bold
	2	itallic
	4	underlined		
	8	struck out
	16 Superscript
	32 Subscript
	64 	with ST-80 Glyphs!


!StrikeFont methodsFor: 'accessing' stamp: 'jmv 12/8/2015 19:35'!
ascent
	"Answer the receiver's maximum extent of characters above the baseline."

	"superscript"
	emphasis = 16 ifTrue: [ ^ ascent * 1.9 ].
	"subscript"
	emphasis = 32 ifTrue: [ ^ ascent * 0.75 ].
	^ascent! !

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 12/6/2015 19:10'!
descent
	"Answer the receiver's maximum extent of characters below the baseline."

	| answer |
	answer _ pointSize < 9
		ifTrue: [descent-1]
		ifFalse: [descent].
	emphasis = 32 ifTrue: [ answer _ answer * 3 ].
	^ answer! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/8/2015 19:36'!
syntheticSubscript
	"Build and answer a derivative that is Subscript."

	| derivative |
	derivative _ (AbstractFont familyName: 'DejaVu' aroundPointSize: pointSize * 0.58) copy.
	derivative name: self name , 'Sub'.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.

que todos, menos estos, tengan superscript y subscript en cero. Y en estos, apropiado. y en 'aca' usarlo. y listo
"! !

!StrikeFont methodsFor: 'synthetic derivatives' stamp: 'jmv 12/8/2015 19:36'!
syntheticSuperscript
	"Build and answer a derivative that is Superscript."

	| derivative |
	derivative _ (AbstractFont familyName: 'DejaVu' aroundPointSize: pointSize * 0.58) copy.
	derivative name: self name , 'Sup'.
	^ derivative

"
StrikeFont allInstances do: [ :a | a reset ].
('Hi ', (Text string: 'there' attribute: TextEmphasis superscript), ' how ', (Text string: 'are' attribute: TextEmphasis subscript), ' you?') edit.
"! !


!TextEditor methodsFor: 'attributes' stamp: 'jmv 12/5/2015 23:24'!
changeEmphasisOrAlignment
	"This is a user command, and generates undo"

	| menuStrings aList reply code align menuList startIndex attribute |
	startIndex _ self startIndex.
	aList _ #(normal bold italic underlined struckThrough leftFlush centered rightFlush justified).
	align _ model actualContents alignmentAt: startIndex.
	code _ model actualContents emphasisAt: startIndex.
	menuList _ WriteStream on: Array new.
	menuList nextPut: (code isZero ifTrue:['<on>'] ifFalse:['<off>']), 'normal'.
	menuList nextPutAll: (#(bold italic underlined struckThrough superscript subscript withST80Glyphs) collect: [ :emph |
		(code anyMask: (TextEmphasis perform: emph) emphasisCode)
			ifTrue: [ '<on>', emph asString ]
			ifFalse: [ '<off>', emph asString ]]).
	menuList nextPutAll: (#(leftFlush centered rightFlush justified) collectWithIndex: [ :type :i |
		align = (i-1)
			ifTrue: [ '<on>', type asString ]
			ifFalse: [ '<off>', type asString ]]).
	menuStrings _ menuList contents.
	aList _ #(normal bold italic underlined struckThrough superscript subscript withST80Glyphs leftFlush centered rightFlush justified).
	reply _ (SelectionMenu labelList: menuStrings lines: #(1 8) selections: aList) startUpWithoutKeyboard.
	reply ifNotNil: [
		(#(leftFlush centered rightFlush justified) includes: reply)
			ifTrue: [
				attribute _ TextAlignment perform: reply]
			ifFalse: [
				attribute _ TextEmphasis perform: reply].
		((menuStrings at: (aList indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttribute: attribute ]
			ifFalse: [ self applyAttribute:  attribute ]].
	^ true! !

