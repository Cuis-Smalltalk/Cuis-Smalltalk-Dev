'From Cuis 4.2 of 25 July 2013 [latest update: #1932] on 9 January 2014 at 11:53:35.67091 pm'!

!TextEditor methodsFor: 'attributes' stamp: 'jmv 1/9/2014 23:48'!
changeEmphasisOrAlignment
	"This is a user command, and generates undo"

	| menuStrings aList reply code align menuList startIndex attribute |
	startIndex _ self startIndex.
	aList _ #(normal bold italic underlined struckThrough leftFlush centered rightFlush justified).
	align _ model actualContents alignmentAt: startIndex.
	code _ model actualContents emphasisAt: startIndex.
	menuList _ WriteStream on: Array new.
	menuList nextPut: (code isZero ifTrue:['<on>'] ifFalse:['<off>']), 'normal'.
	menuList nextPutAll: (#(bold italic underlined struckThrough) collect:[:emph|
		(code anyMask: (TextEmphasis perform: emph) emphasisCode)
			ifTrue: [ '<on>', emph asString ]
			ifFalse: [ '<off>', emph asString ]]).
	menuList nextPutAll: (#(leftFlush centered rightFlush justified) collectWithIndex: [ :type :i |
		align = (i-1)
			ifTrue: [ '<on>', type asString ]
			ifFalse: [ '<off>', type asString ]]).
	menuStrings _ menuList contents.
	aList _ #(normal bold italic underlined struckThrough leftFlush centered rightFlush justified).
	reply _ (SelectionMenu labelList: menuStrings lines: #(1 5) selections: aList) startUpWithoutKeyboard.
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

!methodRemoval: ObjectExplorerWindow #expandPathFromRoot!
ObjectExplorerWindow removeSelector: #expandPathFromRoot!
