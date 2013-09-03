'From Cuis 4.2 of 25 July 2013 [latest update: #1818] on 3 September 2013 at 4:19:06.172173 pm'!

!FormCanvas class methodsFor: 'class initialization' stamp: 'jmv 9/3/2013 16:17'!
clearFormsCache
	"Keep those that are not synthetized
	FormCanvas clearFormsCache
	"
	| new |
	new _ Dictionary new.
	#(#up #down #left #right ) do: [ :symbol |
		new
			at: {symbol. #gradient}
			put: (FormCanvas arrowWithGradientOfDirection: symbol) ].
	CachedForms _ new! !

!methodRemoval: CursorWithAlpha class #resetConstants!
CursorWithAlpha class removeSelector: #resetConstants!
!classRemoval: #GUIFormsPack!
Smalltalk removeClassNamed: #GUIFormsPack!
