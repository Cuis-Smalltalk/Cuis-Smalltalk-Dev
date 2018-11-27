'From Cuis 5.0 of 7 November 2016 [latest update: #3496] on 27 November 2018 at 9:20:13 am'!

!Color class methodsFor: 'instance creation' stamp: 'jmv 11/26/2018 17:17:17'!
colorFrom: parm
	"Return an instantiated color from parm.  If parm is already a color, return it, else return the result of my performing it if it's a symbol or, if it is a list, it can either be an array of three numbers, which will be interpreted as RGB values, or a list of symbols, the first of which is sent to me and then the others of which are in turn sent to the prior result, thus allowing entries of the form #(blue darker).  Else just return the thing"

	| aColor firstParm |
	(parm is: #Color) ifTrue: [ ^ parm ].
	parm isSymbol ifTrue: [ ^ self perform: parm ].
	parm isString ifTrue: [ ^ self fromHexString: parm ].
	(parm isSequenceableCollection and: [ parm size > 0 ])
		ifTrue: [
			firstParm _ parm first.
			firstParm isNumber ifTrue: [
				^ self fromRgbTriplet: parm].
			aColor _ self colorFrom: firstParm.
			parm withIndexDo: [ :sym :ind |
				ind > 1 ifTrue: [
					aColor _ aColor perform: sym ]].
			^ aColor].
	^ parm

"
Color colorFrom: #(blue darker)
Color colorFrom: Color blue darker
Color colorFrom: #blue
Color colorFrom: #(0.0 0.0 1.0)
Color colorFrom: '#D7B360'
"! !

