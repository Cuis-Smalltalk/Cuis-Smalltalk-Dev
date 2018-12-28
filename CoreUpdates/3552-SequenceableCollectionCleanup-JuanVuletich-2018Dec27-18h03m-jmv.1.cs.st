'From Cuis 5.0 of 7 November 2016 [latest update: #3550] on 27 December 2018 at 6:26:26 pm'!

!SequenceableCollection methodsFor: 'comparing' stamp: 'jmv 12/27/2018 18:20:09'!
hasEqualElements: otherCollection
	"Answer whether the receiver's size is the same as otherCollection's
	size, and each of the receiver's elements equal the corresponding 
	element of otherCollection.
	This should probably replace the current definition of #= ."

	| size |
	otherCollection isSequenceable ifFalse: [^ false].
	(size _ self size) = otherCollection size ifFalse: [^ false].
	1 to: size do:
		[:index |
		(self at: index) = (otherCollection at: index) ifFalse: [^ false]].
	^ true! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 12/26/2018 15:47:12'!
polynomialEval: thisX
	"Treat myself as the coefficients of a polynomial in X.  Evaluate it with thisX.  First element is the constant and last is the coefficient for the highest power.
	https://en.wikipedia.org/wiki/Horner's_method"
	"  #(1 2 3) polynomialEval: 2   "   "is 3*X^2 + 2*X + 1 with X = 2"

	| index sum |
	sum := self at: (index := self size).
	[ (index := index - 1) >= 1 ] whileTrue: [
		sum := sum * thisX + (self at: index) ].
	^sum! !


!Color class methodsFor: 'instance creation' stamp: 'jmv 12/27/2018 18:25:18'!
colorFrom: parm
	"Return an instantiated color from parm.  If parm is already a color, return it, else return the result of my performing it if it's a symbol or, if it is a list, it can either be an array of three numbers, which will be interpreted as RGB values, or a list of symbols, the first of which is sent to me and then the others of which are in turn sent to the prior result, thus allowing entries of the form #(blue darker).  Else just return the thing"

	| aColor firstParm |
	(parm is: #Color) ifTrue: [ ^ parm ].
	parm isSymbol ifTrue: [ ^ self perform: parm ].
	parm isString ifTrue: [ ^ self fromHexString: parm ].
	(parm isCollection and: [ parm isSequenceable and: [ parm size > 0 ]])
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


!ParseNode methodsFor: 'printing' stamp: 'jmv 12/27/2018 18:25:45'!
nodePrintOn: aStrm indent: nn
	| var aaStrm myLine |
	"Show just the sub nodes and the code."

	(aaStrm := aStrm) ifNil: [aaStrm := WriteStream on: (String new: 500)].
	nn timesRepeat: [aaStrm tab].
	aaStrm nextPutAll: self class name; space.
	myLine := self printString withBlanksCondensed.
	myLine := myLine copyFrom: 1 to: (myLine size min: 70).
	aaStrm nextPutAll: myLine; newLine.
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var respondsTo: #asReturnNode) ifTrue: [var nodePrintOn: aaStrm indent: nn+1]].
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var isCollection and: [var isSequenceable]) ifTrue: [
			var do: [ :aNode | 
				(aNode respondsTo: #asReturnNode) ifTrue: [
					aNode nodePrintOn: aaStrm indent: nn+1]]]].
	^ aaStrm! !

!methodRemoval: SequenceableCollection #allButFirstDo:!
SequenceableCollection removeSelector: #allButFirstDo:!
!methodRemoval: SequenceableCollection #allButLastDo:!
SequenceableCollection removeSelector: #allButLastDo:!
!methodRemoval: SequenceableCollection #at:incrementBy:!
SequenceableCollection removeSelector: #at:incrementBy:!
!methodRemoval: SequenceableCollection #errorFirstObject:!
SequenceableCollection removeSelector: #errorFirstObject:!
!methodRemoval: SequenceableCollection #forceTo:paddingStartWith:!
SequenceableCollection removeSelector: #forceTo:paddingStartWith:!
!methodRemoval: SequenceableCollection #forceTo:paddingWith:!
SequenceableCollection removeSelector: #forceTo:paddingWith:!
!methodRemoval: SequenceableCollection #integerAt:!
SequenceableCollection removeSelector: #integerAt:!
!methodRemoval: SequenceableCollection #integerAt:put:!
SequenceableCollection removeSelector: #integerAt:put:!
!methodRemoval: SequenceableCollection #isSequenceableCollection!
SequenceableCollection removeSelector: #isSequenceableCollection!
!methodRemoval: Object #isSequenceableCollection!
Object removeSelector: #isSequenceableCollection!
