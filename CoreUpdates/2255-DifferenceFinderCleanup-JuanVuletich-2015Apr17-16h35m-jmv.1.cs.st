'From Cuis 4.2 of 25 July 2013 [latest update: #2255] on 17 April 2015 at 4:40:57.544604 pm'!
!classDefinition: #DifferenceFinder category: #'System-Code Differ'!
Object subclass: #DifferenceFinder
	instanceVariableNames: 'base case x y map tally matrix differences '
	classVariableNames: 'D EmptyLCS L U UL '
	poolDictionaries: ''
	category: 'System-Code Differ'!

!DifferenceFinder class methodsFor: 'constants' stamp: 'jmv 4/17/2015 16:39'!
d
	^1! !

!DifferenceFinder class methodsFor: 'constants' stamp: 'jmv 4/17/2015 16:39'!
l
	^3! !

!DifferenceFinder class methodsFor: 'constants' stamp: 'jmv 4/17/2015 16:39'!
u
	^2! !

!DifferenceFinder class methodsFor: 'constants' stamp: 'jmv 4/17/2015 16:39'!
ul
	^4! !


!DifferenceFinder methodsFor: 'private' stamp: 'jmv 4/17/2015 16:00'!
unfold: aPoint on: pending
	| i j mij |
	i := aPoint x.
	j := aPoint y.
	(i = 0 or: [j = 0]) ifTrue: [^self].
	mij := map i: i j: j.
	mij = self class d ifTrue: [
		pending add: i - 1 @ (j - 1).
		^self].
	mij = self class u ifTrue: [
		pending add: i - 1 @ j.
		^self].
	mij = self class l ifTrue: [
		pending add: i @ (j - 1).
		^self].
	mij = self class ul ifTrue: [
		pending add: i - 1 @ j; add: i @ (j - 1).
		^self].
	self assert: false! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 4/17/2015 16:00'!
computeLcsAt: i at: j
	| mij cij pair left up |
	mij := map i: i j: j.
	mij = self class d ifTrue: [
		cij := self lcsAt: i - 1 at: j - 1.
		pair := Array with: i with: j.
		^cij collect: [:s | s copyWith: pair]].
	mij = self class u ifTrue: [^self lcsAt: i - 1 at: j].
	mij = self class l ifTrue: [^self lcsAt: i at: j - 1].
	mij = self class ul ifTrue: [
		left := self lcsAt: i at: j - 1.
		up := self lcsAt: i - 1 at: j.
		^left copy addAll: up; yourself].
	self assert: false! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 4/17/2015 16:00'!
computeMapAt: i at: j
	| xi yj left up t |
	xi := x at: i.
	yj := y at: j.
	xi = yj ifTrue: [
		t := ((j = 1 or: [i = 1])
			ifTrue: [0]
			ifFalse: [tally i: i - 1 j: j - 1])
			+ 1.
		tally i: i j: j put: t.
		^self class d].
	left := j = 1 ifTrue: [0] ifFalse: [tally i: i j: j - 1].
	up := i = 1 ifTrue: [0] ifFalse: [tally i: i - 1 j: j].
	left < up ifTrue: [
		tally i: i j: j put: up.
		^self class u].
	tally i: i j: j put: left.
	^up < left ifTrue: [self class l] ifFalse: [self class ul]
! !

!DifferenceFinder methodsFor: 'computing' stamp: 'jmv 4/17/2015 16:35'!
lcsAt: i at: j
	| lcs |
	(i = 0 or: [j = 0]) ifTrue: [^Set with: #() "EmptyLCS"].
	lcs := matrix i: i j: j.
	lcs ifNil: [
		lcs := self computeLcsAt: i at: j.
		matrix i: i j: j put: lcs].
	^lcs
! !

!methodRemoval: DifferenceFinder class #initialize!
DifferenceFinder class removeSelector: #initialize!
DifferenceFinder initialize!

!DifferenceFinder class reorganize!
('instance creation' base:case: charactersOf:and: linesOf:and: wordsOf:and:)
('compatibility' displayPatchFrom:to:tryWords: displayPatchFrom:to:tryWords:prettyPrintedIn: wordsDisplayPatchFrom:to:)
('bibliography' references)
('constants' d l u ul)
!

!classDefinition: #DifferenceFinder category: #'System-Code Differ'!
Object subclass: #DifferenceFinder
	instanceVariableNames: 'base case x y map tally matrix differences'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Code Differ'!
