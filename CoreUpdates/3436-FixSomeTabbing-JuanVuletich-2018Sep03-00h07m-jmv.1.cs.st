'From Cuis 5.0 of 7 November 2016 [latest update: #3430] on 3 September 2018 at 12:08:37 am'!

!LayoutSpec commentStamp: 'jmv 9/3/2018 00:07:43' prior: 0!
LayoutSpecs are the basis for the layout mechanism. Any Morph can be given a LayoutSpec, but in order to honor it, its owner must be a LayoutMorph.

A LayoutSpec specifies how a morph wants to be layed out. It can specify either a fixed width or a fraction of some available owner width. Same goes for height. If a fraction is specified, a minimum extent is also possible.


Alternatives:
	- proportionalWidth notNil, fixedWidth notNil	->		Use fraction of available space, take fixedWidth as minimum desired width
	- proportionalWidth isNil, fixedWidth isNil		->		Use current morph width
	- proportionalWidth isNil, fixedWidth notNil		->		Use fixedWidth
	- proportionalWidth notNil, fixedWidth isNil		->		NOT VALID

Same goes for proportionalHeight and fixedHeight
!


!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 9/3/2018 00:08:13'!
proportionalWidth: aNumberOrNil minimum: otherNumberOrNil
	"Alternatives:
		- proportionalWidth notNil, fixedWidth notNil 	->		Use fraction of available space, take fixedWidth as minimum desired width
		- proportionalWidth isNil, fixedWidth isNil  	->		Use current morph width
		- proportionalWidth isNil, fixedWidth notNil 	->		Use fixedWidth
		- proportionalWidth notNil, fixedWidth isNil 	->		NOT VALID"
	proportionalWidth _ aNumberOrNil.
	fixedWidth _ otherNumberOrNil! !

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 9/3/2018 00:08:20'!
setProportionalWidth: aNumberOrNil
	"Alternatives:
		- proportionalWidth notNil, fixedWidth notNil	->		Use fraction of available space, take fixedWidth as minimum desired width
		- proportionalWidth isNil, fixedWidth isNil		->		Use current morph width
		- proportionalWidth isNil, fixedWidth notNil		->		Use fixedWidth
		- proportionalWidth notNil, fixedWidth isNil		->		NOT VALID"
	proportionalWidth _ aNumberOrNil! !

